module DingTalk.Yesod where

-- {{{1 imports
import           ClassyPrelude.Yesod hiding (requestHeaders)
import           Control.Monad.Trans.Except
import           Yesod.Core.Types (HandlerContents(HCError))
import           Data.List ((!!))
import           Network.Wai         (requestHeaders)
import           System.Random (randomRIO)

import           DingTalk.Types
import           DingTalk.OAPI.Basic
import           DingTalk.OAPI.Callback
import           DingTalk.OAPI.Contacts
import           DingTalk.OAPI.Crypto
import           DingTalk.OAPI.SNS
import           DingTalk.Misc
-- }}}1


-- | 从 User-Agent 找微信版本
handlerGetDingTalkVer :: MonadHandler m => m (Maybe Text)
-- {{{1
handlerGetDingTalkVer = do
    req <- waiRequest
    let headers = requestHeaders req
    return $ join $ fmap (httpUserAgentDingTalkVer . decodeUtf8) $ lookup hUserAgent headers
-- }}}1


sessionKeyDingTalkAuthState :: SnsAppId -> Text
sessionKeyDingTalkAuthState app_id = "dt-oauth-st|" <> unSnsAppId app_id


-- | 生成随机字串作为 oauth 的state参数之用
-- 这是按官方文档的思路，用于防 csrf
-- 所生成的随机字串会放在 sessionKeyWxppOAuthState 会话变量里
handlerMakeRandomState :: (MonadHandler m)
                       => SnsAppId
                       -> m Text
-- {{{1
handlerMakeRandomState app_id = do
  m_oauth_random_st <- lookupSession skey
  case m_oauth_random_st of
      Just x | not (null x) -> do
          return x
      _   -> do
          random_state <- liftIO $ fmap pack $ replicateM 32 pick_random_char
          setSession skey random_state
          return random_state
  where
    skey = sessionKeyDingTalkAuthState app_id
    chars = ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z']
    chars_num = length chars

    pick_random_char = do
      idx <- randomRIO (0, chars_num-1)
      return $ chars !! idx
-- }}}1


handlerCheckSnsReturnCodeState :: (MonadHandler m, MonadLogger m)
                               => SnsAppId
                               -> m (Maybe SnsTmpAuthCode)
-- {{{1
handlerCheckSnsReturnCodeState app_id = do
  m_code <- lookupGetParam "code"
  oauth_state <- liftM (fromMaybe "") $ lookupGetParam "state"
  m_expected_state <- lookupSession (sessionKeyDingTalkAuthState app_id)
  unless (m_expected_state == Just oauth_state) $ do
      $logErrorS logSourceName $
          "OAuth state check failed, got: " <> oauth_state
      throwM $ HCError NotAuthenticated

  return $ fmap SnsTmpAuthCode m_code
-- }}}1


sessionKeyUserId :: CorpId -> Text
sessionKeyUserId corp_id = "dt-user-id|" <> unCorpId corp_id

handlerSetSessionUserId :: MonadHandler m
                        => CorpId
                        -> UserId
                        -> m ()
handlerSetSessionUserId corp_id user_id =
  setSession (sessionKeyUserId corp_id) (unUserId user_id)


handlerGetSessionUserId :: MonadHandler m
                        => CorpId
                        -> m (Maybe UserId)
handlerGetSessionUserId corp_id =
  fmap (fmap UserId) $ lookupSession (sessionKeyUserId corp_id)


-- | 根据是否在钉钉打开页面，选择适当的重定向入口，完成使用钉钉登录
handlerDingTalkLoginUrl :: ( MonadHandler m
                           , HasDingTalkLoginAppId (HandlerSite m)
                           )
                        => Text
                        -> m Text
-- {{{1
handlerDingTalkLoginUrl return_url = do
  foundation <- getYesod
  let login_app_id = getDingTalkLoginAppId foundation

  state <- handlerMakeRandomState login_app_id

  fmap isJust handlerGetDingTalkVer
          >>= return
              . bool
                  (oapiSnsQrCodeLoginRedirectUrl login_app_id (Just state) return_url)
                  (oapiSnsDingTalkLoginRedirectUrl login_app_id (Just state) return_url)

-- }}}1


-- | 使用钉钉登录，并返回UserId到原处理逻辑．实际上可能发生多次重定向
handlerDingTalkLoginComeBack :: (MonadHandler m
                                , Yesod (HandlerSite m)
                                , HasDingTalkLoginAppId (HandlerSite m)
                                , HasDingTalkCorpId (HandlerSite m)
                                , DingTalkAccessTokenRun m (HandlerSite m)
                                , DingTalkSnsAccessTokenRun m (HandlerSite m)
                                , ToTypedContent c
                                )
                             => (WidgetT (HandlerSite m) IO () -> m c)
                             -- ^ usually it is 'defaultLayout'
                             -> m UserId
-- {{{1
handlerDingTalkLoginComeBack show_widget = do
  foundation <- getYesod
  let corp_id = getDingTalkCorpId foundation
      login_app_id = getDingTalkLoginAppId foundation

  m_uid <- handlerGetSessionUserId corp_id
  case m_uid of
    Just x -> return x
    Nothing -> do
      m_code <- handlerCheckSnsReturnCodeState login_app_id
      case m_code of
        Just code
          | validateSnsTmpAuthCode code -> do
              err_or <- runExceptT $ do
                union_id <- ExceptT $ runWithDingTalkSnsAccessToken foundation $ runExceptT $ do
                  fmap snsPersistentAuthCodeRespUnionId $ ExceptT $ oapiSnsGetPersistentAuthCode code

                ExceptT $ runWithDingTalkAccessToken foundation $ oapiGetUserIdByUnionId union_id

              case err_or of
                Right x -> return x
                Left err -> do
                  $logErrorS logSourceName $ "DingTalk api error: " <> tshow err
                  fail $ "钉钉接口错误，请稍后重试"

          | otherwise -> do
              rdr_url <- get_rdr_url
              setMessage $ "钉钉登录失败"
              (sendResponse =<<) $ show_widget $ do
                setTitle "使用钉钉登录"
                [whamlet|
                  <p>
                    <a href="#{rdr_url}">点击这里重试
                |]

        Nothing -> do
          get_rdr_url >>= redirect

  where
    get_rdr_url = do
      params0 <- reqGetParams <$> getRequest
      let params = deleteMap "code" $ deleteMap "state" params0
      render_url <- getUrlRenderParams
      current_route <- getCurrentRoute >>= maybe (error "no current route") return
      let return_url = render_url current_route params
      handlerDingTalkLoginUrl return_url
-- }}}1


handleDingTalkCallback :: (MonadHandler m, MonadLogger m)
                       => DingTalkAesEnv
                       -> CallbackToken
                       -> [ (CallbackEventInput -> m ()) ]
                       -> m Value
-- {{{1
handleDingTalkCallback aes_env token cb_handlers = do
  body_jv <- requireJsonBody
  case decryptCallbackPostBody aes_env body_jv of
    Left err -> do
      $logErrorS logSourceName $ "Failed to parse callback post body: " <> fromString err
      invalidArgs ["POST Body"]

    Right cb_input -> do
      let event_tag = cbEventInputTag cb_input
      unless (isKnownCallbackTag event_tag) $ do
        $logErrorS logSourceName $ "Unknown callback event type: " <> event_tag

      sequence_ $ map ($ cb_input) cb_handlers
      let corp_or_suite = rawTextToCorpIdOrSuiteKey $ cbEventInputCorpOrSuiteKey cb_input
      mkCallbackRespose aes_env token corp_or_suite
-- }}}1


-- vim: set foldmethod=marker:
