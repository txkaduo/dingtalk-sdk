module DingTalk.Yesod where

-- {{{1 imports
import           ClassyPrelude.Yesod hiding (requestHeaders)
import           Yesod.Core.Types (HandlerContents(HCError))
import           Data.List ((!!))
import           Network.Wai         (requestHeaders)
import           System.Random (randomRIO)

import           DingTalk.Types
import           DingTalk.Misc (httpUserAgentDingTalkVer)
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

-- vim: set foldmethod=marker:
