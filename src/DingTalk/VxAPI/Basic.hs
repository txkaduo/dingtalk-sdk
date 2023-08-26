module DingTalk.VxAPI.Basic where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Concurrent (threadDelay)
import           Control.Monad.Except hiding (mapM_)
import           Control.Monad.Logger
import           Data.Aeson           as A hiding (Options)
-- import qualified Data.Aeson.Extra     as AE
import           Data.Aeson.TH                 (deriveJSON)
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Network.Wreq hiding (Proxy)
import           Network.Wreq.Types   (Postable)
import qualified Network.Wreq.Session as WS
import           Text.Show.Unicode (ushow)
-- import           Data.Proxy

import DingTalk.Types
import DingTalk.Helpers
import DingTalk.VxAPI.ErrorCode
-- }}}1


-- | 新版服务器接口地址，前面有一段是版本号(不含/)
apiVxUrlBase :: (IsString s, Semigroup s) => s -> s
apiVxUrlBase ver = "https://api.dingtalk.com/" <> ver


-- | 新版接口错误报文内容
-- 错误代码变成一个字符串
data ApiVxError = ApiVxError
  { apiVxErrorCode    :: Text
  , apiVxErrorMessage :: Text
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 10 }) ''ApiVxError)

instance Show ApiVxError where
  show (ApiVxError c m) =
    "ApiVxError { "
      <> intercalate ", " [ "code=" <> unpack c, "message=" <> ushow m ]
      <> " }"


type ApiVxRpcWithAtk m a = ReaderT AccessToken m (Either ApiVxError a)

type ApiVxRpcWithAtkExcept m = ExceptT ApiVxError (ReaderT AccessToken m)

type ApiVxRpcWithAtkSource m a =
#if MIN_VERSION_conduit(1, 3, 0)
  ConduitT
#else
  ConduitM
#endif
   () a (ExceptT ApiVxError (ReaderT AccessToken m)) ()


data ApiVxErrorOrPayload a = ApiVxErrorOrPayload { unApiVxErrorOrPayload :: Either ApiVxError a }

instance FromJSON a => FromJSON (ApiVxErrorOrPayload a) where
-- {{{1
  parseJSON v = fmap ApiVxErrorOrPayload $ fmap Left parse_as_error <|> fmap Right parse_as_x
    where
      parse_as_error = do
        oe <- parseJSON v
        guard $ apiVxErrorCode oe /= ""
        return oe

      parse_as_x = parseJSON v
-- }}}1


-- | 目测多数新版的接口回复报文都是类似的结构
-- 'success' 字段有些接口有，有些没有，但似乎都没什么实际意义
data AviCallResponse a = AviCallResponse
  { aviCallResponseResult  :: a
  , aviCallResponseSuccess :: Maybe Bool
  }


$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 15 }) ''AviCallResponse)


apiVxToPayload :: (MonadLogger m, MonadIO m, FromJSON a)
               => String
               -> Options
               -> Value
               -> m (Either ApiVxError a)
-- {{{1
apiVxToPayload url_path opts v = do
  case fromJSON'Message v of
    Left err -> do
      $logErrorS logSourceName $ "Could not parse response body to payload: " <> err
      liftIO $ throwIO $ DatagramError (unpack err)

    Right (ApiVxErrorOrPayload err_or_res) -> do
      case err_or_res of
        Right _ -> do
          $logDebugS logSourceName $ "API '" <> fromString url_path <> "' succeeded."
              <> " Options=" <> tshow opts

        Left err -> do
          $logErrorS logSourceName $
            "API '" <> fromString url_path <> "' got error: " <> tshow err
              <> ". Options=" <> tshow opts

      return err_or_res
-- }}}1


apiVxConvertResp :: (FromJSON a, MonadIO m, MonadLogger m)
                 => String
                 -> Options
                 -> Response LB.ByteString
                 -> m (Either ApiVxError a)
-- {{{1
apiVxConvertResp url_path opts r = do
  let response_txt = toStrict $ decodeUtf8 (r ^. responseBody)
  $logDebugS logSourceName $ "api '" <> fromString url_path <> "' response:\n" <> response_txt

  case eitherDecode (r ^. responseBody) of
    Left err -> do
      $logErrorS logSourceName $
        "Failed to decode response as JSON: " <> fromString err
        <> ", response was:\n" <> response_txt
      liftIO $ throwIO $ JSONError err

    Right resp_json -> do
      apiVxToPayload url_path opts resp_json
-- }}}1


apiVxEcCanRetryShortly :: Text -> Bool
apiVxEcCanRetryShortly ec =
  ec `elem`
    [ apiVxEcForbiddenQpsLimitForApi
    , apiVxEcForbiddenQpsLimitForAppkeyAndApi
    ]


-- | 新版服务器接口调用: 重试
apiVxAutoRetryCall :: (MonadLogger m, MonadIO m, FromJSON a)
                   => String
                   -> Options
                   -> IO (Response LB.ByteString)
                   -> m (Either ApiVxError a)
apiVxAutoRetryCall url_path opts io_call = loop 0
  where
    max_retry_cnt = 2 :: Int

    loop cnt = do
      err_or_res <- liftIO io_call >>= apiVxConvertResp url_path opts
      case err_or_res of
        Left err | cnt < max_retry_cnt && apiVxEcCanRetryShortly (apiVxErrorCode err) -> do
          liftIO $ threadDelay (500 * 1000)
          $logWarnS logSourceName $ "Retrying call #" <> tshow (cnt + 1) <> ": " <> fromString url_path
          loop (cnt + 1)

        _ -> pure err_or_res


-- | 新版服务器接口调用
-- 把 access_token 会放在 header 里
-- 返回时，non-200 http status 也要解释
apiVxCall :: (HttpCallMonad env m, FromJSON a, Postable p)
          => String
          -> String
          -> Options
          -> Maybe p  -- ^ Nothing: GET method; Just: POST method
          -> ReaderT AccessToken m (Either ApiVxError a)
apiVxCall ver url_path opts0 m_payload = do
  access_token <- ask

  let opts = opts0
                & header "x-acs-dingtalk-access-token" .~ [ encodeUtf8 (unAccessToken access_token) ]
                & header "Content-Type" .~ [ "application/json" ]
                & checkResponse .~ (Just $ const $ const $ pure ())

  HttpApiRunEnv t sess <- lift ask
  throttleRemoteCall t $ do
    apiVxAutoRetryCall url_path opts $
      case m_payload of
        Nothing -> WS.getWith opts sess url
        Just payload -> WS.postWith opts sess url payload
  where
    url = apiVxUrlBase ver <> url_path


apiVxGetCall :: (HttpCallMonad env m, FromJSON a)
             => String
             -> String
             -> ParamKvList
             -> ReaderT AccessToken m (Either ApiVxError a)
apiVxGetCall ver url_path kv_list = apiVxCall ver url_path opts (Nothing :: Maybe ByteString)
  where opts = defaults & applyParamKvListInQs kv_list


-- | 若响应报文使用的是 AviCallResponse 通用格式: 真正的 payload 在 result 字段里
apiVxGetCallInResult :: (HttpCallMonad env m, FromJSON a)
                     => String
                     -> String
                     -> ParamKvList
                     -> ReaderT AccessToken m (Either ApiVxError a)
apiVxGetCallInResult ver url_path kv_list =
  fmap aviCallResponseResult <$> apiVxGetCall ver url_path kv_list


apiVxPostCall :: (HttpCallMonad env m, FromJSON a, Postable p)
              => String
              -> String
              -> ParamKvList
              -> p
              -> ReaderT AccessToken m (Either ApiVxError a)
apiVxPostCall ver url_path kv_list payload = apiVxCall ver url_path opts (Just payload)
  where opts = defaults & applyParamKvListInQs kv_list


-- | 若响应报文使用的是 AviCallResponse 通用格式: 真正的 payload 在 result 字段里
apiVxPostCallInResult :: (HttpCallMonad env m, FromJSON a, Postable p)
                      => String
                      -> String
                      -> ParamKvList
                      -> p
                      -> ReaderT AccessToken m (Either ApiVxError a)
apiVxPostCallInResult ver url_path kv_list payload =
  fmap aviCallResponseResult <$> apiVxPostCall ver url_path kv_list payload


data VxAccessTokenResp = VxAccessTokenResp
  { vxAccessTokenRespAccessToken :: AccessToken
  , vxAccessTokenRespExpireIn    :: Int
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 17 }) ''VxAccessTokenResp)


apiVxGetAccessToken' :: HttpCallMonad env m
                     => AppKey
                     -> AppSecret
                     -> m (Either ApiVxError VxAccessTokenResp)
apiVxGetAccessToken' app_key app_secret = do
  HttpApiRunEnv t sess <- ask
  throttleRemoteCall t $
    apiVxAutoRetryCall url_path opts (WS.postWith opts sess url payload)
  where
    opts = defaults & header "Content-Type" .~ [ "application/json" ]
    url_path = "/oauth2/accessToken"
    url = apiVxUrlBase "v1.0" <> url_path

    payload = object
                [ "appKey" .= app_key
                , "appSecret" .= app_secret
                ]


apiVxGetAccessToken :: HttpCallMonad env m
                    => AppKey
                    -> AppSecret
                    -> m (Either ApiVxError AccessToken)
apiVxGetAccessToken app_key app_secret =
  fmap vxAccessTokenRespAccessToken <$> apiVxGetAccessToken' app_key app_secret


-- | 把分页获取的数据转换成 Conduit
apiVxSourceByStep
  :: HttpCallMonad env m
  => (r -> [a])  -- ^ real data
  -> (r -> Maybe t) -- ^ get next token
  -> Float  -- ^ seconds. delay between iterations
  -> (Maybe t -> ApiVxRpcWithAtk m r)
  -> ApiVxRpcWithAtkSource m a
apiVxSourceByStep get_list get_next_token delay_sec get_once = loop Nothing
  where delay_us = round $ delay_sec * 1000 * 1000
        delay = liftIO $ threadDelay delay_us

        loop m_next_token = do
          resp <- lift $ ExceptT $ get_once m_next_token
          mapM_ yield (get_list resp)
          mapM_ (\ x -> delay >> loop (Just x)) (get_next_token resp)


apiVxNotFoundToMaybe :: Either ApiVxError a -> Either ApiVxError (Maybe a)
apiVxNotFoundToMaybe (Right x)  = Right (Just x)
apiVxNotFoundToMaybe (Left err) = if apiVxErrorCode err == apiVxEcNotFound
                                    then Right Nothing
                                    else Left err

apiVxNotFoundToMaybeExcept :: Monad m => ExceptT ApiVxError m a -> ExceptT ApiVxError m (Maybe a)
apiVxNotFoundToMaybeExcept =
  flip catchError ( \ err ->
    if apiVxErrorCode err == apiVxEcNotFound
       then pure Nothing
       else throwError err
    ) . fmap Just

-- vim: set foldmethod=marker:
