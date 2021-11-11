module DingTalk.OAPI.Basic
  ( OapiError(..), OapiErrorOrPayload(..), oapiNotFoundToMaybe
  , OapiRpcWithAtk, OapiRpcWithAtkExcept, OapiRpcWithAtkSource
  , catchOapiError, ignoreOapiError
  , oapiGetAccessToken, oapiAccessTokenTTL
  , UserInfoByCodeResp(..), oapiGetUserInfoByAuthCode
  , AuthOrgEntiies(..), AuthScopes(..), oapiGetAccessTokenScopes
  , oapiPostCall, oapiGetCall, oapiGetCallWithAtk, oapiPostCallWithAtk
  , oapiGetCallWithAtkLike, oapiPostCallWithAtkLike
  , oapiUrlBase
  , DingTalkHttpApiRun(..), DingTalkAccessTokenRun(..)
  , module DingTalk.Types
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Concurrent (threadDelay)
import           Control.Monad.Logger
import           Control.Monad.Except hiding (mapM_)
import           Data.Aeson           as A hiding (Options)
import qualified Data.Aeson.Extra     as AE
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Data.Proxy
import           Network.Wreq hiding (Proxy)
import           Network.Wreq.Types   (Postable)
import qualified Network.Wreq.Session as WS
import           Text.Show.Unicode (ushow)

import DingTalk.Types
import DingTalk.Helpers
import DingTalk.OAPI.ErrorCode
-- }}}1


data OapiError = OapiError
  { oapiErrorCode :: Int
  , oapiErrorMsg :: Text
  }

-- {{{1 instances
instance Show OapiError where
  show (OapiError c m) =
    "OapiError { "
      <> intercalate ", " [ "errcode=" <> show c, "errmsg=" <> ushow m ]
      <> " }"

instance FromJSON OapiError where
  parseJSON = withObject "OapiError" $ \ o ->
    OapiError <$> o .: "errcode"
              <*> o .: "errmsg"
-- }}}1


type OapiRpcWithAtk m a = ReaderT AccessToken m (Either OapiError a)

type OapiRpcWithAtkExcept m = ExceptT OapiError (ReaderT AccessToken m)

type OapiRpcWithAtkSource m a =
#if MIN_VERSION_conduit(1, 3, 0)
  ConduitT
#else
  ConduitM
#endif
   () a (ExceptT OapiError (ReaderT AccessToken m)) ()


data OapiErrorOrPayload a = OapiErrorOrPayload { unOapiErrorOrPayload :: Either OapiError a }

instance FromJSON a => FromJSON (OapiErrorOrPayload a) where
-- {{{1
  parseJSON v = fmap OapiErrorOrPayload $ fmap Left parse_as_error <|> fmap Right parse_as_x
    where
      parse_as_error = do
        oe <- parseJSON v
        guard $ oapiErrorCode oe /= 0
        return oe

      parse_as_x = parseJSON v
-- }}}1


oapiUrlBase :: IsString s => s
oapiUrlBase = "https://oapi.dingtalk.com"


oapiToPayload :: (MonadLogger m, MonadIO m, FromJSON a)
              => String
              -> Options
              -> Value
              -> m (Either OapiError a)
-- {{{1
oapiToPayload url_path opts v = do
  case fromJSON'Message v of
    Left err -> do
      $logErrorS logSourceName $ "Could not parse response body to payload: " <> err
      liftIO $ throwIO $ DatagramError (unpack err)

    Right (OapiErrorOrPayload err_or_res) -> do
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


oapiNotFoundToMaybe :: Either OapiError a
                    -> Either OapiError (Maybe a)
oapiNotFoundToMaybe (Right x) = Right (Just x)
oapiNotFoundToMaybe (Left err) = if oapiErrorCode err == oapiEcNotFound
                                    then Right Nothing
                                    else Left err


catchOapiError :: MonadError OapiError m
               => Int
               -> m a
               -> m a
               -> m a
-- {{{1
catchOapiError err_code f h =
  f `catchError`
      ( \ err -> do
        if oapiErrorCode err == err_code
           then h
           else throwError err
      )
-- }}}1


ignoreOapiError :: MonadError OapiError m => Int -> m () -> m ()
ignoreOapiError ec f = catchOapiError ec f (return ())


-- | 文档说 access token 有效期固定为 7200 秒，且每次有效期内重复获取会自动续期
oapiAccessTokenTTL :: Num a => a
oapiAccessTokenTTL = fromIntegral (7200 :: Int)

oapiGetAccessToken :: HttpCallMonad env m
                   => AppKey
                   -> AppSecret
                   -> m (Either OapiError AccessToken)
-- {{{1
oapiGetAccessToken app_key app_secret = do
  oapiGetCall "/gettoken"
    [ "appkey" &= app_key
    , "appsecret" &= app_secret
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "access_token"))
-- }}}1


data UserInfoByCodeResp = UserInfoByCodeResp
                            DeviceId
                            UserId
                            Bool
                            Int

-- {{{1
instance FromJSON UserInfoByCodeResp where
  parseJSON = withObject "UserInfoByCodeResp" $ \ o -> do
    UserInfoByCodeResp <$> o .: "deviceId"
                       <*> o .: "userid"
                       <*> o .: "is_sys"
                       <*> o .: "sys_level"
-- }}}1


oapiGetUserInfoByAuthCode :: HttpCallMonad env m
                          => Text
                          -> OapiRpcWithAtk m UserInfoByCodeResp
-- {{{1
oapiGetUserInfoByAuthCode auth_code = do
  oapiGetCallWithAtk "/user/getuserinfo"
    [ "code" &= auth_code
    ]
-- }}}1


data AuthOrgEntiies = AuthOrgEntiies
  { authDeptIds :: [ DeptId ]
  , authUserIds :: [ UserId ]
  }
  deriving (Show)

instance FromJSON AuthOrgEntiies where
  parseJSON = withObject "AuthOrgEntiies" $ \ o ->
    AuthOrgEntiies <$> o .: "authed_dept"
                   <*> o .: "authed_user"


data AuthScopes = AuthScopes
  { authUserFields      :: [Text]
  , authConditionFields :: [Text]
  , authEntities        :: AuthOrgEntiies
  }
  deriving (Show)

instance FromJSON AuthScopes where
  parseJSON = withObject "AuthScopes" $ \ o ->
    AuthScopes <$> o .: "auth_user_field"
                     <*> o .: "condition_field"
                     <*> o .: "auth_org_scopes"


oapiGetAccessTokenScopes :: HttpCallMonad env m
                         => OapiRpcWithAtk m AuthScopes
oapiGetAccessTokenScopes = oapiGetCallWithAtk "/auth/scopes" []


oapiAutoRetryCall :: (MonadLogger m, MonadIO m, FromJSON a)
                  => String
                  -> Options
                  -> IO (Response LB.ByteString)
                  -> m (Either OapiError a)
oapiAutoRetryCall url_path opts io_call = loop 0
  where
    max_retry_cnt = 2 :: Int

    loop cnt = do
      err_or_res <- liftIO io_call >>= oapiConvertResp url_path opts
      case err_or_res of
        Left err | cnt < max_retry_cnt && oapiErrorCode err == oapiEcForbiddenTemporarily -> do
          liftIO $ threadDelay (500 * 1000)
          $logWarnS logSourceName $ "Retrying call #" <> tshow (cnt + 1) <> ": " <> fromString url_path
          loop (cnt + 1)

        _ -> pure err_or_res


oapiConvertResp :: (FromJSON a, MonadIO m, MonadLogger m)
                => String
                -> Options
                -> Response LB.ByteString
                -> m (Either OapiError a)
-- {{{1
oapiConvertResp url_path opts r = do
  let response_txt = toStrict $ decodeUtf8 (r ^. responseBody)
  $logDebugS logSourceName $ "api '" <> fromString url_path <> "' response:\n" <> response_txt

  case eitherDecode (r ^. responseBody) of
    Left err -> do
      $logErrorS logSourceName $
        "Failed to decode response as JSON: " <> fromString err
        <> ", response was:\n" <> response_txt
      liftIO $ throwIO $ JSONError err

    Right resp_json -> do
      oapiToPayload url_path opts resp_json
-- }}}1


oapiGetCall :: (HttpCallMonad env m, FromJSON a)
            => String
            -> ParamKvList
            -> m (Either OapiError a)
oapiGetCall url_path kv_list = do
  HttpApiRunEnv t sess <- ask
  throttleRemoteCall t $ do
    oapiAutoRetryCall url_path opts (WS.getWith opts sess url)
  where
    url = oapiUrlBase <> url_path
    opts = defaults & applyParamKvListInQs kv_list


oapiGetCallWithAtkLike :: (HttpCallMonad env m, FromJSON a, ParamValue k)
                       => String
                       -> ParamKvList
                       -> ReaderT k m (Either OapiError a)
oapiGetCallWithAtkLike url_path kv_list = do
  atk <- ask
  let kv_list' = ("access_token" &= atk) : kv_list
  lift $ oapiGetCall url_path kv_list'

oapiGetCallWithAtk :: (HttpCallMonad env m, FromJSON a)
                   => String
                   -> ParamKvList
                   -> OapiRpcWithAtk m a
oapiGetCallWithAtk = oapiGetCallWithAtkLike


oapiPostCall :: (HttpCallMonad env m, FromJSON a, Postable b)
             => String
             -> ParamKvList
             -> b
             -> m (Either OapiError a)
oapiPostCall url_path kv_list post_data = do
  HttpApiRunEnv t sess <- ask
  throttleRemoteCall t $ do
    oapiAutoRetryCall url_path opts (WS.postWith opts sess url post_data)
  where
    url = oapiUrlBase <> url_path
    opts = defaults & applyParamKvListInQs kv_list


oapiPostCallWithAtkLike :: (HttpCallMonad env m, FromJSON a, Postable b, ParamValue k)
                        => String
                        -> ParamKvList
                        -> b
                        -> ReaderT k m (Either OapiError a)
oapiPostCallWithAtkLike url_path kv_list post_data = do
  atk <- ask
  let kv_list' = ("access_token" &= atk) : kv_list
  lift $ oapiPostCall url_path kv_list' post_data


oapiPostCallWithAtk :: (HttpCallMonad env m, FromJSON a, Postable b)
                    => String
                    -> ParamKvList
                    -> b
                    -> OapiRpcWithAtk m a
oapiPostCallWithAtk = oapiPostCallWithAtkLike


class DingTalkHttpApiRun a where
  runWithDingTalkHttpEnv :: (HttpCallBaseMonad m)
                         => a
                         -> ReaderT HttpApiRunEnv' m r
                         -> m r

class DingTalkAccessTokenRun a where
  runWithDingTalkAccessToken :: (HttpCallBaseMonad m)
                             => a
                             -> ReaderT AccessToken (ReaderT HttpApiRunEnv' m) r
                             -> m r


-- vim: set foldmethod=marker:
