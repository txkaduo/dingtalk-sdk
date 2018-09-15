module DingTalk.OAPI.Basic
  ( OapiError(..), OapiErrorOrPayload(..)
  , oapiGetAccessToken, oapiAccessTokenTTL
  , UserInfoByCodeResp(..), oapiGetUserInfoByAuthCode
  , AuthOrgEntiies(..), AuthScopes(..), oapiGetAccessTokenScopes
  , oapiGetCall, oapiGetCallWithAtk, oapiPostCallWithAtk
  , oapiGetCallWithAtkLike, oapiPostCallWithAtkLike
  , oapiUrlBase
  , DingTalkAccessTokenRun(..)
  , module DingTalk.Types
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Reader (asks)
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import qualified Data.ByteString.Lazy as LB
import           Data.Proxy
import           Network.Wreq hiding (Proxy)
import           Network.Wreq.Types   (Postable)
import qualified Network.Wreq.Session as WS
import           Text.Show.Unicode (ushow)

import DingTalk.Types
import DingTalk.Helpers
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


oapiToPayload :: (MonadLogger m, MonadThrow m, FromJSON a)
              => Value
              -> m (Either OapiError a)
-- {{{1
oapiToPayload v = do
  case fromJSON v of
    A.Error err -> do
      $logErrorS logSourceName $ "Could not parse response body to payload: " <> fromString err
      throwM $ DatagramError err

    A.Success (OapiErrorOrPayload x) -> return x
-- }}}1


-- | 文档说 access token 有效期固定为 7200 秒，且每次有效期内重复获取会自动续期
oapiAccessTokenTTL :: Num a => a
oapiAccessTokenTTL = fromIntegral (7200 :: Int)

oapiGetAccessToken :: HttpCallMonad env m
                   => CorpId
                   -> CorpSecret
                   -> m (Either OapiError AccessToken)
-- {{{1
oapiGetAccessToken corp_id corp_secret = do
  oapiGetCall "/gettoken"
    [ "corpid" &= corp_id
    , "corpsecret" &= corp_secret
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
                          -> ReaderT AccessToken m (Either OapiError UserInfoByCodeResp)
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
                         => ReaderT AccessToken m (Either OapiError AuthScopes)
oapiGetAccessTokenScopes = oapiGetCallWithAtk "/auth/scopes" []


oapiConvertResp :: (FromJSON a, MonadIO m, MonadLogger m, MonadThrow m)
                => String
                -> Response LB.ByteString
                -> m (Either OapiError a)
-- {{{1
oapiConvertResp url_path r = do
  let response_txt = toStrict $ decodeUtf8 (r ^. responseBody)
  $logDebugS logSourceName $ "api '" <> fromString url_path <> "' response:\n" <> response_txt

  case eitherDecode (r ^. responseBody) of
    Left err -> do
      $logErrorS logSourceName $
        "Failed to decode response as JSON: " <> fromString err
        <> ", response was:\n" <> response_txt
      liftIO $ throwM $ JSONError err

    Right resp_json -> do
      oapiToPayload resp_json
-- }}}1


oapiGetCall :: (HttpCallMonad env m, FromJSON a)
            => String
            -> ParamKvList
            -> m (Either OapiError a)
-- {{{1
oapiGetCall url_path kv_list = do
  sess <- asks getWreqSession
  liftIO (WS.getWith opts sess url)
    >>= oapiConvertResp url_path
  where
    url = oapiUrlBase <> url_path
    opts = defaults & applyParamKvListInQs kv_list
-- }}}1


oapiGetCallWithAtkLike :: (HttpCallMonad env m, FromJSON a, ParamValue k)
                       => String
                       -> ParamKvList
                       -> ReaderT k m (Either OapiError a)
-- {{{1
oapiGetCallWithAtkLike url_path kv_list = do
  atk <- ask
  let kv_list' = ("access_token" &= atk) : kv_list
  lift $ oapiGetCall url_path kv_list'
-- }}}1

oapiGetCallWithAtk :: (HttpCallMonad env m, FromJSON a)
                   => String
                   -> ParamKvList
                   -> ReaderT AccessToken m (Either OapiError a)
oapiGetCallWithAtk = oapiGetCallWithAtkLike


oapiPostCall :: (HttpCallMonad env m, FromJSON a, Postable b)
             => String
             -> ParamKvList
             -> b
             -> m (Either OapiError a)
-- {{{1
oapiPostCall url_path kv_list post_data = do
  sess <- asks getWreqSession
  liftIO (WS.postWith opts sess url post_data)
    >>= oapiConvertResp url_path
  where
    url = oapiUrlBase <> url_path
    opts = defaults & applyParamKvListInQs kv_list
-- }}}1


oapiPostCallWithAtkLike :: (HttpCallMonad env m, FromJSON a, Postable b, ParamValue k)
                        => String
                        -> ParamKvList
                        -> b
                        -> ReaderT k m (Either OapiError a)
-- {{{1
oapiPostCallWithAtkLike url_path kv_list post_data = do
  atk <- ask
  let kv_list' = ("access_token" &= atk) : kv_list
  lift $ oapiPostCall url_path kv_list' post_data
-- }}}1


oapiPostCallWithAtk :: (HttpCallMonad env m, FromJSON a, Postable b)
                    => String
                    -> ParamKvList
                    -> b
                    -> ReaderT AccessToken m (Either OapiError a)
oapiPostCallWithAtk = oapiPostCallWithAtkLike


class (MonadIO m, MonadLogger m) => DingTalkAccessTokenRun m a where
  runWithDingTalkAccessToken :: a -> ReaderT AccessToken (ReaderT WS.Session m) r -> m r


-- vim: set foldmethod=marker:
