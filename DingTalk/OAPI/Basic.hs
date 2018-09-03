module DingTalk.OAPI.Basic where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Reader (asks)
import           Data.Aeson           as A
import           Network.Wreq
import           Network.Wreq.Types   (Postable)
import qualified Network.Wreq.Session as WS

import DingTalk.Types
import DingTalk.Helpers
-- }}}1


data OapiError = OapiError
  { oapiErrorCode :: Int
  , oapiErrorMsg :: Text
  }
  deriving (Show)

-- {{{1 instances
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


data AccessTokenResp = AccessTokenResp
  { accessTokenRespAccessToken :: AccessToken
  }

-- {{{1 instances
instance FromJSON AccessTokenResp where
  parseJSON = withObject "AccessTokenResp" $ \ o -> do
    AccessTokenResp <$> o .: "access_token"
-- }}}1


oapiGetAccessToken' :: HttpCallMonad env m
                    => CorpId
                    -> CorpSecret
                    -> m (Either OapiError AccessToken)
oapiGetAccessToken' corp_id corp_secret =
  fmap (fmap accessTokenRespAccessToken) $ oapiGetAccessToken corp_id corp_secret


oapiGetAccessToken :: HttpCallMonad env m
                   => CorpId
                   -> CorpSecret
                   -> m (Either OapiError AccessTokenResp)
-- {{{1
oapiGetAccessToken corp_id corp_secret = do
  oapiGetCall "/gettoken"
    [ "corpid" &= corp_id
    , "corpsecret" &= corp_secret
    ]
-- }}}1


data JsapiTicketResp = JsapiTicketResp
  { jsapiTicketRespTicket :: JsapiTicket
  , jsapiTicketRespExpiresIn :: Int
  }

-- {{{1 instances
instance FromJSON JsapiTicketResp where
  parseJSON = withObject "JsapiTicketResp" $ \ o -> do
    JsapiTicketResp <$> o .: "ticket"
                    <*> o .: "expires_in"
-- }}}1


oapiGetJsapiTicket :: HttpCallMonad env m
                   => ReaderT AccessToken m (Either OapiError JsapiTicketResp)
-- {{{1
oapiGetJsapiTicket = do
  oapiGetCallWithAtk "/get_jsapi_ticket"
    [ "type" &= asText "jsapi"
    ]
-- }}}1


data UserInfoByCodeResp = UserInfoByCodeResp
                            DeviceID
                            UserID
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


data AuthOrgScopes = AuthOrgScopes
                      [ DeptID ]
                      [ UserID ]
                    deriving (Show)

instance FromJSON AuthOrgScopes where
  parseJSON = withObject "AuthOrgScopes" $ \ o ->
    AuthOrgScopes <$> o .: "authed_dept"
                  <*> o .: "authed_user"


data AuthTokenScopeResp = AuthTokenScopeResp
                            [Text]
                            [Text]
                            AuthOrgScopes
                          deriving (Show)

instance FromJSON AuthTokenScopeResp where
  parseJSON = withObject "AuthTokenScopeResp" $ \ o ->
    AuthTokenScopeResp <$> o .: "auth_user_field"
                       <*> o .: "condition_field"
                       <*> o .: "auth_org_scopes"


oapiGetAuthTokenScopes :: HttpCallMonad env m
                       => ReaderT AccessToken m (Either OapiError AuthTokenScopeResp)
oapiGetAuthTokenScopes = oapiGetCallWithAtk "/auth/scopes" []



oapiGetCall :: (HttpCallMonad env m, FromJSON a)
            => String
            -> ParamKvList
            -> m (Either OapiError a)
-- {{{1
oapiGetCall url_path kv_list = do
  sess <- asks getWreqSession
  liftIO (WS.getWith opts sess url)
    >>= asJSON
    >>= return . view responseBody
    >>= oapiToPayload
  where
    url = oapiUrlBase <> url_path
    opts = defaults & applyParamKvListInQs kv_list
-- }}}1


oapiGetCallWithAtk :: (HttpCallMonad env m, FromJSON a)
                   => String
                   -> ParamKvList
                   -> ReaderT AccessToken m (Either OapiError a)
-- {{{1
oapiGetCallWithAtk url_path kv_list = do
  atk <- ask
  let kv_list' = ("access_token" &= atk) : kv_list
  lift $ oapiGetCall url_path kv_list'
-- }}}1


oapiPostCall :: (HttpCallMonad env m, FromJSON a, Postable b)
             => String
             -> ParamKvList
             -> b
             -> m (Either OapiError a)
-- {{{1
oapiPostCall url_path kv_list post_data = do
  sess <- asks getWreqSession
  liftIO (WS.postWith opts sess url post_data)
    >>= asJSON
    >>= return . view responseBody
    >>= oapiToPayload
  where
    url = oapiUrlBase <> url_path
    opts = defaults & applyParamKvListInQs kv_list
-- }}}1


oapiPostCallWithAtk :: (HttpCallMonad env m, FromJSON a, Postable b)
                    => String
                    -> ParamKvList
                    -> b
                    -> ReaderT AccessToken m (Either OapiError a)
-- {{{1
oapiPostCallWithAtk url_path kv_list post_data = do
  atk <- ask
  let kv_list' = ("access_token" &= atk) : kv_list
  lift $ oapiPostCall url_path kv_list' post_data
-- }}}1



-- vim: set foldmethod=marker:
