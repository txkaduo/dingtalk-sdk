module DingTalk.OAPI.Basic where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Reader (asks)
import           Data.Aeson           as A
import           Network.Wreq
import qualified Network.Wreq.Session as WS

import DingTalk.Types
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
      $logError $ "Could not parse response body to payload: " <> fromString err
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
  sess <- asks getWreqSession
  liftIO (WS.getWith opts sess url)
    >>= asJSON
    >>= return . view responseBody
    >>= oapiToPayload
  where
    url = oapiUrlBase <> "/gettoken"
    opts = defaults & param "corpid" .~ [ unCorpId corp_id ]
                    & param "corpsecret" .~ [ unCorpSecret corp_secret ]
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
                   => AccessToken
                   -> m (Either OapiError JsapiTicketResp)
-- {{{1
oapiGetJsapiTicket atk = do
  sess <- asks getWreqSession
  liftIO (WS.getWith opts sess url)
    >>= asJSON
    >>= return . view responseBody
    >>= oapiToPayload
  where
    url = oapiUrlBase <> "/get_jsapi_ticket"
    opts = defaults & param "access_token" .~ [ unAccessToken atk ]
                    & param "type" .~ [ asText "jsapi" ]
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
                          => AccessToken
                          -> Text
                          -> m (Either OapiError UserInfoByCodeResp)
-- {{{1
oapiGetUserInfoByAuthCode atk auth_code = do
  sess <- asks getWreqSession
  liftIO (WS.getWith opts sess url)
    >>= asJSON
    >>= return . view responseBody
    >>= oapiToPayload
  where
    url = oapiUrlBase <> "/user/getuserinfo"
    opts = defaults & param "access_token" .~ [ unAccessToken atk ]
                    & param "code" .~ [ auth_code ]
-- }}}1


-- vim: set foldmethod=marker:
