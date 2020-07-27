module DingTalk.OAPI.SNS
  ( SnsUserInfo(..), oapiSnsGetUserInfoByTmpAuthCode
  , oapiSnsQrCodeLoginRedirectUrl, oapiSnsDingTalkLoginRedirectUrl
  , validateSnsTmpAuthCode
  , oapiGetCallWithSnsAtk, oapiPostCallWithSnsAtk
  , DingTalkSnsAccessTokenRun(..)
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Logger
import qualified Data.ByteString.Builder as BB
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.Proxy
import           Data.Time.Clock.POSIX
import           Network.HTTP.Types   (renderQueryText)
import           Network.Wreq.Types   (Postable)

import DingTalk.OAPI.Crypto (snsCallSignaturePure)
import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


-- | 文档无解释登录不成功会不会收到code，收到什么样的code
validateSnsTmpAuthCode :: SnsTmpAuthCode -> Bool
validateSnsTmpAuthCode = not . null . unSnsTmpAuthCode


data SnsUserInfo = SnsUserInfo
  { snsUserInfoNick    :: Text
  , snsUserInfoOpenId  :: OpenId
  , snsUserInfoUnionId :: UnionId
  }

instance FromJSON SnsUserInfo where
  parseJSON = withObject "SnsUserInfo" $ \ o ->
                SnsUserInfo <$> o .: "nick"
                            <*> o .: "openid"
                            <*> o .: "unionid"


snsCallSignature :: MonadIO m => SnsAppSecret -> m (Timestamp, Text)
snsCallSignature secret = do
  ts <- liftIO $ fmap timestampFromPOSIXTime getPOSIXTime
  pure (ts, snsCallSignaturePure secret ts)


oapiSnsGetUserInfoByTmpAuthCode :: HttpCallMonad env m
                                => SnsAppId
                                -> SnsAppSecret
                                -> SnsTmpAuthCode
                                -> m (Either OapiError SnsUserInfo)
oapiSnsGetUserInfoByTmpAuthCode app_id app_secret tmp_auth_code = do
  (ts, signature) <- snsCallSignature app_secret
  $logDebugS logSourceName $ "ts=" <> tshow ts
  $logDebugS logSourceName $ "signature=" <> tshow signature
  oapiPostCall "/sns/getuserinfo_bycode"
    [ "accessKey" &= app_id, "timestamp" &= ts, "signature" &= signature ]
    (object [ "tmp_auth_code" .= tmp_auth_code ])
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "user_info"))


-- | 扫码登录第三方网站的入口URL
-- https://oapi.dingtalk.com/connect/qrconnect?appid=APPID&response_type=code&scope=snsapi_login&state=STATE&redirect_uri=REDIRECT_URI
oapiSnsQrCodeLoginRedirectUrl :: SnsAppId
                              -> Maybe Text
                              -> Text
                              -> Text
-- {{{1
oapiSnsQrCodeLoginRedirectUrl app_id m_state return_url =
  url_endpoint <> decodeUtf8 (toStrict (BB.toLazyByteString $ renderQueryText True qs_vars))
  where
    qs_vars = [ ("appid", Just (toParamValue app_id))
              , ("response_type", Just "code")
              , ("scope", Just "snsapi_login")
              , ("state", m_state)
              , ("redirect_uri", Just return_url)
              ]

    url_endpoint = "https://oapi.dingtalk.com/connect/qrconnect"
-- }}}1

-- | 钉钉内打开登录第三方网站的入口URL
-- https://oapi.dingtalk.com/connect/oauth2/sns_authorize?appid=APPID&response_type=code&scope=snsapi_auth&state=STATE&redirect_uri=REDIRECT_URI
oapiSnsDingTalkLoginRedirectUrl :: SnsAppId
                                -> Maybe Text
                                -> Text
                                -> Text
-- {{{1
oapiSnsDingTalkLoginRedirectUrl app_id m_state return_url =
  url_endpoint <> decodeUtf8 (toStrict (BB.toLazyByteString $ renderQueryText True qs_vars))
  where
    qs_vars = [ ("appid", Just (toParamValue app_id))
              , ("response_type", Just "code")
              , ("scope", Just "snsapi_auth")
              , ("state", m_state)
              , ("redirect_uri", Just return_url)
              ]

    url_endpoint = "https://oapi.dingtalk.com/connect/oauth2/sns_authorize"
-- }}}1


oapiGetCallWithSnsAtk :: (HttpCallMonad env m, FromJSON a)
                      => String
                      -> ParamKvList
                      -> ReaderT SnsAccessToken m (Either OapiError a)
oapiGetCallWithSnsAtk = oapiGetCallWithAtkLike


oapiPostCallWithSnsAtk :: (HttpCallMonad env m, FromJSON a, Postable b)
                       => String
                       -> ParamKvList
                       -> b
                       -> ReaderT SnsAccessToken m (Either OapiError a)
oapiPostCallWithSnsAtk = oapiPostCallWithAtkLike


class DingTalkSnsAccessTokenRun a where
  runWithDingTalkSnsAccessToken :: (HttpCallBaseMonad m)
                                => a
                                -> ReaderT SnsAccessToken (ReaderT HttpApiRunEnv' m) r
                                -> m r


-- vim: set foldmethod=marker:
