module DingTalk.OAPI.SNS
  ( oapiSnsGetToken
  , SnsPersistentAuthCodeResp(..), oapiSnsGetPersistentAuthCode
  , oapiSnsGetUserSnsToken
  , SnsUserInfo(..), oapiSnsGetUserInfo
  , oapiSnsQrCodeLoginRedirectUrl, oapiSnsDingTalkLoginRedirectUrl
  , httpUserAgentDingTalkVer
  ) where

-- {{{1 imports
import           ClassyPrelude
import qualified Data.ByteString.Builder as BB
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.Proxy
import qualified Data.Text            as T
import           Network.HTTP.Types   (renderQueryText)

import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


-- | 获取开放应用的access token
oapiSnsGetToken :: HttpCallMonad env m
                => SnsAppId
                -> SnsAppSecret
                -> m (Either OapiError AccessToken)
-- {{{1
oapiSnsGetToken app_id app_secret = do
  oapiGetCall "/sns/gettoken"
    [ "appid" &= app_id
    , "appsecret" &= app_secret
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "access_token"))
-- }}}1


data SnsPersistentAuthCodeResp = SnsPersistentAuthCodeResp OpenId UnionId SnsPersistentAuthCode

instance FromJSON SnsPersistentAuthCodeResp where
  parseJSON = withObject "SnsPersistentAuthCodeResp" $ \ o ->
    SnsPersistentAuthCodeResp <$> o .: "openid"
                              <*> o .: "unionid"
                              <*> o .: "persistent_code"


oapiSnsGetPersistentAuthCode :: HttpCallMonad env m
                             => SnsTmpAuthCode
                             -> ReaderT AccessToken m (Either OapiError SnsPersistentAuthCodeResp)
oapiSnsGetPersistentAuthCode tmp_auth_code =
  oapiPostCallWithAtk "/sns/get_persistent_code" [] (object [ "tmp_auth_code" .= tmp_auth_code ])


oapiSnsGetUserSnsToken :: HttpCallMonad env m
                       => OpenId
                       -> SnsPersistentAuthCode
                       -> ReaderT AccessToken m (Either OapiError SnsToken)
oapiSnsGetUserSnsToken open_id p_auth_code =
  oapiPostCallWithAtk "/sns/get_sns/token" []
    $ object [ "openid" .= open_id, "persistent_code" .= p_auth_code ]



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

oapiSnsGetUserInfo :: HttpCallMonad env m
                   => SnsToken
                   -> m (Either OapiError SnsUserInfo)
oapiSnsGetUserInfo sns_token =
  oapiGetCall "/sns/getuserinfo"
    [ "sns_token" &= sns_token ]
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


httpUserAgentDingTalkVer :: Text -> Maybe Text
-- {{{1
httpUserAgentDingTalkVer ua = do
  guard $ not $ null s1
  let s2 = T.drop (T.length p) s1
  let (ver, s3) = T.breakOn ")" s2
  guard $ not $ null s3
  return $ T.strip ver
  where
    p = "(DingTalk/"
    (_, s1) = T.breakOn p ua
-- }}}1


-- vim: set foldmethod=marker:
