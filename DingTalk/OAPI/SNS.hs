module DingTalk.OAPI.SNS
  ( oapiSnsGetAccessToken, oapiSnsAccessTokenTTL
  , oapiSnsGetPersistentAuthCodeTTL
  , SnsPersistentAuthCodeResp(..), oapiSnsGetPersistentAuthCode
  , oapiSnsGetUserSnsToken
  , SnsUserInfo(..), oapiSnsGetUserInfo
  , oapiSnsQrCodeLoginRedirectUrl, oapiSnsDingTalkLoginRedirectUrl
  , validateSnsTmpAuthCode
  , oapiGetCallWithSnsAtk, oapiPostCallWithSnsAtk
  , DingTalkSnsAccessTokenRun(..)
  ) where

-- {{{1 imports
import           ClassyPrelude
import qualified Data.ByteString.Builder as BB
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.Proxy
import           Network.HTTP.Types   (renderQueryText)
import           Network.Wreq.Types   (Postable)

import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


-- | 获取开放应用的access token
oapiSnsGetAccessToken :: HttpCallMonad env m
                      => SnsAppId
                      -> SnsAppSecret
                      -> m (Either OapiError SnsAccessToken)
-- {{{1
oapiSnsGetAccessToken app_id app_secret = do
  oapiGetCall "/sns/gettoken"
    [ "appid" &= app_id
    , "appsecret" &= app_secret
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "access_token"))
-- }}}1


-- | 文档说 access token 有效期固定为 7200 秒，且每次有效期内重复获取会自动续期
oapiSnsAccessTokenTTL :: Num a => a
oapiSnsAccessTokenTTL = fromIntegral (7200 :: Int)


-- | 永久授权码无过期时间
oapiSnsGetPersistentAuthCodeTTL :: Num a => a
oapiSnsGetPersistentAuthCodeTTL = fromIntegral (maxBound :: Int)


data SnsPersistentAuthCodeResp = SnsPersistentAuthCodeResp
  { snsPersistentAuthCodeRespOpenId   :: OpenId
  , snsPersistentAuthCodeRespUnionId  :: UnionId
  , snsPersistentAuthCodeRespAuthCode :: SnsPersistentAuthCode
  }

instance FromJSON SnsPersistentAuthCodeResp where
  parseJSON = withObject "SnsPersistentAuthCodeResp" $ \ o ->
    SnsPersistentAuthCodeResp <$> o .: "openid"
                              <*> o .: "unionid"
                              <*> o .: "persistent_code"


-- | 文档无解释登录不成功会不会收到code，收到什么样的code
validateSnsTmpAuthCode :: SnsTmpAuthCode -> Bool
validateSnsTmpAuthCode = not . null . unSnsTmpAuthCode


oapiSnsGetPersistentAuthCode :: HttpCallMonad env m
                             => SnsTmpAuthCode
                             -> ReaderT SnsAccessToken m (Either OapiError SnsPersistentAuthCodeResp)
oapiSnsGetPersistentAuthCode tmp_auth_code =
  oapiPostCallWithSnsAtk "/sns/get_persistent_code" [] (object [ "tmp_auth_code" .= tmp_auth_code ])


oapiSnsGetUserSnsToken :: HttpCallMonad env m
                       => OpenId
                       -> SnsPersistentAuthCode
                       -> ReaderT SnsAccessToken m (Either OapiError SnsToken)
oapiSnsGetUserSnsToken open_id p_auth_code =
  oapiPostCallWithSnsAtk "/sns/get_sns/token" []
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
