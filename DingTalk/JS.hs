module DingTalk.JS where

-- {{{1 imports
import           ClassyPrelude
import           Data.Aeson           as A
import qualified Data.Aeson.Types     as A
import           Data.Time.Clock.POSIX

import DingTalk.Types
import DingTalk.Helpers
import DingTalk.OAPI.Basic
import DingTalk.OAPI.Crypto
-- }}}1


-- | 移动端的jsapi文件URL
-- see: https://open-doc.dingtalk.com/docs/doc.htm?spm=a219a.7629140.0.0.aunBua&treeId=171&articleId=104910&docType=1
mobileClientJsSdkUrl :: IsString s => s
mobileClientJsSdkUrl = "https://g.alicdn.com/dingding/dingtalk-jsapi/2.0.72/dingtalk.open.js"


-- | PC端的jsapi文件URL
-- see: https://open-doc.dingtalk.com/docs/doc.htm?spm=a219a.7629140.0.0.faWTxU&treeId=176&articleId=104955&docType=1
pcClientJsSdkUrl :: IsString s => s
pcClientJsSdkUrl = "https://g.alicdn.com/dingding/dingtalk-pc-api/2.7.0/index.js"


data JsApiTicketResp = JsApiTicketResp
  { jsApiTicketRespTicket :: JsApiTicket
  , jsApiTicketRespTTL    :: Int
  }

instance FromJSON JsApiTicketResp where
  parseJSON = withObject "JsApiTicketResp" $ \ o ->
                JsApiTicketResp <$> o .: "ticket"
                                <*> o .: "expires_in"


-- | 注意：
-- 1.在企业应用中，在jsticket未过期的时候通过get_jsapi_ticket获取到的都是一个全新的jsticket（和旧的jsticket值不同），这个全新的jsticket过期时间是2小时。
-- 2.在ISV应用中，在jsticket未过期的时候，也就是两小时之内通过get_jsapi_ticket获取到的jsticket和老的jsticket值相同，只是过期时间延长到2小时。
-- 3.jsticket是以一个企业对应一个，所以在使用的时候需要将jsticket以企业为维度进行缓存下来（设置缓存过期时间2小时），并不需要每次都通过接口拉取。
oapiGetJsApiTicket :: HttpCallMonad env m
                   => ReaderT AccessToken m (Either OapiError JsApiTicketResp)
oapiGetJsApiTicket = oapiGetCallWithAtk "/get_jsapi_ticket"
    [ "type" &= asText "jsapi"
    ]


-- | 用于 js 的 config 函数的参数
jsapiConfigOptions :: CorpId
                   -> JsApiTicket
                   -> AgentId
                   -> POSIXTime
                   -> Nonce
                   -> Text
                   -> [Text]
                   -> [A.Pair]
-- {{{1
jsapiConfigOptions corp_id ticket agent_id t nonce url api_list =
  [ "corpId" .= corp_id
  , "agentId" .= agent_id
  , "timeStamp" .= tshow (round t :: Int64)
  , "nonceStr" .= nonce
  , "signature" .= signature
  , "jsApiList" .= api_list
  ]
  where signature = oapiSignUrlWithJsApiTicket' ticket nonce t url
-- }}}1

-- vim: set foldmethod=marker:
