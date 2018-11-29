module DingTalk.JS where

-- {{{1 imports
import           ClassyPrelude
import           Control.Arrow
import           Control.Monad.Logger
import           Control.Monad.Trans.Except
import           Data.Aeson           as A
import qualified Data.Aeson.Types     as A
import qualified Database.Redis as R

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


-- | 就是为了 redisGetOrUpdateJsApiTicket 可能出的错定义的类型
data RedisError = RedisErrorReply R.Reply
                | RedisErrorStatus R.Status
                deriving (Show)


-- | 根据文档要求，使用redis实现简单的加锁并更新的功能
-- 方法是设置一个key，保证当时是不存在的，然后调用 oapiGetJsApiTicket，得到返回后保存在另一个key里
-- 并不是绝对可靠，但应该若redis服务器及钉钉接口可靠，出错的机会极小
redisGetOrUpdateJsApiTicket :: (HttpCallMonad env m, MonadLoggerIO m)
                            => R.Connection
                            -> (ByteString, ByteString)
                            -> Int  -- ^ time out, in seconds
                            -> ReaderT AccessToken m (Either (Either RedisError OapiError) JsApiTicket)
-- {{{1
redisGetOrUpdateJsApiTicket conn (lock_key, save_key) max_seconds = do
  log_func <- askLoggerIO
  atk <- ask
  e <- lift ask
  random_str <- fmap fromString $ randomBase64String 16
  err_or <- liftIO $ R.runRedis conn $ runExceptT $ do
    m_ticket <- fmap (fmap $ JsApiTicket . decodeUtf8) $ ExceptT $ R.get save_key
    case m_ticket of
      Just x -> return $ Right x
      Nothing -> do
        let go cnt = do
              lock_st <- ExceptT $ R.setOpts lock_key random_str (R.SetOpts (Just $ fromIntegral max_seconds) Nothing (Just R.Nx))
              case lock_st of
                R.Ok -> do
                  err_or <- liftIO $ flip runLoggingT log_func $ flip runReaderT e $ flip runReaderT atk $ oapiGetJsApiTicket
                  case err_or of
                    Left err -> return $ Left $ Right err
                    Right (JsApiTicketResp ticket ttl) -> do
                      void $ ExceptT $ R.setOpts save_key
                                            (encodeUtf8 $ unJsApiTicket ticket)
                                            (R.SetOpts (Just $ fromIntegral $ ttl - 300) Nothing Nothing)
                      void $ ExceptT $ R.del [lock_key]
                      return $ Right ticket

                _ -> do
                  if cnt < div (max_seconds * 1000 * 1000) loop_delay
                     then liftIO (threadDelay loop_delay) >> go (cnt + 1)
                     else return $ Left $ Left lock_st

        go 0

  return $ either (Left . Left . RedisErrorReply) (left $ left RedisErrorStatus) err_or
  where
    loop_delay = 500 * 1000
-- }}}1


-- | 用于 js 的 config 函数的参数
jsapiConfigOptions :: CorpId
                   -> JsApiTicket
                   -> AgentId
                   -> Timestamp
                   -> Nonce
                   -> Text
                   -> [Text]
                   -> [A.Pair]
-- {{{1
jsapiConfigOptions corp_id ticket agent_id t nonce url api_list =
  [ "corpId" .= corp_id
  , "agentId" .= agent_id
  , "timeStamp" .= t
  , "nonceStr" .= nonce
  , "signature" .= signature
  , "jsApiList" .= api_list
  ]
  where signature = oapiSignUrlWithJsApiTicket' ticket nonce t url
-- }}}1

-- vim: set foldmethod=marker:
