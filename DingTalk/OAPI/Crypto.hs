module DingTalk.OAPI.Crypto where

-- {{{1 imports
import           ClassyPrelude
import           Control.Exception          (evaluate)
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64.URL as B64L
import qualified Data.ByteString.Char8      as C8
import qualified Data.Text                  as T
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Network.HTTP               (urlDecode)
import           System.Random              (randomIO)

import DingTalk.Types
-- }}}1


-- | 生成随机 Nonce
oapiMakeNonce :: MonadIO m
              => Int
              -> m Nonce
-- {{{1
oapiMakeNonce salt_len = liftIO $ do
  liftM (Nonce . fromString . C8.unpack . take salt_len . B64L.encode . pack) $
      replicateM gen_len randomIO
  where gen_len = salt_len  -- long enough
-- }}}1


oapiSignParams :: [(Text, Text)] -> Text
-- {{{1
oapiSignParams =
  toLower . decodeUtf8 . B16.encode . SHA1.hash . encodeUtf8 . join_params . sortWith fst
  where
    join_params = intercalate "&" . map (\ (x, y) -> x <> "=" <> y)
-- }}}1


-- | 文档说：url要去掉 hash (#后的部分)，query string为 url decode 一次
-- CAUTION: may throw exception if query string is invalid
oapiPreprocessUrlForSign :: Text -> Text
-- {{{1
oapiPreprocessUrlForSign url = url2 <> qs'
  where
    url0 = fst $ T.breakOnEnd url "#"
    url1 = T.takeWhile (/= '#') $ if null url0 then url else url0
    (url2, qs) = T.breakOn "?" url1
    qs' = pack $ urlDecode $ unpack qs
-- }}}1


oapiSignUrlWithJsapiTicket' :: JsapiTicket
                            -> Nonce
                            -> Int64
                            -> Text
                            -> Text
-- {{{1
oapiSignUrlWithJsapiTicket' ticket nonce t url =
  oapiSignParams
      [ ("url", oapiPreprocessUrlForSign url)
      , ("noncestr", unNonce nonce)
      , ("jsapi_ticket", unJsapiTicket ticket)
      , ("timestamp", tshow t)
      ]
-- }}}1


oapiSignUrlWithJsapiTicket :: JsapiTicket
                           -> Text
                           -> IO (Either SomeException
                                          (Text, (Nonce, Int64))
                                 )
                                 -- ^ 正常情况下得到签名，及 Nonce, 时间
-- {{{1
oapiSignUrlWithJsapiTicket ticket url = tryAny $ do
  nonce <- oapiMakeNonce 8
  t <- fmap round $ getPOSIXTime
  sign <- evaluate $ oapiSignUrlWithJsapiTicket' ticket nonce t url
  return (sign, (nonce, t))
-- }}}1

  


-- vim: set foldmethod=marker:
