module DingTalk.OAPI.Crypto where

-- {{{1 imports
import           ClassyPrelude
import           Control.Exception          (evaluate)
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Base16     as B16
import qualified Data.Text                  as T
import           Data.Time.Clock.POSIX
import           Network.HTTP               (urlDecode)

import DingTalk.Types
import DingTalk.Helpers
-- }}}1


-- | 生成随机 Nonce
oapiMakeNonce :: MonadIO m
              => Int
              -> m Nonce
-- {{{1
oapiMakeNonce = fmap (Nonce . fromString) . oapiMakeString


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


oapiSignUrlWithJsApiTicket' :: JsApiTicket
                            -> Nonce
                            -> POSIXTime
                            -> Text
                            -> Text
-- {{{1
oapiSignUrlWithJsApiTicket' ticket nonce t url =
  oapiSignParams
      [ ("url", oapiPreprocessUrlForSign url)
      , ("noncestr", unNonce nonce)
      , ("jsapi_ticket", unJsApiTicket ticket)
      , ("timestamp", tshow (round t :: Int64))
      ]
-- }}}1


oapiSignUrlWithJsApiTicket :: JsApiTicket
                           -> Text
                           -> IO (Either SomeException
                                          (Text, (Nonce, POSIXTime))
                                 )
                                 -- ^ 正常情况下得到签名，及 Nonce, 时间
-- {{{1
oapiSignUrlWithJsApiTicket ticket url = tryAny $ do
  nonce <- oapiMakeNonce 8
  t <- getPOSIXTime
  sign <- evaluate $ oapiSignUrlWithJsApiTicket' ticket nonce t url
  return (sign, (nonce, t))
-- }}}1

  


-- vim: set foldmethod=marker:
