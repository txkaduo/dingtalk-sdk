{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.OAPI.Crypto where

-- {{{1 imports
import           ClassyPrelude
import           Control.Arrow              (left)
import           Control.Exception          (evaluate)
import qualified Data.Binary.Get            as Bin
import qualified Data.Binary.Put            as Bin
import qualified Crypto.Cipher.AES          as CN
import qualified Crypto.Error               as CN
import qualified Crypto.Hash                as CN
import qualified Crypto.Cipher.Types        as CN
import qualified Crypto.Data.Padding        as CN
import qualified Crypto.Random.Types        as CN
import qualified Data.ByteString.Base64     as B64
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
  toLower . tshow . (CN.hash :: _ -> CN.Digest CN.SHA1) . encodeUtf8 . join_params . sortWith fst
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


data DingTalkAesEnv = DingTalkAesEnv CN.AES256 (CN.IV CN.AES256)

instance Show DingTalkAesEnv where
  show (DingTalkAesEnv _ _) = show (asString "AES256", asString "some IV")


-- | 解释钉钉的AES Key字串
parseEncodingAesKey :: EncodingAesKey -> Either String DingTalkAesEnv
-- {{{1
parseEncodingAesKey (EncodingAesKey key_txt) = do
  bs <- B64.decode $ encodeUtf8 (key_txt <> "=")
  unless (length bs == 32) $ do
    Left $ "Invalid AES Key length: " <> show (length bs)

  cipher <- left show $ CN.eitherCryptoError $ CN.cipherInit bs
  iv <- maybe (Left "failed to generate IV") return $ CN.makeIV $ take 16 bs
  return (DingTalkAesEnv cipher iv)
-- }}}1


newEncodingAesKey :: MonadIO m => m EncodingAesKey
newEncodingAesKey = fmap (EncodingAesKey . fromString) $ randomAlphaNumString 43


encryptForProcessApi' :: (MonadIO m)
                      => DingTalkAesEnv
                      -> Either CorpId SuiteKey
                      -> ByteString
                      -> m ByteString
encryptForProcessApi' aes_key corp_or_suite msg = do
  random_pad <- liftIO $ CN.getRandomBytes 16
  return $ encryptForProcessApi aes_key corp_or_suite random_pad msg


encryptForProcessApi :: DingTalkAesEnv
                     -> Either CorpId SuiteKey
                     -> ByteString -- ^ random padding string, must be of length 16
                     -> ByteString -- ^ message payload
                     -> ByteString
-- {{{1
encryptForProcessApi (DingTalkAesEnv cipher iv) corp_or_suite random_pad msg = do
  B64.encode $ CN.cbcEncrypt cipher iv padded_msg
  where msg_len = length msg
        msg_len_pad = toStrict $ Bin.runPut $ Bin.putWord32be $ fromIntegral msg_len
        key = encodeUtf8 $ either toParamValue toParamValue corp_or_suite
        block_size = CN.blockSize cipher
        msg_to_encrypt = random_pad <> msg_len_pad <> msg <> key 
        padded_msg = CN.pad (CN.PKCS7 block_size) msg_to_encrypt
-- }}}1


decryptForProcessApi :: DingTalkAesEnv
                     -> ByteString
                     -> Either String (ByteString, Text)
-- ^ result is (message payload, CorpId/SuiteKey)
-- {{{1
decryptForProcessApi (DingTalkAesEnv cipher iv) msg = do
  bs_to_decrypt <- B64.decode msg
  let msg_decrypted = CN.cbcDecrypt cipher iv bs_to_decrypt
  msg_unpadded <- maybe (Left "Failed to unpad") return $ CN.unpad (CN.PKCS7 block_size) msg_decrypted
  when (length msg_unpadded <= 16 + 4) $ do
    Left $ "Length of unpadded message is: " <> show (length msg_unpadded)

  let msg1 = drop 16 msg_unpadded
  let (msg_len_pad, msg_and_key) = splitAt 4 msg1
  msg_len <- fmap fromIntegral $
              either (const $ Left "length part is invalid") (return . thd3) $
                Bin.runGetOrFail Bin.getWord32be $ fromStrict msg_len_pad

  when (length msg_and_key <= msg_len) $ do
    Left $ "length part is too large: " <> show msg_len

  let (msg_payload, key) = splitAt msg_len msg_and_key
  return (msg_payload, decodeUtf8 key)
  where block_size = CN.blockSize cipher
-- }}}1


signForProcessApi :: IsString a
                  => CallbackToken
                  -> POSIXTime
                  -> Nonce
                  -> ByteString -- ^ 这应是加密的信息json里的 'encrypt' 字段里的字串
                  -> a
-- {{{1
signForProcessApi (CallbackToken token) timestamp (Nonce nonce) msg =
  fromString $ show $ (CN.hash :: _ -> CN.Digest CN.SHA1) $
    encodeUtf8 (token <> tshow timestamp <> nonce) <> msg
-- }}}1


thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

-- vim: set foldmethod=marker:
