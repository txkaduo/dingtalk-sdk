module DingTalk.OAPI.CryptoSpec where

-- {{{1
import ClassyPrelude
import Data.Either
import Control.Exception (ErrorCall(..), evaluate)
import Test.Hspec
import Test.QuickCheck
import DingTalk.OAPI.Crypto
import DingTalk.Types
-- }}}1

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "oapiPreprocessUrlForSign" $ do
    it "works for example as doc" $ do
      oapiPreprocessUrlForSign "http://abc.com?url=http%3A%2F%2Fabc.com%2Fsomewhere"
        `shouldBe` "http://abc.com?url=http://abc.com/somewhere"

      oapiPreprocessUrlForSign "http://abc.com?url=http%3A%2F%2Fabc.com%2Fsomewhere#xxx"
        `shouldBe` "http://abc.com?url=http://abc.com/somewhere"

      oapiPreprocessUrlForSign "http://abc.com" `shouldBe` "http://abc.com"

    it "works for invalid query string" $ do
      evaluate (oapiPreprocessUrlForSign "http://abc.com?url=http%3A%2F%2Fabc.com%2somewhere")
        `shouldThrow` ( \ (_ :: ErrorCall) -> True)

  describe "oapiSignUrlWithJsApiTicket'" $ do
    it "works for simple input" $ do
      oapiSignUrlWithJsApiTicket' (JsApiTicket "xxx") (Nonce "nonce") 23423244 "http://abc.com"
        `shouldBe` "1ff3af2ddfb918fd2b930996bb954024f3d7eb59"


  describe "Dingtalk encryption related" $ do
    describe "parseEncodingAesKey" $ do
      it "works for sample input" $ do
        parseEncodingAesKey (EncodingAesKey "1234567890123456789012345678901234567890123")
          `shouldSatisfy` isRight

        parseEncodingAesKey (EncodingAesKey "11111111lvdhntotr3x9qhlbytb18zyz5z111111111")
          `shouldSatisfy` isRight


    describe "encryptForProcessApi/decryptForProcessApi" $ do
      it "decryptForProcessApi reverse encryptForProcessApi" $ do
        aes_env <- either (\ err -> error $ "parseEncodingAesKey failed: " <> err) return $
                      parseEncodingAesKey (EncodingAesKey "1234567890123456789012345678901234567890123")

        let origin_msg = "this_is_the_msg"
        let corp_id = CorpId "corpid"
            encrypted_msg = encryptForProcessApi aes_env (Left corp_id) "0123456789123456" origin_msg

        (decrypted_msg, corp_id2) <-
            either (\ err -> error $ "decryptForProcessApi failed: " <> err) return $
                          decryptForProcessApi aes_env encrypted_msg

        decrypted_msg `shouldBe` origin_msg
        (CorpId corp_id2) `shouldBe` corp_id
