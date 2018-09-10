module DingTalk.OAPI.CryptoSpec where

-- {{{1
import ClassyPrelude
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
