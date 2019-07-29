module DingTalk.MiscSpec where

-- {{{1
import ClassyPrelude
import Test.Hspec

import DingTalk.Misc
-- }}}1

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "httpUserAgentDingTalkVer" $ do
    it "works for examples from http access log" $ do
      httpUserAgentDingTalkVer "Mozilla/5.0 (Linux; U; Android 9; zh-CN; EML-AL00 Build/HUAWEIEML-AL00) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/57.0.2987.108 UCBrowser/11.9.4.974 UWS/2.13.2.46 Mobile Safari/537.36 AliApp(DingTalk/4.6.40) com.alibaba.android.rimet/11675274 Channel/227200 language/zh-CN"
        `shouldBe` (Just "4.6.40")

