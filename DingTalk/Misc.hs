module DingTalk.Misc where

-- {{{1 imports
import ClassyPrelude
import qualified Data.Text            as T
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
