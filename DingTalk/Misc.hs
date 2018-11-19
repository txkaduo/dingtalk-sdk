module DingTalk.Misc where

-- {{{1 imports
import ClassyPrelude
import qualified Data.Text            as T

import DingTalk.Types
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


rawTextToCorpIdOrSuiteKey :: Text -> Either CorpId SuiteKey
rawTextToCorpIdOrSuiteKey t =
  if "ding" `isPrefixOf` t || "corp" `isPrefixOf` t
     then Left $ CorpId t
     else Right $ SuiteKey t



-- vim: set foldmethod=marker:
