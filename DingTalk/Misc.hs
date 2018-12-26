module DingTalk.Misc where

-- {{{1 imports
import ClassyPrelude
import Control.Monad.Logger
import qualified Data.Text            as T
import Text.Show.Unicode (ushow)

import DingTalk.Types
import DingTalk.OAPI.Basic
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


logDingTalkError :: MonadLogger m => m a -> Either OapiError a -> m a
-- {{{1
logDingTalkError f err_or = do
  case err_or of
    Right x -> return x
    Left err -> do
        logDingTalkError_ err
        f
-- }}}1


logDingTalkError_ :: MonadLogger m => OapiError -> m ()
-- {{{1
logDingTalkError_ err = do
  $logError $ "DingTalk API error: " <> fromString (ushow err)
-- }}}1


-- vim: set foldmethod=marker:
