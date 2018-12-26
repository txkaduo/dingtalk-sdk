module DingTalk.Misc where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Trans.Except
import           Control.Monad.Logger
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Text.Show.Unicode (ushow)

import DingTalk.Types
import DingTalk.OAPI.Basic
import DingTalk.OAPI.Contacts
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


-- | 在某个部门下，递归查找指定用户名的信息
-- XXX: 如果部门下的用户比较多，会比较慢
filterUserByNameInDept :: HttpCallMonad env m
                       => DeptId
                       -> Text
                       -> OapiRpcWithAtk m [UserDetails]
-- {{{1
filterUserByNameInDept dept_id name = runExceptT $ do
  oapiSourceDeptUserSimpleInfoRecursive dept_id
    =$= CL.filter ((== name) . userSimpleInfoName)
    =$= CL.mapM (ExceptT . oapiGetUserDetails . userSimpleInfoId)
    =$= CL.catMaybes
    $$ CL.consume
-- }}}1

-- vim: set foldmethod=marker:
