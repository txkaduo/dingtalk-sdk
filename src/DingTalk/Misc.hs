module DingTalk.Misc where

-- {{{1 imports
import           ClassyPrelude hiding (bracket)
import           UnliftIO.Exception (bracket)
import           Control.Monad.Trans.Except
import           Control.Monad.Logger
import qualified Control.Monad.Logger.CallStack as LCS
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.Show.Unicode (ushow)
import qualified System.Clock as SC
import           GHC.Stack (HasCallStack)

import DingTalk.Types
import DingTalk.OAPI.Basic
import DingTalk.OAPI.Contacts

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Concurrent (threadDelay)
#endif
-- }}}1


dingTalkTimeZone :: TimeZone
dingTalkTimeZone = hoursToTimeZone 8

dayToTimestamp :: Day -> Timestamp
dayToTimestamp = timestampFromPOSIXTime . utcTimeToPOSIXSeconds . localTimeToUTC tz . flip LocalTime midnight
  where tz = dingTalkTimeZone

timestampToZonedTime :: Timestamp -> ZonedTime
timestampToZonedTime = utcToZonedTime tz . posixSecondsToUTCTime . timestampToPOSIXTime
  where tz = dingTalkTimeZone

timestampToDay :: Timestamp -> Day
timestampToDay = localDay . zonedTimeToLocalTime . timestampToZonedTime


nullUserIdToNothing :: UserId -> Maybe UserId
nullUserIdToNothing (UserId t) = guard (not $ null t) >> pure (UserId t)


newtype DingTalkRunner = DingTalkRunner
  { runDingTalk :: forall a m. (HttpCallBaseMonad m)
                => ReaderT AccessToken (ReaderT HttpApiRunEnv' m) a
                -> m a
  }


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


logDingTalkError :: (HasCallStack, MonadLogger m, Show e) => m a -> Either e a -> m a
logDingTalkError f err_or = do
  case err_or of
    Right x -> return x
    Left err -> do
        logDingTalkError_ err
        f


logDingTalkError_ :: (HasCallStack, MonadLogger m, Show e) => e -> m ()
logDingTalkError_ err = do
  LCS.logError $ "DingTalk API error: " <> fromString (ushow err)


-- | 在某个部门下，递归查找指定用户名的信息
-- XXX: 如果部门下的用户比较多，会比较慢
filterUserByNameInDept :: HttpCallMonad env m
                       => DeptId
                       -> Text
                       -> OapiRpcWithAtk m [UserDetails]
-- {{{1
filterUserByNameInDept dept_id name = runExceptT $ runConduit $ do
  oapiSourceDeptUserSimpleInfoRecursive dept_id
    .| CL.filter ((== name) . userSimpleInfoName)
    .| CL.mapM (ExceptT . oapiGetUserDetails . userSimpleInfoId)
    .| CL.catMaybes
    .| CL.consume
-- }}}1


data MinimalIntervalThrottle = MinimalIntervalThrottle
                                Float -- ^ seconds
                                (MVar (Maybe SC.TimeSpec))

instance RemoteCallThrottle MinimalIntervalThrottle where
  throttleRemoteCall (MinimalIntervalThrottle interval mvar) f = do
    bracket (liftIO $ takeMVar mvar) (const save_current_time) $ \ m_last_time -> do
      when (interval_us > 0) $ do
        now <- liftIO $ SC.getTime SC.Monotonic

        forM_ m_last_time $ \ last_time -> do
          let time_diff_nano = SC.toNanoSecs $ SC.diffTimeSpec now last_time
          when (time_diff_nano < interval_us * 1000) $ do
            let delay_us = fromIntegral $ interval_us - time_diff_nano `div` 1000
            --   $logWarnS logSourceName $ "Delaying call for " <> tshow delay_us <> " us."
            liftIO $ threadDelay delay_us

      f

    where interval_us = round $ interval * 1000 * 1000
          save_current_time = liftIO $ SC.getTime SC.Monotonic >>= putMVar mvar . Just


newMinimalIntervalThrottle :: MonadIO m => m MinimalIntervalThrottle
newMinimalIntervalThrottle = liftIO $ MinimalIntervalThrottle 0 <$> newMVar Nothing


overrideMinimalIntervalThrottle :: Float -> MinimalIntervalThrottle -> MinimalIntervalThrottle
overrideMinimalIntervalThrottle new_interval (MinimalIntervalThrottle _ mvar) =
  MinimalIntervalThrottle new_interval mvar


-- vim: set foldmethod=marker:
