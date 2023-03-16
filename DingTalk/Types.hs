{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module DingTalk.Types
  ( module DingTalk.Types
  , ParamValue(..), SomeParamValue(..)
  , DatagramError(..)
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Logger
import           Data.Aeson as A
import           Data.Time.Clock.POSIX
import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))
import qualified Network.Wreq.Session  as WS
import           Text.Blaze.Html       (ToMarkup (..))
import           Web.HttpApiData (ToHttpApiData (..), FromHttpApiData (..))
import           Yesod.Core

import DingTalk.Helpers

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Monad.Trans.Control
#endif
-- }}}1


#define NEWTYPE_TEXT_DERIVING \
  deriving (Show, Read, Eq, Ord, Typeable, ToMessage, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           , ParamValue \
           , PathPiece \
           , ToHttpApiData, FromHttpApiData \
           )

#define NEWTYPE_DEF(t1, un_t1, t2) newtype t1 = t1 { un_t1 :: t2 }

#define NEWTYPE_DEF_TEXT(t1, un_t1) NEWTYPE_DEF(t1, un_t1, Text) NEWTYPE_TEXT_DERIVING

#define INSTANCES_BY_SHOW_READ(t, un_t) \
instance ParamValue t where { toParamValue = tshow . un_t } ; \
instance ToJSON t where { toJSON = toJSON . un_t }; \
instance FromJSON t where { \
  parseJSON (A.String s) = maybe mzero (return . t) $ readMay s ;\
  parseJSON v = fmap t $ parseJSON v; \
                          }

NEWTYPE_DEF_TEXT(AccessToken, unAccessToken)
NEWTYPE_DEF_TEXT(SuiteKey, unSuiteKey)
NEWTYPE_DEF_TEXT(CorpId, unCorpId)
NEWTYPE_DEF_TEXT(AppKey, unAppKey)
NEWTYPE_DEF_TEXT(AppSecret, unAppSecret)
NEWTYPE_DEF_TEXT(JsApiTicket, unJsApiTicket)
NEWTYPE_DEF_TEXT(Nonce, unNonce)
NEWTYPE_DEF_TEXT(UserId, unUserId)
NEWTYPE_DEF_TEXT(SnsTmpAuthCode, unSnsTmpAuthCode)
NEWTYPE_DEF_TEXT(DeviceId, unDeviceId)
NEWTYPE_DEF_TEXT(MediaId, unMediaId)
NEWTYPE_DEF_TEXT(OpenId, unOpenId)
NEWTYPE_DEF_TEXT(UnionId, unUnionId)
NEWTYPE_DEF_TEXT(MessageId, unMessageId)

-- | 从现在文档看，登录应用取 access token 跟其它钉钉应用使用相同的接口取 access token
-- 所以类型上不区分 SnsAppId/AppKey 等
type SnsAppId = AppKey
type SnsAppSecret = AppSecret
type SnsAccessToken = AccessToken

-- CallbackToken 是回调接口用的 Token
NEWTYPE_DEF_TEXT(CallbackToken, unCallbackToken)

NEWTYPE_DEF_TEXT(EncodingAesKey, unEncodingAesKey)
NEWTYPE_DEF_TEXT(ProcessCode, unProcessCode)
NEWTYPE_DEF_TEXT(ProcessInstanceId, unProcessInstanceId)
NEWTYPE_DEF_TEXT(BizCategoryId, unBizCategoryId)

-- | 审批实例业务编号. 含义不明，仅见于 "获取单个审批实例" 接口文档
NEWTYPE_DEF_TEXT(ProcessBizId, unProcessBizId)

NEWTYPE_DEF_TEXT(ProcessTaskId, unProcessTaskId)

-- | 日志模板标识
NEWTYPE_DEF_TEXT(ReportCode, unReportCode)

-- | 日志唯一id
NEWTYPE_DEF_TEXT(ReportId, unReportId)


NEWTYPE_DEF(AgentId, unAgentId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , ToJSON, FromJSON
           )

NEWTYPE_DEF(ChatId, unChartId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , ToJSON, FromJSON
           )

NEWTYPE_DEF(CorpConversationAsyncSendId, unCorpConversationAsyncSendId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , ToJSON, FromJSON
           )


NEWTYPE_DEF(RoleId, unRoleId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , ToJSON, FromJSON
           )

NEWTYPE_DEF(DeptId, unDeptId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )

INSTANCES_BY_SHOW_READ(DeptId, unDeptId)

-- | 根部门代表的就是整个企业
rootDeptId :: DeptId
rootDeptId = DeptId 1



data ProcessInstResult = ProcessApproved | ProcessDenied
  deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue ProcessInstResult where
  toParamValue ProcessApproved = "agree"
  toParamValue ProcessDenied   = "refuse"

instance ToJSON ProcessInstResult where
  toJSON = toJSON . toParamValue

instance FromJSON ProcessInstResult where
  parseJSON = parseJsonParamValueEnumBounded "ProcessInstResult"
-- }}}1


-- | 考勤记录详情
NEWTYPE_DEF(AttendPunchDetailsId, unAttendPunchDetailsId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendPunchDetailsId, unAttendPunchDetailsId)


-- | 考勤记录打卡结果
NEWTYPE_DEF(AttendPunchResId, unAttendPunchResId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendPunchResId, unAttendPunchResId)

-- | 考勤组
NEWTYPE_DEF(AttendGroupId, unAttendGroupId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendGroupId, unAttendGroupId)

-- | 排班
NEWTYPE_DEF(AttendPlanId, unAttendPlanId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendPlanId, unAttendPlanId)

-- | 考勤班次
NEWTYPE_DEF(AttendClassId, unAttendClassId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendClassId, unAttendClassId)


class HasAccessToken a where
  getAccessToken ::  a -> AccessToken


class HasWreqSession a where
    getWreqSession :: a -> WS.Session

instance HasWreqSession WS.Session where
    getWreqSession = id


-- | 钉钉接口每秒调用到100次就会报错，而且经常缺少批量取得信息的接口
-- 所以要有某种自我约束调用频率的方法
class RemoteCallThrottle a where
  throttleRemoteCall :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
                     => a -> m c -> m c

instance RemoteCallThrottle () where
  throttleRemoteCall _ = id


data SomeRemoteCallThrottle = forall t. RemoteCallThrottle t => SomeRemoteCallThrottle t

instance RemoteCallThrottle SomeRemoteCallThrottle where
  throttleRemoteCall (SomeRemoteCallThrottle t) = throttleRemoteCall t


class HasDingTalkCorpId a where
  getDingTalkCorpId :: a -> CorpId

class HasDingTalkLoginApp a where
  getDingTalkLoginAppId :: a -> SnsAppId
  getDingTalkLoginAppSecret :: a -> SnsAppSecret


data HttpApiRunEnv t = HttpApiRunEnv t WS.Session
  deriving (Functor)

type HttpApiRunEnv' = HttpApiRunEnv SomeRemoteCallThrottle

type HttpCallBaseMonad m = ( MonadIO m, MonadLogger m, MonadBaseControl IO m )

type HttpCallMonad t m = ( HttpCallBaseMonad m
                         , RemoteCallThrottle t
                         , MonadReader (HttpApiRunEnv t) m
                         )



-- | 估计所有钉钉的时间戳都是以毫秒为单位
newtype Timestamp = Timestamp { unTimestamp :: Int64 }
  deriving (Eq, Ord, Num)

-- {{{1
instance ParamValue Timestamp where
  toParamValue = tshow . unTimestamp

instance ToJSON Timestamp where toJSON = toJSON . unTimestamp

instance FromJSON Timestamp where
  -- 报文中，数字经常以字串的形式出现
  parseJSON (A.String s) = maybe mzero (return . Timestamp) $ readMay s
  parseJSON v = fmap Timestamp $ parseJSON v

instance Show Timestamp where show = showTimeStamp
-- }}}1


timestampFromPOSIXTime :: POSIXTime -> Timestamp
timestampFromPOSIXTime = Timestamp . round . (* 1000)


timestampToPOSIXTime :: Timestamp -> POSIXTime
timestampToPOSIXTime = (/ 1000) . fromIntegral . unTimestamp


showTimeStamp :: Timestamp -> String
showTimeStamp ts = show (posixSecondsToUTCTime ept)
  where ept = timestampToPOSIXTime ts


-- | 文档说 access token 有效期固定为 7200 秒，且每次有效期内重复获取会自动续期
accessTokenTTL :: Num a => a
accessTokenTTL = fromIntegral (7200 :: Int)


data ProcessInfo = ProcessInfo
  { processInfoName    :: Text
  , processInfoCode    :: ProcessCode
  , processInfoIconUrl :: Text
  , processInfoUrl     :: Text
  }

data ProcessListResponse = ProcessListResponse
  { processListNextCursor :: Maybe Int
  , processListItems      :: [ProcessInfo]
  }

-- {{{1 instances
instance FromJSON ProcessInfo where
  parseJSON = withObject "ProcessInfo" $ \ o -> do
                ProcessInfo <$> o .: "name"
                            <*> o .: "process_code"
                            <*> o .: "icon_url"
                            <*> o .: "url"

instance ToJSON ProcessInfo where
  toJSON (ProcessInfo {..}) =
    object [ "name" .= processInfoName
           , "process_code" .= processInfoCode
           , "icon_url" .= processInfoIconUrl
           , "url" .= processInfoUrl
           ]

instance FromJSON ProcessListResponse where
  parseJSON = withObject "ProcessListResponse" $ \ o -> do
                o2 <- o .: "result"
                ProcessListResponse <$> o2 .:? "next_cursor"
                                    <*> o2 .: "process_list"
-- }}}1


-- vim: set foldmethod=marker:
