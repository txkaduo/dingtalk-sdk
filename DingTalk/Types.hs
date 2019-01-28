{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -pgmP cc -optP -E -optP -undef -optP -std=c89 #-}
-- 上面这个命令行是为了使用 ## 这样的CPP操作符. ghc ticket 12516
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
import           Text.Shakespeare.I18N (ToMessage (..))

import DingTalk.Helpers
-- }}}1


#define NEWTYPE_TEXT_DERIVING \
  deriving (Show, Eq, Ord, Typeable, ToMessage, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           , ParamValue \
           )

#define NEWTYPE_DEF(t1, t2) newtype t1 = t1 { un ## t1 :: t2 }

#define NEWTYPE_DEF_TEXT(t1) NEWTYPE_DEF(t1, Text) NEWTYPE_TEXT_DERIVING

#define INSTANCES_BY_SHOW_READ(t) \
instance ParamValue t where { toParamValue = tshow . un ## t } ; \
instance ToJSON t where { toJSON = toJSON . un ## t }; \
instance FromJSON t where { \
  parseJSON (A.String s) = maybe mzero (return . t) $ readMay s ;\
  parseJSON v = fmap t $ parseJSON v; \
                          }

NEWTYPE_DEF_TEXT(AccessToken)
NEWTYPE_DEF_TEXT(SnsAccessToken)
NEWTYPE_DEF_TEXT(SuiteKey)
NEWTYPE_DEF_TEXT(CorpId)
NEWTYPE_DEF_TEXT(CorpSecret)
NEWTYPE_DEF_TEXT(JsApiTicket)
NEWTYPE_DEF_TEXT(Nonce)
NEWTYPE_DEF_TEXT(UserId)
NEWTYPE_DEF_TEXT(SnsAppId)
NEWTYPE_DEF_TEXT(SnsAppSecret)
NEWTYPE_DEF_TEXT(SnsTmpAuthCode)
NEWTYPE_DEF_TEXT(SnsToken)
NEWTYPE_DEF_TEXT(DeviceId)
NEWTYPE_DEF_TEXT(MediaId)
NEWTYPE_DEF_TEXT(OpenId)
NEWTYPE_DEF_TEXT(UnionId)
NEWTYPE_DEF_TEXT(MessageId)
NEWTYPE_DEF_TEXT(SnsPersistentAuthCode)

-- CallbackToken 是回调接口用的 Token
NEWTYPE_DEF_TEXT(CallbackToken)

NEWTYPE_DEF_TEXT(EncodingAesKey)
NEWTYPE_DEF_TEXT(ProcessCode)
NEWTYPE_DEF_TEXT(ProcessInstanceId)
NEWTYPE_DEF_TEXT(BizCategoryId)

-- | 审批实例业务编号. 含义不明，仅见于 "获取单个审批实例" 接口文档
NEWTYPE_DEF_TEXT(ProcessBizId)

NEWTYPE_DEF_TEXT(ProcessTaskId)


NEWTYPE_DEF(AgentId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , ToJSON, FromJSON
           )

NEWTYPE_DEF(ChatId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , ToJSON, FromJSON
           )

NEWTYPE_DEF(CorpConversationAsyncSendId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , ToJSON, FromJSON
           )


NEWTYPE_DEF(RoleId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , ToJSON, FromJSON
           )

NEWTYPE_DEF(DeptId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )

INSTANCES_BY_SHOW_READ(DeptId)

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
NEWTYPE_DEF(AttendPunchDetailsId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendPunchDetailsId)


-- | 考勤记录打卡结果
NEWTYPE_DEF(AttendPunchResId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendPunchResId)

-- | 考勤组
NEWTYPE_DEF(AttendGroupId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendGroupId)

-- | 排班
NEWTYPE_DEF(AttendPlanId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendPlanId)

-- | 考勤班次
NEWTYPE_DEF(AttendClassId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )
INSTANCES_BY_SHOW_READ(AttendClassId)


class HasAccessToken a where
  getAccessToken ::  a -> AccessToken


class HasWreqSession a where
    getWreqSession :: a -> WS.Session

instance HasWreqSession WS.Session where
    getWreqSession = id


-- | 钉钉接口每秒调用到100次就会报错，而且经常缺少批量取得信息的接口
-- 所以要有某种自我约束调用频率的方法
class RemoteCallThrottle a where
  throttleRemoteCall :: (MonadBaseControl IO m) => a -> m c -> m c

instance RemoteCallThrottle () where
  throttleRemoteCall _ = id


data SomeRemoteCallThrottle = forall t. RemoteCallThrottle t => SomeRemoteCallThrottle t

instance RemoteCallThrottle SomeRemoteCallThrottle where
  throttleRemoteCall (SomeRemoteCallThrottle t) = throttleRemoteCall t


class HasDingTalkCorpId a where
  getDingTalkCorpId :: a -> CorpId

class HasDingTalkLoginAppId a where
  getDingTalkLoginAppId :: a -> SnsAppId


data HttpApiRunEnv t = HttpApiRunEnv t WS.Session
  deriving (Functor)

type HttpApiRunEnv' = HttpApiRunEnv SomeRemoteCallThrottle

type HttpCallBaseMonad m = ( MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m )

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


-- vim: set foldmethod=marker:
