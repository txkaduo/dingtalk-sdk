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
import           Control.Monad.Fail
import           Data.Aeson as A
import           Data.Time
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


data ProcessOpType = ProcessOpExecuteTaskNormal           -- ^ 正常执行任务
                   | ProcessOpExecuteTaskAgent            -- ^ 代理人执行任务
                   | ProcessOpExecuteTaskAuto             -- ^ undocumented
                   | ProcessOpAppendTaskBefore            -- ^ 前加签任务
                   | ProcessOpAppendTaskAfter             -- ^ 后加签任务
                   | ProcessOpRedirectTask                -- ^ 转交任务
                   | ProcessOpStartProcessInst            -- ^ 发起流程实例
                   | ProcessOpTerminateProcessInst        -- ^ 终止(撤销)流程实例
                   | ProcessOpFinishProcessInst           -- ^ 结束流程实例
                   | ProcessOpAddRemark                   -- ^ 添加评论
                   | ProcessOpCc                          -- ^ 不在文档中，运行时发现，应该是抄送之意
                   | ProcessOpRedirectProcess             -- ^ 审批退回
                   | ProcessOpTypeNone                        -- ^ 文档没有说明。看起来像是或签时取消未操作任务。
                   deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue ProcessOpType where
  toParamValue ProcessOpExecuteTaskNormal    = "EXECUTE_TASK_NORMAL"
  toParamValue ProcessOpExecuteTaskAgent     = "EXECUTE_TASK_AGENT"
  toParamValue ProcessOpExecuteTaskAuto      = "EXECUTE_TASK_AUTO"
  toParamValue ProcessOpAppendTaskBefore     = "APPEND_TASK_BEFORE"
  toParamValue ProcessOpAppendTaskAfter      = "APPEND_TASK_AFTER"
  toParamValue ProcessOpRedirectTask         = "REDIRECT_TASK"
  toParamValue ProcessOpStartProcessInst     = "START_PROCESS_INSTANCE"
  toParamValue ProcessOpTerminateProcessInst = "TERMINATE_PROCESS_INSTANCE"
  toParamValue ProcessOpFinishProcessInst    = "FINISH_PROCESS_INSTANCE"
  toParamValue ProcessOpAddRemark            = "ADD_REMARK"
  toParamValue ProcessOpCc                   = "PROCESS_CC"
  toParamValue ProcessOpRedirectProcess      = "REDIRECT_PROCESS"
  toParamValue ProcessOpTypeNone             = "NONE"

instance ToJSON ProcessOpType where toJSON = toJSON . toParamValue

instance FromJSON ProcessOpType where
  parseJSON = parseJsonParamValueEnumBounded "ProcessOpType"
-- }}}1


data ProcessOpResult = ProcessOpApproved
                     | ProcessOpDenied
                     | ProcessOpNone    -- ^ undocumented
                     deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1
instance ParamValue ProcessOpResult where
  toParamValue ProcessOpApproved = "AGREE"
  toParamValue ProcessOpDenied   = "REFUSE"
  toParamValue ProcessOpNone     = "NONE"

instance ToJSON ProcessOpResult where toJSON = toJSON . toParamValue

instance FromJSON ProcessOpResult where
  parseJSON = parseJsonParamValueEnumBounded "ProcessOpResult"
-- }}}1


data ProcessOpRecord = ProcessOpRecord
  { processOpUserId :: UserId
  , processOpTime   :: LocalTime
  , processOpType   :: ProcessOpType
  , processOpResult :: ProcessOpResult
  , processOpRemark :: Maybe Text
  }

instance FromJSON ProcessOpRecord where
  parseJSON = withObject "ProcessOpRecord" $ \ o -> do
                ProcessOpRecord <$> o .: "userid"
                                <*> o .: "date"
                                <*> o .: "operation_type"
                                <*> o .: "operation_result"
                                <*> (o .:? "remark" >>= nullTextAsNothing)

-- not needed for DingTalk API, but useful for serializing for cache
instance ToJSON ProcessOpRecord where
  toJSON (ProcessOpRecord {..}) = object $ catMaybes $
    [ pure $ "userid" .= processOpUserId
    , pure $ "date" .= formatTime defaultTimeLocale "%F %T" processOpTime
    , pure $ "operation_type" .= processOpType
    , pure $ "operation_result" .= processOpResult
    , ("remark" .=) <$> processOpRemark
    ]


-- | 审批实例业务动作
data ProcessBizAction = ProcessBizModify  -- ^ MODIFY表示该审批实例是基于原来的实例修改而来
                      | ProcessBizRevoke  -- ^ REVOKE表示该审批实例是由原来的实例撤销后重新发起的
                      | ProcessBizNone    -- ^ NONE表示正常发起
                      deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue ProcessBizAction where
  toParamValue ProcessBizModify = "MODIFY"
  toParamValue ProcessBizRevoke = "REVOKE"
  toParamValue ProcessBizNone   = "NONE"

instance ToJSON ProcessBizAction where toJSON = toJSON . toParamValue

instance FromJSON ProcessBizAction where
  parseJSON = parseJsonParamValueEnumBounded "ProcessBizAction"
-- }}}1


-- | 任务状态，分为
--   NEW（未启动），RUNNING（处理中），PAUSED（暂停），CANCELED（取消），COMPLETED（完成），TERMINATED（终止）
data ProcessTaskStatus = ProcessTaskNew
                       | ProcessTaskRunning
                       | ProcessTaskPaused
                       | ProcessTaskCanceled
                       | ProcessTaskCompleted
                       | ProcessTaskTerminated
                      deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue ProcessTaskStatus where
  toParamValue ProcessTaskNew        = "NEW"
  toParamValue ProcessTaskRunning    = "RUNNING"
  toParamValue ProcessTaskPaused     = "PAUSED"
  toParamValue ProcessTaskCanceled   = "CANCELED"
  toParamValue ProcessTaskCompleted  = "COMPLETED"
  toParamValue ProcessTaskTerminated = "TERMINATED"

instance ToJSON ProcessTaskStatus where toJSON = toJSON . toParamValue

instance FromJSON ProcessTaskStatus where
  parseJSON = parseJsonParamValueEnumBounded "ProcessTaskStatus"
-- }}}1


-- | 结果，分为
-- AGREE（同意），REFUSE（拒绝），REDIRECTED（转交）
data ProcessTaskResult = ProcessTaskAgreed
                       | ProcessTaskRefused
                       | ProcessTaskRedirected
                       | ProcessTaskNone  -- ^ 文档没解释这是什么
                                          -- ^ XXX: 经测试，或签的情况下，其他没有表示意见的人时NONE
                      deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue ProcessTaskResult where
  toParamValue ProcessTaskAgreed     = "AGREE"
  toParamValue ProcessTaskRefused    = "REFUSE"
  toParamValue ProcessTaskRedirected = "REDIRECTED"
  toParamValue ProcessTaskNone       = "NONE"

instance ToJSON ProcessTaskResult where toJSON = toJSON . toParamValue

instance FromJSON ProcessTaskResult where
  parseJSON = parseJsonParamValueEnumBounded "ProcessTaskResult"
-- }}}1


data ProcessTaskInfo = ProcessTaskInfo
  { processTaskInfoUserId     :: UserId
  , processTaskInfoStatus     :: ProcessTaskStatus
  , processTaskInfoResult     :: ProcessTaskResult
  , processTaskInfoCreateTime :: Maybe LocalTime
  -- ^ 实测这有可能不出现．比如流程有两个环节时就会这样
  , processTaskInfoFinishTime :: Maybe LocalTime
  , processTaskInfoId         :: ProcessTaskId
  , processTaskInfoUrl        :: Text -- undocumented
  }

-- {{{1 instances
instance FromJSON ProcessTaskInfo where
  parseJSON = withObject "ProcessTaskInfo" $ \ o -> do
                ProcessTaskInfo <$> o .: "userid"
                                <*> o .: "task_status"
                                <*> o .: "task_result"
                                <*> o .:? "create_time"
                                <*> o .:? "finish_time"
                                <*> o .: "taskid"
                                <*> o .: "url"

-- not needed for DingTalk API, but useful for serializing for cache
instance ToJSON ProcessTaskInfo where
  toJSON (ProcessTaskInfo {..}) = object $ catMaybes
    [ pure $ "userid" .= processTaskInfoUserId
    , pure $ "task_status" .= processTaskInfoStatus
    , pure $ "task_result" .= processTaskInfoResult
    , ( "create_time" .= ) . formatTime defaultTimeLocale "%F %T" <$> processTaskInfoCreateTime
    , ( "finish_time" .= ) . formatTime defaultTimeLocale "%F %T" <$> processTaskInfoFinishTime
    , pure $ "taskid" .= processTaskInfoId
    , pure $ "url" .= processTaskInfoUrl
    ]
-- }}}1


data FormComponentInput = FormComponentInput
  { formComponentInputName     :: Text
  , formComponentInputType     :: Maybe Text
  -- ^ 似乎有了 ext_value 就没 component_type
  , formComponentInputValue    :: Text
  , formComponentInputExtValue :: Maybe Value
  }

instance FromJSON FormComponentInput where
  parseJSON = withObject "FormComponentInput" $ \ o -> do
                m_name <- o .:? "name"
                typ <- o .:? "component_type"
                case m_name of
                  Nothing -> do
                    if typ == Just "TextNote"
                       then pure (FormComponentInput "" typ "" Nothing)
                       else fail $ unpack $ "missing 'name' field, while type is '" <> tshow typ <> "'"

                  Just name ->
                    FormComponentInput name typ
                      <$> o .: "value"
                       <*> o .:? "ext_value"

instance ToJSON FormComponentInput where
  toJSON (FormComponentInput {..}) =
    object $ catMaybes
          [ pure $ "name" .= formComponentInputName
          , ("component_type" .=) <$> formComponentInputType
          , pure $ "value" .= formComponentInputValue
          , ("ext_value" .=) <$> formComponentInputExtValue
          ]

-- 明细类型的值实际返回的例子是这样的
  {--
   {
        "value": "[{\"rowValue\":[{\"componentType\":\"NumberField\",\"label\":\"报销金额(元)\",\"value\":\"51\",\"key\":\"报销金额(元)\"},{\"componentType\":\"TextField\",\"label\":\"报销
类别\",\"value\":\"活动经费\",\"key\":\"报销类别\"},{\"componentType\":\"TextareaField\",\"label\":\"费用明细\",\"value\":\"天河北音乐会，买8瓶王老吉跟三瓶小矿泉水。折后一共51元\",\"key\":\"费用明细\"},{\"componentType\":\"DDSelectField\",\"label\":\"所属校区\",\"extendValue\":{\"key\":\"option_JYTXA9UZ\"},\"value\":\"总部\",\"key\":\"DDSelectField-JLQOJD9L\"}]}]",
        "name": "报销明细",
        "ext_value": "{\"statValue\":[{\"id\":\"报销金额(元)\",\"label\":\"总报销金额(元)\",\"num\":\"51\",\"upper\":\"\"}],\"componentName\":\"TableField\"}"
    }
  -}
-- 最外面仍然是 FormComponentInput 一样的结构
-- value 包含了复合结构的字串序列化
-- 我们分解其逻辑结构为下面若干个类型
data FormCompDetailsRow = FormCompDetailsRow
  { fcdRowLabel :: Text
  -- ^ 从上面的例子看 key, label 有时相同，但 label 应该总是给人看到的那个字串
  , fcdRowValue :: Text
  , fcdRowType  :: Text -- NumberField, TextareaField, DDSelectField
  }

instance FromJSON FormCompDetailsRow where
  parseJSON = withObject "FormCompDetailsRow" $ \ o -> do
    FormCompDetailsRow <$> o .: "label"
                       <*> o .: "value"
                       <*> o .: "componentType"

instance ToJSON FormCompDetailsRow where
  toJSON (FormCompDetailsRow {..}) = object [ "label" .= fcdRowLabel
                                            , "value" .= fcdRowValue
                                            , "componentType" .= fcdRowType
                                            ]


data FormCompDetailsX = FormCompDetailsX
  { fcdRows :: [ FormCompDetailsRow ]
  }

instance ToJSON FormCompDetailsX where
  toJSON (FormCompDetailsX {..}) = object [ "rowValue" .= fcdRows ]

instance FromJSON FormCompDetailsX where
  parseJSON = withObject "FormCompDetailsX" $ \ o -> do
    FormCompDetailsX <$> o .: "rowValue"


-- vim: set foldmethod=marker:
