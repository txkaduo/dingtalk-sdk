module DingTalk.OAPI.Process
  ( ProcessInfo(..), ProcessListResponse(..)
  , maxOapiGetProcessBatchSize
  , oapiGetProcessListByUser
  , oapiSourceProcessListByUser
  , CcTiming(..), FormComponentValue(..), FormCompValueNameValues, ToFormComponentValue(..)
  , (@=), (@=!), (@=?)
  , ConcensusType(..), Approvers, ToApprovers(..), oapiCreateProcessInstance
  , maxOapiGetProcessInstBatchSize
  , ProcessInstListResponse(..)
  , oapiGetProcessInstanceIdList, oapiSourceProcessInstId
  , ProcessInstStatus(..), ProcessOpType(..), ProcessOpResult(..)
  , ProcessOpRecord(..), ProcessBizAction(..), ProcessTaskStatus(..), ProcessTaskResult(..), FormComponentInput(..)
  , ProcessTaskInfo(..), ProcessInstInfo(..)
  , processInstInfoFormLookup
  , oapiGetProcessInstanceInfo
  , oapiGetUserProcessInstanceToDo
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Except hiding (mapM_, mapM)
import           Data.Aeson           as A
import           Data.Aeson.Text      as A
import qualified Data.Aeson.Extra     as AE
import           Data.Conduit
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Proxy
import           Data.Time
import qualified Data.Set.NonEmpty as NES
import           Money

import DingTalk.OAPI.Basic
import DingTalk.OAPI.Contacts
import DingTalk.Helpers

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Concurrent (threadDelay)
#endif
-- }}}1


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


maxOapiGetProcessBatchSize :: Int
maxOapiGetProcessBatchSize = 100


-- | 获取用户可见的审批模板
-- XXX: 此接口的文档已从官网文档入口中消失，但页面还在
-- https://open-doc.dingtalk.com/microapp/serverapi2/tcwmha
-- 不知道是否将来会删除
--
-- XXX: 若接口的调用频率限制很严
-- [Error] oapiSourceProcessListByUser failed: OapiError { errcode=88, errmsg="ding talk error[subcode=90002,submsg=您的服务器调用钉钉开放平台当前接口的所有请求都被暂时禁用了, apiPath(dingtalk.oapi.process.listbyuserid), 从 2018-12-09 15:01:10 到 2018-12-09 15:01:10 请求总次数超过了 100 次, 处罚将在 2018-12-09 15:01:11 结束.]" } @(main:Main tools/manage.hs:284:11)
oapiGetProcessListByUser :: HttpCallMonad env m
                         => Maybe UserId
                         -> Int
                         -> Int
                         -> OapiRpcWithAtk m ProcessListResponse
-- {{{1
oapiGetProcessListByUser m_user_id offset batch_size =
  oapiPostCallWithAtk "/topapi/process/listbyuserid"
    []
    ( object $ catMaybes
        [ ("userid" .=) <$> m_user_id
        , Just $ "offset" .= offset
        , Just $ "size" .= min maxOapiGetProcessBatchSize batch_size
        ]
    )
-- }}}1


oapiSourceProcessListByUser :: HttpCallMonad env m
                            => Float  -- ^ seconds. delay between iterations
                            -> Maybe UserId
                            -> OapiRpcWithAtkSource m ProcessInfo
-- {{{1
oapiSourceProcessListByUser delay_sec m_user_id = loop 0
  where size = maxOapiGetProcessBatchSize
        delay_us = round $ delay_sec * 1000 * 1000
        delay = liftIO $ threadDelay delay_us

        loop offset = do
          resp <- lift $ ExceptT $ oapiGetProcessListByUser m_user_id offset size
          mapM_ yield (processListItems resp)
          mapM_ (\ x -> delay >> loop x) (processListNextCursor resp)
-- }}}1


-- | 抄送的时机
data CcTiming = CcOnStart
              | CcOnFinish
              | CcOnStartFinish
              deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue CcTiming where
  toParamValue CcOnStart       = "START"
  toParamValue CcOnFinish      = "FINISH"
  toParamValue CcOnStartFinish = "START_FINISH"

instance ToJSON CcTiming where
  toJSON = toJSON . toParamValue
-- }}}1


-- TODO: 钉钉修正可以支持日期区间、数字、金额控件了，待补充
data FormComponentValue = FormCompValueText Text    -- ^ 普通文字输入框
                        | FormCompValueImages (NonEmpty Text) -- ^ 最多9张图片
                        -- FIXME: 文档有提过可以用 media_id ，但按此逻辑实现的代码测试是图片是烂的
                        -- UPDATE: 当前最新文档明确只能使用文本URL
                        | FormCompValueDetails (NonEmpty FormCompValueNameValues)
                        | FormCompValueContact (NonEmpty UserId) -- ^ 内部联系人
                        | FormCompValueDay Day
                        | FormCompValueDayRange Day Day
                        | FormCompValueInt Integer
                        | FormCompValueDouble Double
                        | FormCompValueCNY (Dense "CNY")
                        deriving (Show)

type FormCompValueNameValues = Map Text FormComponentValue

instance ToJSON FormComponentValue where
  toJSON (FormCompValueText t)            = toJSON t
  toJSON (FormCompValueImages urls)       = toJSON $ A.encodeToLazyText $ toList urls
  toJSON (FormCompValueDetails kvs_list)  = toJSON $ A.encodeToLazyText $ map formComponentNameValuesToJson $ toList kvs_list
  toJSON (FormCompValueContact contacts)  = toJSON $ A.encodeToLazyText $ map toParamValue $ toList contacts
  toJSON (FormCompValueDay day)           = toJSON $ formatTime defaultTimeLocale "%Y-%m-%d" day

  toJSON (FormCompValueDayRange d1 d2)    = let to_str = formatTime defaultTimeLocale "%Y-%m-%d"
                                             in toJSON $ A.encodeToLazyText [ to_str d1, to_str d2 ]

  toJSON (FormCompValueInt x)             = toJSON $ tshow x
  toJSON (FormCompValueDouble x)          = toJSON $ tshow x
  toJSON (FormCompValueCNY x)             = toJSON $ denseToDecimal defaultDecimalConf Round x


instance IsString FormComponentValue where
  fromString = FormCompValueText . fromString


class ToFormComponentValue a where
  toFormComponentValue ::  a -> FormComponentValue

instance ToFormComponentValue FormComponentValue where toFormComponentValue = id
instance ToFormComponentValue Text where toFormComponentValue = FormCompValueText
instance ToFormComponentValue String where toFormComponentValue = FormCompValueText . fromString
instance ToFormComponentValue (NonEmpty UserId) where toFormComponentValue = FormCompValueContact
instance ToFormComponentValue Day where toFormComponentValue = FormCompValueDay
instance ToFormComponentValue (Day, Day) where toFormComponentValue = uncurry FormCompValueDayRange
instance ToFormComponentValue (NonEmpty FormCompValueNameValues) where toFormComponentValue = FormCompValueDetails
instance ToFormComponentValue Int where toFormComponentValue = FormCompValueInt . fromIntegral
instance ToFormComponentValue Double where toFormComponentValue = FormCompValueDouble
instance ToFormComponentValue (Dense "CNY") where toFormComponentValue = FormCompValueCNY


formComponentNameValueToJson :: (Text, FormComponentValue) -> Value
formComponentNameValueToJson (k, v) = object [ "name" .= k, "value" .= v ]

formComponentNameValuesToJson :: Map Text FormComponentValue -> [Value]
formComponentNameValuesToJson = map formComponentNameValueToJson . mapToList


(@=) :: ToFormComponentValue a => Text -> a -> (Text, FormComponentValue)
infix 3 @=
(@=) n v = (n, toFormComponentValue v)

infix 3 @=!, @=?

(@=!) :: ToFormComponentValue a => Text -> a -> Maybe (Text, FormComponentValue)
(@=!) = (. Just) . (@=?)

(@=?) :: ToFormComponentValue a => Text -> Maybe a -> Maybe (Text, FormComponentValue)
(@=?) = fmap . (@=)


data ConcensusType = ConcensusAnd -- ^ 会签
                   | ConcensusOr  -- ^ 或签
                   deriving (Show, Eq, Ord, Enum, Bounded)

instance ToJSON ConcensusType where
  toJSON ConcensusAnd = toJSON $ asText "AND"
  toJSON ConcensusOr = toJSON $ asText "OR"


-- Don't export constructor
data Approvers = ApproverSingle UserId
               | ApproverMulti ConcensusType (NES.NESet UserId)

instance ToJSON Approvers where
  toJSON (ApproverSingle uid)   = object [ "user_ids" .= [ uid ], "task_action_type" .= asText "NONE" ]
  toJSON (ApproverMulti t uids) = object [ "user_ids" .= NES.toList uids, "task_action_type" .= t ]


-- | Smart constructor
class ToApprovers a where
  toApprovers :: ConcensusType -> a -> Approvers


instance ToApprovers (NES.NESet UserId) where
  toApprovers t uids =
    if NES.size uids == 1
       then ApproverSingle (NES.findMin uids)
       else ApproverMulti t uids

instance ToApprovers (NES.NESet UserDetails) where
  toApprovers t = toApprovers t . NES.map userDetailsUserId



-- | 发起审批实例
oapiCreateProcessInstance :: HttpCallMonad env m
                          => Maybe AgentId  -- ^ 企业应用标识(ISV调用必须设置)
                          -> ProcessCode
                          -> UserId     -- ^ 发起人
                          -> DeptId     -- ^ 发起人所属部门. 不明白为什么要传这个钉钉系统本身应该知道的信息
                          -> Maybe (NonEmpty Approvers)  -- ^ 审批人
                          -> Maybe (NonEmpty UserId, CcTiming) -- ^ 抄送人
                          -> FormCompValueNameValues
                          -> OapiRpcWithAtk m ProcessInstanceId
-- {{{1
oapiCreateProcessInstance m_agent_id proc_code user_id dept_id m_approvers m_cc_info form_vals = do
  oapiPostCallWithAtk "/topapi/processinstance/create"
    []
    ( object $ catMaybes
        [ ("agent_id" .=) <$> m_agent_id
        , Just $ "process_code" .= proc_code
        , Just $ "originator_user_id" .= user_id
        , Just $ "dept_id" .= dept_id
        , ("approvers_v2" .=) <$> m_approvers
        , ("cc_list" .=) . intercalate "," . map toParamValue <$> m_cc_list
        , ("cc_position" .=) <$> m_cc_timing
        , Just $ "form_component_values" .= formComponentNameValuesToJson form_vals
        ]
    )
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "process_instance_id"))
  where m_cc_list = fmap (toList . fst) m_cc_info
        m_cc_timing = fmap snd m_cc_info
-- }}}1


data ProcessInstListResponse = ProcessInstListResponse
  { processInstListNextCursor :: Maybe Int
  , processInstListItems      :: [ProcessInstanceId]
  }

instance FromJSON ProcessInstListResponse where
  parseJSON = withObject "ProcessInstListResponse" $ \ o -> do
                o2 <- o .: "result"
                ProcessInstListResponse <$> o2 .:? "next_cursor"
                                        <*> o2 .: "list"


maxOapiGetProcessInstBatchSize :: Int
maxOapiGetProcessInstBatchSize = 10


-- | 批量获取审批实例id
oapiGetProcessInstanceIdList :: HttpCallMonad env m
                             => ProcessCode
                             -> Timestamp
                             -> Maybe Timestamp
                             -> Maybe (NonEmpty UserId)
                             -> Int
                             -> Int
                             -> OapiRpcWithAtk m ProcessInstListResponse
-- {{{1
oapiGetProcessInstanceIdList proc_code start_time m_end_time m_user_ids offset batch_size =
  oapiPostCallWithAtk "/topapi/processinstance/listids"
    []
    ( object $ catMaybes
        [ Just $ "process_code" .= proc_code
        , Just $ "start_time" .= start_time
        , ("end_time" .=) <$> m_end_time
        , ("userid_list" .=) . intercalate "," . map toParamValue <$> m_user_ids
        , Just $ "cursor" .= offset
        , Just $ "size" .= min maxOapiGetProcessInstBatchSize batch_size
        ]
    )
-- }}}1


oapiSourceProcessInstId :: HttpCallMonad env m
                        => ProcessCode
                        -> Timestamp
                        -> Maybe Timestamp
                        -> Maybe (NonEmpty UserId)
                        -> OapiRpcWithAtkSource m ProcessInstanceId
-- {{{1
oapiSourceProcessInstId proc_code start_time m_end_time m_user_ids = loop 0
  where size = maxOapiGetProcessInstBatchSize

        loop offset = do
          resp <- lift $ ExceptT $ oapiGetProcessInstanceIdList proc_code start_time m_end_time m_user_ids offset size
          mapM_ yield (processInstListItems resp)
          mapM_ loop (processInstListNextCursor resp)
-- }}}1


data ProcessInstStatus = ProcessInstNew
                       | ProcessInstRunning
                       | ProcessInstTerminated
                       | ProcessInstCompleted
                       deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1
instance ParamValue ProcessInstStatus where
  toParamValue ProcessInstNew        = "NEW"
  toParamValue ProcessInstRunning    = "RUNNING"
  toParamValue ProcessInstTerminated = "TERMINATED"
  toParamValue ProcessInstCompleted  = "COMPLETED"

instance ToJSON ProcessInstStatus where
  toJSON = toJSON . toParamValue

instance FromJSON ProcessInstStatus where
  parseJSON = parseJsonParamValueEnumBounded "ProcessInstStatus"
-- }}}1


data ProcessOpType = ProcessOpExecuteTaskNormal           -- ^ 正常执行任务
                   | ProcessOpExecuteTaskAgent            -- ^ 代理人执行任务
                   | ProcessOpAppendTaskBefore            -- ^ 前加签任务
                   | ProcessOpAppendTaskAfter             -- ^ 后加签任务
                   | ProcessOpRedirectTask                -- ^ 转交任务
                   | ProcessOpStartProcessInst            -- ^ 发起流程实例
                   | ProcessOpTerminateProcessInst        -- ^ 终止(撤销)流程实例
                   | ProcessOpFinishProcessInst           -- ^ 结束流程实例
                   | ProcessOpAddRemark                   -- ^ 添加评论
                   | ProcessOpCc                          -- ^ 不在文档中，运行时发现，应该是抄送之意
                   | ProcessOpTypeNone                        -- ^ 文档没有说明。看起来像是或签时取消未操作任务。
                   deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue ProcessOpType where
  toParamValue ProcessOpExecuteTaskNormal    = "EXECUTE_TASK_NORMAL"
  toParamValue ProcessOpExecuteTaskAgent     = "EXECUTE_TASK_AGENT"
  toParamValue ProcessOpAppendTaskBefore     = "APPEND_TASK_BEFORE"
  toParamValue ProcessOpAppendTaskAfter      = "APPEND_TASK_AFTER"
  toParamValue ProcessOpRedirectTask         = "REDIRECT_TASK"
  toParamValue ProcessOpStartProcessInst     = "START_PROCESS_INSTANCE"
  toParamValue ProcessOpTerminateProcessInst = "TERMINATE_PROCESS_INSTANCE"
  toParamValue ProcessOpFinishProcessInst    = "FINISH_PROCESS_INSTANCE"
  toParamValue ProcessOpAddRemark            = "ADD_REMARK"
  toParamValue ProcessOpCc                   = "PROCESS_CC"
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
-- }}}1


data FormComponentInput = FormComponentInput
  { formComponentInputName :: Text
  , formComponentInputValue :: Text
  , formComponentInputExtValue :: Maybe Value
  }

instance FromJSON FormComponentInput where
  parseJSON = withObject "FormComponentInput" $ \ o -> do
                FormComponentInput <$> o .: "name"
                                   <*> o .: "value"
                                   <*> o .:? "ext_value"

instance ToJSON FormComponentInput where
  toJSON (FormComponentInput {..}) =
    object [ "name" .= formComponentInputName
           , "value" .= formComponentInputValue
           , "ext_value" .= formComponentInputExtValue
           ]

data ProcessInstInfo = ProcessInstInfo
  { processInstInfoTitle                  :: Text
  , processInstInfoCreateTime             :: LocalTime
  , processInstInfoFinishTime             :: Maybe LocalTime
  , processInstInfoOriginatorUserId       :: UserId
  , processInstInfoOriginatorDeptId       :: DeptId
  , processInstInfoStatus                 :: ProcessInstStatus
  , processInstInfoApproverUserIds        :: [UserId]
  , processInstInfoCcUserIds              :: [UserId]
  , processInstInfoFormComponentKeyValues :: [FormComponentInput]
  , processInstInfoResult                 :: Maybe ProcessInstResult
  , processInstInfoBizId                  :: ProcessBizId
  , processInstInfoOpRecords              :: [ProcessOpRecord]
  , processInstInfoTasks                  :: [ProcessTaskInfo]
  , processInstInfoOriginatorDeptName     :: Text
  , processInstInfoBizAction              :: ProcessBizAction
  , processInstInfoAttachedProcessInstIds :: [ProcessInstanceId]
  }

-- {{{1 instances
instance FromJSON ProcessInstInfo where
  parseJSON = withObject "ProcessInstInfo" $ \ o0 -> do
                o <- o0 .: "process_instance"
                ProcessInstInfo <$> o .: "title"
                                <*> o .: "create_time"
                                <*> o .:? "finish_time"
                                <*> o .: "originator_userid"
                                <*> o .: "originator_dept_id"
                                <*> o .: "status"
                                <*> ((o .:? "approver_userids" >>= mapM (aesonParseSepTextOrList "," (return . UserId))) .!= [])
                                <*> ((o .:? "cc_userids" >>= mapM (aesonParseSepTextOrList "," (return . UserId))) .!= [])
                                <*> o .: "form_component_values"
                                <*> ( o .:? "result"
                                      >>= nullTextAsNothing
                                      >>= mapM (parseJsonParamValueEnumBounded "ProcessInstResult" . toJSON)
                                    )
                                <*> o .: "business_id"
                                <*> o .:? "operation_records" .!= []
                                <*> o .:? "tasks" .!= []
                                <*> o .: "originator_dept_name"
                                <*> o .: "biz_action"
                                <*> o .:? "attached_process_instance_ids" .!= []
-- }}}1


processInstInfoFormLookup :: Text -> ProcessInstInfo -> Maybe FormComponentInput
processInstInfoFormLookup n =
  find ((== n) . formComponentInputName) . processInstInfoFormComponentKeyValues


-- | 获取单个审批实例
oapiGetProcessInstanceInfo :: HttpCallMonad env m
                           => ProcessInstanceId
                           -> OapiRpcWithAtk m ProcessInstInfo
-- {{{1
oapiGetProcessInstanceInfo procss_id =
  oapiPostCallWithAtk "/topapi/processinstance/get"
    []
    ( object
        [ "process_instance_id" .= procss_id
        ]
    )
-- }}}1


-- | 获取用户待审批数量
oapiGetUserProcessInstanceToDo :: HttpCallMonad env m
                               => UserId
                               -> OapiRpcWithAtk m Int
-- {{{1
oapiGetUserProcessInstanceToDo user_id =
  oapiPostCallWithAtk "/topapi/process/gettodonum"
    []
    ( object
        [ "userid" .= user_id
        ]
    )
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "count"))
-- }}}1


-- vim: set foldmethod=marker:
