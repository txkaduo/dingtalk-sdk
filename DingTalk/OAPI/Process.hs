module DingTalk.OAPI.Process
  ( ProcessInfo(..), ProcessListResponse(..)
  , maxOapiGetProcessBatchSize
  , oapiGetProcessListByUser
  , oapiSourceProcessListByUser
  , CcTiming(..), FormComponentValue(..), FormCompValueNameValues
  , oapiCreateProcessInstance
  , maxOapiGetProcessInstBatchSize
  , ProcessInstListResponse(..)
  , oapiGetProcessInstanceIdList, oapiSourceProcessInstId
  , ProcessOpRecord(..), ProcessBizAction(..), ProcessTaskStatus(..), ProcessTaskResult(..), FormComponentInput(..)
  , ProcessTaskInfo(..), ProcessInstInfo(..)
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

import DingTalk.OAPI.Basic
import DingTalk.Helpers
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
oapiGetProcessListByUser :: HttpCallMonad env m
                         => Maybe UserId
                         -> Int
                         -> Int
                         -> ReaderT AccessToken m (Either OapiError ProcessListResponse)
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
                            => Maybe UserId
                            -> Source (ExceptT OapiError (ReaderT AccessToken m)) ProcessInfo
-- {{{1
oapiSourceProcessListByUser m_user_id = loop 0
  where size = maxOapiGetProcessBatchSize

        loop offset = do
          resp <- lift $ ExceptT $ oapiGetProcessListByUser m_user_id offset size
          mapM_ yield (processListItems resp)
          mapM_ loop (processListNextCursor resp)
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


data FormComponentValue = FormCompValueText Text    -- ^ 普通文字输入框
                        | FormCompValueImages (NonEmpty (Either Text MediaId)) -- ^ 最多9张图片
                        | FormCompValueDetails (NonEmpty FormCompValueNameValues)

type FormCompValueNameValues = Map Text FormComponentValue

instance ToJSON FormComponentValue where
  toJSON (FormCompValueText t)            = toJSON t
  toJSON (FormCompValueImages url_or_ids) = toJSON $ A.encodeToLazyText $ map (either id toParamValue) $ toList url_or_ids
  toJSON (FormCompValueDetails kvs_list)  = toJSON $ A.encodeToLazyText $ map formComponentNameValuesToJson $ toList kvs_list


formComponentNameValueToJson :: (Text, FormComponentValue) -> Value
formComponentNameValueToJson (k, v) = object [ "name" .= k, "value" .= v ]

formComponentNameValuesToJson :: Map Text FormComponentValue -> [Value]
formComponentNameValuesToJson = map formComponentNameValueToJson . mapToList

-- | 发起审批实例
oapiCreateProcessInstance :: HttpCallMonad env m
                          => Maybe AgentId  -- ^ 企业应用标识(ISV调用必须设置)
                          -> ProcessCode
                          -> UserId     -- ^ 发起人
                          -> DeptId     -- ^ 发起人所属部门. 不明白为什么要传这个钉钉系统本身应该知道的信息
                          -> NonEmpty UserId  -- ^ 审批人
                          -> Maybe (NonEmpty UserId, CcTiming) -- ^ 抄送人
                          -> FormCompValueNameValues
                          -> ReaderT AccessToken m (Either OapiError ProcessInstanceId)
-- {{{1
oapiCreateProcessInstance m_agent_id proc_code user_id dept_id approvers m_cc_info form_vals = do
  oapiPostCallWithAtk "/topapi/processinstance/create"
    []
    ( object $ catMaybes
        [ ("agent_id" .=) <$> m_agent_id
        , Just $ "process_code" .= proc_code
        , Just $ "originator_user_id" .= user_id
        , Just $ "dept_id" .= dept_id
        , Just $ ("approvers" .=) . intercalate "," . map toParamValue $ approvers
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
                             -> ReaderT AccessToken m (Either OapiError ProcessInstListResponse)
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
                        -> Source (ExceptT OapiError (ReaderT AccessToken m)) ProcessInstanceId
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
                      deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue ProcessTaskResult where
  toParamValue ProcessTaskAgreed     = "AGREE"
  toParamValue ProcessTaskRefused    = "REFUSE"
  toParamValue ProcessTaskRedirected = "REDIRECTED"

instance ToJSON ProcessTaskResult where toJSON = toJSON . toParamValue

instance FromJSON ProcessTaskResult where
  parseJSON = parseJsonParamValueEnumBounded "ProcessTaskResult"
-- }}}1


data ProcessTaskInfo = ProcessTaskInfo
  { processTaskInfoUserId     :: UserId
  , processTaskInfoStatus     :: ProcessTaskStatus
  , processTaskInfoResult     :: ProcessTaskResult
  , processTaskInfoCreateTime :: LocalTime
  , processTaskInfoFinishTime :: Maybe LocalTime
  , processTaskInfoId         :: ProcessTaskId
  }

-- {{{1 instances
instance FromJSON ProcessTaskInfo where
  parseJSON = withObject "ProcessTaskInfo" $ \ o -> do
                ProcessTaskInfo <$> o .: "userid"
                                <*> o .: "task_status"
                                <*> o .: "task_result"
                                <*> o .: "create_time"
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
  , processInstInfoResult                 :: ProcessInstResult
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
                                <*> (o .: "approver_userids" >>= aesonParseSepTextOrList "," (return . UserId))
                                <*> ((o .:? "cc_userids" >>= mapM (aesonParseSepTextOrList "," (return . UserId))) .!= [])
                                <*> o .: "form_component_values"
                                <*> o .: "result"
                                <*> o .: "business_id"
                                <*> o .:? "operation_records" .!= []
                                <*> o .:? "tasks" .!= []
                                <*> o .: "originator_dept_name"
                                <*> o .: "biz_action"
                                <*> o .:? "attached_process_instance_ids" .!= []
-- }}}1


-- | 获取单个审批实例
oapiGetProcessInstanceInfo :: HttpCallMonad env m
                           => ProcessInstanceId
                           -> ReaderT AccessToken m (Either OapiError ProcessInstInfo)
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
                               -> ReaderT AccessToken m (Either OapiError Int)
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