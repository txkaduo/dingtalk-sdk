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
  , FormCompDetailsRow(..), FormCompDetailsX(..)
  , ProcessTaskInfo(..), ProcessInstInfo(..)
  , processInstInfoId, processInstInfoApprovedTime
  , processInstInfoFormLookup, lookupProcessInstFormComponentInputValue
  , oapiGetProcessInstanceInfo, oapiGetProcessInstanceInfo'
  , oapiGetUserProcessInstanceToDo
  , mkInternalUrlOfProcessInst
  , oapiProcessInstanceAddComment
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Logger
import           Control.Monad.Except hiding (mapM_, mapM)
import           Data.Aeson           as A
import           Data.Aeson.Text      as A
import qualified Data.Aeson.Extra     as AE
import qualified Data.Text            as T
import           Data.Conduit
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Proxy
import           Data.Time
import qualified Data.Set.NonEmpty as NES
-- import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))
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

-- | apiVxGetProcessInstanceIdList 的 startTime 最早距离当前时间的秒数
maxOapiGetProcessInstIdListTimeMinStartTimeSeconds :: Num a => a
maxOapiGetProcessInstIdListTimeMinStartTimeSeconds = 60 * 60 * 24 * 365



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
maxOapiGetProcessInstBatchSize = 20


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
oapiGetProcessInstanceIdList proc_code start_time m_end_time m_user_ids offset batch_size = do
   -- 开始时间不能早于当前时间的 365 天之前
   -- 而且不满足此条件时的错误信息是 code=invalidEndTime, message="获取审批实例ID列表，审批实例结束时间不能小于开始时间"
   -- 有误导性
  now <- liftIO getCurrentTime
  let oldest_start_time = timestampFromUTCTime $ addUTCTime (negate maxOapiGetProcessInstIdListTimeMinStartTimeSeconds) now
  when (start_time < oldest_start_time) $ do
    $logErrorS logSourceName $ "start_time must be >= " <> tshow oldest_start_time
    throwIO $ userError $ "DingTalk API: start_time is too old"

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
  , processInstInfoMainProcessInstId      :: Maybe ProcessInstanceId
  }

-- {{{1 instances
-- XXX: process_instance 这一层其实是钉钉新版本接口统一的行为，在这里暂时理解为 ProcessInstInfo 一部分，以后要去掉
instance FromJSON ProcessInstInfo where
  parseJSON = withObject "ProcessInstInfo" $ \ o0 -> do
                o <- o0 .: "process_instance"
                ProcessInstInfo <$> o .: "title"
                                <*> o .: "create_time"
                                <*> o .:? "finish_time"
                                <*> o .: "originator_userid"  -- not originator_user_id
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
                                <*> o .:? "main_process_instance_id"

-- not needed for DingTalk API, but useful for serializing for cache
instance ToJSON ProcessInstInfo where
  toJSON (ProcessInstInfo {..}) = object [ "process_instance" .= o ]
    where
      o = object $ catMaybes $
        [ pure $ "title" .= processInstInfoTitle
        , pure $ "create_time" .= formatTime defaultTimeLocale "%F %T" processInstInfoCreateTime
        , ("finish_time" .=) . formatTime defaultTimeLocale "%F %T" <$> processInstInfoFinishTime
        , pure $ "originator_userid" .= processInstInfoOriginatorUserId
        , pure $ "originator_dept_id" .= processInstInfoOriginatorDeptId
        , pure $ "status" .= processInstInfoStatus
        , guard (not $ null processInstInfoApproverUserIds)
            >> pure ("approver_userids" .= processInstInfoApproverUserIds)
        , guard (not $ null processInstInfoCcUserIds)
            >> pure ("cc_userids" .= processInstInfoCcUserIds)
        , pure $ "form_component_values" .= processInstInfoFormComponentKeyValues
        , ("result" .=) <$> processInstInfoResult
        , pure $ "business_id" .= processInstInfoBizId
        , guard (not $ null processInstInfoOpRecords)
            >> pure ("operation_records" .= processInstInfoOpRecords)
        , guard (not $ null processInstInfoTasks)
            >> pure ("tasks" .= processInstInfoTasks)
        , pure $ "originator_dept_name" .= processInstInfoOriginatorDeptName
        , pure $ "biz_action" .= processInstInfoBizAction
        , guard (not $ null processInstInfoAttachedProcessInstIds)
            >> pure ("attached_process_instance_ids" .= processInstInfoAttachedProcessInstIds)
        , ("main_process_instance_id" .=) <$> processInstInfoMainProcessInstId
        ]
-- }}}1


-- | 审批通过的时间
processInstInfoApprovedTime :: ProcessInstInfo -> Maybe LocalTime
processInstInfoApprovedTime inst_info = do
  guard $ processInstInfoStatus inst_info == ProcessInstCompleted
  guard $ processInstInfoResult inst_info == Just ProcessApproved
  processInstInfoFinishTime inst_info


-- | XXX: ProcessInstInfo 居然没有 ProcessInstanceId 的字段
-- 但task有个 url 字段，包含形如 aflow.dingtalk.com?procInsId=XXXX&taskId=XXXX&businessId=XXX 的字串
-- 可以从中提取 ProcessInstanceId
processInstInfoId :: ProcessInstInfo -> ProcessInstanceId
processInstInfoId (ProcessInstInfo {..}) =
  fromMaybe (error $ "cannot get procInstId from url: " <> unpack url) $ do
    s1 <- T.stripPrefix "aflow.dingtalk.com?procInsId=" url
    let (pid, others) = T.breakOn "&" s1
    guard $ not $ null others
    pure $ ProcessInstanceId pid
  where task = fromMaybe (error "empty tasks in dingtalk process instance info") $ listToMaybe processInstInfoTasks
        url = processTaskInfoUrl task


processInstInfoFormLookup :: Text -> ProcessInstInfo -> Maybe FormComponentInput
processInstInfoFormLookup n =
  find ((== n) . formComponentInputName) . processInstInfoFormComponentKeyValues


lookupProcessInstFormComponentInputValue :: ProcessInstInfo -> Text -> Maybe Text
lookupProcessInstFormComponentInputValue pii name =
  fmap formComponentInputValue $ find ((== name) . formComponentInputName) (processInstInfoFormComponentKeyValues pii)


-- | 获取单个审批实例
oapiGetProcessInstanceInfo :: HttpCallMonad env m
                           => ProcessInstanceId
                           -> OapiRpcWithAtk m ProcessInstInfo
oapiGetProcessInstanceInfo process_id =
  oapiPostCallWithAtk "/topapi/processinstance/get"
    []
    ( object
        [ "process_instance_id" .= process_id
        ]
    )


-- | 为方便使用，把 ProcessInstanceId 打包到结果里
-- 尽量避免使用 processInstInfoId 这个 hacking
oapiGetProcessInstanceInfo' :: HttpCallMonad env m
                            => ProcessInstanceId
                            -> OapiRpcWithAtk m (ProcessInstanceId, ProcessInstInfo)
oapiGetProcessInstanceInfo' process_id = fmap (process_id, ) <$> oapiGetProcessInstanceInfo process_id


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


-- | XXX: UNDOCUMENTED
-- see: https://www.cnblogs.com/Alex-Mercer/p/12768319.html
-- 4.通过二维码或者网页查看流程（必须通过钉钉扫码）
--
-- 在钉钉的审批里面，我们打印的时候可以看到二维码，而里面的内容如下
--
-- https://m.tb.cn/E3.3MdqGv?corpid=企业ID&procInstId=审批实例id
--
-- 所以直接将链接转二维码或者直接发送给钉钉个人打开，就可以直接看到该审批实例的详细信息了
mkInternalUrlOfProcessInst :: CorpId -> ProcessInstanceId -> Text
mkInternalUrlOfProcessInst (CorpId cid) (ProcessInstanceId pid) = mconcat
  [ "https://m.tb.cn/E3.3MdqGv?"
  , "corpid=" <> cid
  , "&procInstId=" <> pid
  ]


-- | 添加审批评论
-- TODO: 支持文件附件
oapiProcessInstanceAddComment :: HttpCallMonad env m
                              => ProcessInstanceId
                              -> UserId
                              -> Text
                              -> [Text] -- ^ Photos URLs
                              -> OapiRpcWithAtk m Bool
oapiProcessInstanceAddComment process_id user_id text photo_urls = do
  oapiPostCallWithAtk "/topapi/process/instance/comment/add"
    []
    ( object [ "request" .= request_jv ] )
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "success"))
    where
      file_jv = do
        guard $ not (null photo_urls)
        pure $ object [ "photos" .= photo_urls
                      ]

      request_jv = object $ catMaybes
          [ pure $ "process_instance_id" .= process_id
          , pure $ "comment_userid" .= user_id
          , pure $ "text" .= text
          , ("file" .=) <$> file_jv
          ]

-- vim: set foldmethod=marker:
