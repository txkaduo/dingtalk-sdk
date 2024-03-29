module DingTalk.VxAPI.Process where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Logger
import           Control.Monad.Except hiding (mapM_, mapM)
import           Data.Aeson           as A
import           Data.Aeson.TH                 (deriveJSON)
import           Data.Conduit
import           Data.List.NonEmpty   (NonEmpty(..))
-- import           Data.Proxy
import qualified Data.Text            as T
import           Data.Time
import           Data.Time.Clock.POSIX
-- import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))

import DingTalk.Types
import DingTalk.Helpers
import DingTalk.VxAPI.Basic
-- }}}1


-- | 获取指定用户可见的审批表单列表 报文关键内容
-- 跟旧版一样，但 json 字段格式不同
data VxProcessInfo = VxProcessInfo
  { vxProcessInfoName        :: Text
  , vxProcessInfoProcessCode :: ProcessCode
  , vxProcessInfoIconUrl     :: Text
  , vxProcessInfoUrl         :: Text
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 13 }) ''VxProcessInfo)


-- | 获取指定用户可见的审批表单列表 报文
data VxUserVisibleProcessResponse = VxUserVisibleProcessResponse
  { vxUserVisibleProcessNextToken   :: Maybe Int
  , vxUserVisibleProcessProcessList :: [VxProcessInfo]
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 20 }) ''VxUserVisibleProcessResponse)


-- | 获取当前企业所有可管理的表单的报文关键内容，相比 "获取指定用户可见的审批表单列表" 包含更多字段
data VxProcessInfoEx = VxProcessInfoEx
  { vxProcessInfoExIconName       :: Text
  , vxProcessInfoExFlowTitle      :: Text
  , vxProcessInfoExProcessCode    :: ProcessCode
  , vxProcessInfoExNewProcess     :: Bool
  , vxProcessInfoExIconUrl        :: Text
  , vxProcessInfoExAttendanceType :: Int
  , vxProcessInfoExGmtModified    :: ZonedTime
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 15 }) ''VxProcessInfoEx)


-- | 获取指定用户可见的审批表单列表，分页版本
apiVxGetProcessListByUser :: HttpCallMonad env m
                          => Maybe UserId
                          -> Maybe Int
                          -> Int
                          -> ApiVxRpcWithAtk m VxUserVisibleProcessResponse
apiVxGetProcessListByUser m_user_id m_next_token batch_size =
  apiVxGetCallInResult "v1.0" "/workflow/processes/userVisibilities/templates"
    (catMaybes
        [ "userId" &?= m_user_id
        , "nextToken" &!= fromMaybe 0 m_next_token
        , "maxResults" &!= min maxApiVxUserVisibleProcessBatchSize batch_size
        ]
    )


maxApiVxUserVisibleProcessBatchSize :: Int
maxApiVxUserVisibleProcessBatchSize = 100


-- | 获取指定用户可见的审批表单列表
apiVxSourceProcessListByUser :: HttpCallMonad env m
                             => Float  -- ^ seconds. delay between iterations
                             -> Maybe UserId
                             -> ApiVxRpcWithAtkSource m VxProcessInfo
apiVxSourceProcessListByUser delay_sec m_user_id =
  apiVxSourceByStep
    vxUserVisibleProcessProcessList
    vxUserVisibleProcessNextToken
    delay_sec
    (\ m_next_token -> apiVxGetProcessListByUser m_user_id m_next_token maxApiVxUserVisibleProcessBatchSize)
  

-- | apiVxGetProcessInstanceIdList 一次最多取多长的时间区间
maxApiVxGetProcessInstIdListTimeSpanSeconds :: Num a => a
maxApiVxGetProcessInstIdListTimeSpanSeconds = 60 * 60 * 24 * maxApiVxGetProcessInstIdListTimeSpanDays

maxApiVxGetProcessInstIdListTimeSpanDays :: Num a => a
maxApiVxGetProcessInstIdListTimeSpanDays = 120

-- | apiVxGetProcessInstanceIdList 的 startTime 最早距离当前时间的天数
maxApiVxGetProcessInstIdListTimeMinStartTimeDays :: Num a => a
maxApiVxGetProcessInstIdListTimeMinStartTimeDays = 365

-- | apiVxGetProcessInstanceIdList 的 startTime 最早距离当前时间的秒数
maxApiVxGetProcessInstIdListTimeMinStartTimeSeconds :: Num a => a
maxApiVxGetProcessInstIdListTimeMinStartTimeSeconds = 60 * 60 * 24 * maxApiVxGetProcessInstIdListTimeMinStartTimeDays


-- | apiVxGetProcessInstanceIdList 用户列表参数数量最的值
maxApiVxGetProcessInstIdListMaxUserIds :: Num a => a
maxApiVxGetProcessInstIdListMaxUserIds = 10


-- | apiVxGetProcessInstanceIdList 一次最多取多少个结果
maxApiVxGetProcessInstIdListBatchSize :: Num a => a
maxApiVxGetProcessInstIdListBatchSize = 20


data VxProcessInstListResponse = VxProcessInstListResponse
  { vxProcessInstListNextToken :: Maybe Int
  , vxProcessInstListList      :: [ProcessInstanceId]
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 17 }) ''VxProcessInstListResponse)


-- | 获取审批实例ID列表, 分页版本
-- 新版增加了状态过滤参数
apiVxGetProcessInstanceIdList :: HttpCallMonad env m
                             => ProcessCode
                             -> Timestamp
                             -> Maybe Timestamp
                             -> Maybe (NonEmpty UserId)
                             -> Maybe ( NonEmpty ProcessInstStatus )
                             -> Maybe Int
                             -> Int
                             -> ApiVxRpcWithAtk m VxProcessInstListResponse
-- {{{1
apiVxGetProcessInstanceIdList proc_code start_time m_end_time m_user_ids m_status_list m_next_token batch_size = do
   -- 开始时间不能早于当前时间的 365 天之前
   -- 而且不满足此条件时的错误信息是 code=invalidEndTime, message="获取审批实例ID列表，审批实例结束时间不能小于开始时间"
   -- 有误导性
  now <- liftIO getCurrentTime
  let oldest_start_time = timestampFromUTCTime $ addUTCTime (negate maxApiVxGetProcessInstIdListTimeMinStartTimeSeconds) now
  when (start_time < oldest_start_time) $ do
    $logErrorS logSourceName $ "start_time must be >= " <> tshow oldest_start_time
    throwIO $ userError $ "DingTalk API: start_time is too old"

  apiVxPostCallInResult "v1.0" "/workflow/processes/instanceIds/query"
    []
    ( object $ catMaybes
        [ Just $ "processCode" .= proc_code
        , Just $ "startTime" .= start_time
        , ("endTime" .=) <$> m_end_time
        , ("userIds" .=) . map toParamValue <$> m_user_ids
        , ("statuses" .=) . map toParamValue <$> m_status_list
        , Just $ "nextToken" .= fromMaybe 0 m_next_token
        , Just $ "maxResults" .= min maxApiVxGetProcessInstIdListBatchSize batch_size
        ]
    )
-- }}}1


-- | 获取审批实例ID列表, conduit版本，不检查时间跨度
apiVxSourceProcessInstId' :: HttpCallMonad env m
                          => ProcessCode
                          -> Timestamp
                          -> Maybe Timestamp
                          -> Maybe (NonEmpty UserId)
                          -> Maybe ( NonEmpty ProcessInstStatus )
                          -> ApiVxRpcWithAtkSource m ProcessInstanceId
apiVxSourceProcessInstId' proc_code start_time m_end_time m_user_ids m_status_list = loop Nothing
  where size = maxApiVxGetProcessInstIdListBatchSize

        loop m_next_token = do
          resp <- lift $ ExceptT $ apiVxGetProcessInstanceIdList proc_code start_time m_end_time m_user_ids m_status_list m_next_token size
          mapM_ yield (vxProcessInstListList resp)
          mapM_ (loop . Just) (vxProcessInstListNextToken resp)


-- | 获取审批实例ID列表, conduit版本，自动切分时间区间至指定大小
apiVxSourceProcessInstId :: HttpCallMonad env m
                          => DiffTime
                          -- ^ 接口限制最大返回数量为 1,000,000 个审批实例ID，调用者要自行调整时间窗大小以保证总量不会超限制
                          -> ProcessCode
                          -> Timestamp
                          -> Maybe Timestamp
                          -> Maybe (NonEmpty UserId)
                          -> Maybe ( NonEmpty ProcessInstStatus )
                          -> ApiVxRpcWithAtkSource m ProcessInstanceId
apiVxSourceProcessInstId max_time_span0 proc_code start_time0 m_end_time0 m_user_ids m_status_list = do
  now <- liftIO $ timestampFromPOSIXTime <$> getPOSIXTime
  go start_time0 (fromMaybe now m_end_time0)
  where remote_call start_time end_time =
          apiVxSourceProcessInstId' proc_code start_time (Just end_time) m_user_ids m_status_list

        max_time_span = timestampFromPOSIXTime $ realToFrac $ min max_time_span0 maxApiVxGetProcessInstIdListTimeSpanSeconds

        go start_time end_time = when (end_time > start_time) $ do
          let next_start_time = start_time + max_time_span
          if next_start_time > end_time
             then remote_call start_time end_time
             else remote_call start_time next_start_time >> go next_start_time end_time



data VxProcessOpRecord = VxProcessOpRecord
  { vxProcessOpUserId    :: UserId
  , vxProcessOpDate      :: ZonedTime
  , vxProcessOpType      :: ProcessOpType
  , vxProcessOpResult    :: ProcessOpResult
  , vxProcessOpRemark    :: Maybe Text
  , vxProcessOpCcUserIds :: Maybe [UserId]
  -- TODO: 未实现字段
  -- , vxProcessOpAttachments :: [ ?? ]
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 11 }) ''VxProcessOpRecord)

instance HackFixZonedTime VxProcessOpRecord where
  hackFixZonedTime x = x { vxProcessOpDate = hackFixZonedTime (vxProcessOpDate x) }


-- | 这个结构对应 formComponentValues
-- 但文档里描述的很多字段都不必然出现. 这里只反映最本质需要的字段
-- 测试用例 ProcessInstanceId:
-- * 7-b_5LFhR82-z6S6ODl1bQ02641678867589
-- * 8FQgcy7dR7iHyn13cZuhmw02641677843347
-- * xy1bmw7rSA-yPAQYPD7fmg02641678688905
-- * VWeGUx3ZRz-Q8RXgRpa9cw02641677834132
-- * a7ReYUt_TN-qdXZ8dpz6rw02641679230801
data VxFormInput = VxFormInput
  { vxFormInputName          :: Maybe Text
  -- | 实测 id 字段不一定存在，也没什么用
  -- , vxFormInputId            :: Text
  -- ^ 似乎说明文字，无 'name' 字段
  , vxFormInputComponentType :: Maybe Text

  -- 有时只有 extValue，有时 extValue, value 都不出现，如 componentType=DDAttachment
  , vxFormInputValue         :: Maybe Text
  , vxFormInputExtValue      :: Maybe Value
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 11 }) ''VxFormInput)


data VxProcessTaskInfo = VxProcessTaskInfo
  { vxProcessTaskInfoTaskId            :: VxProcessTaskId
  , vxProcessTaskInfoUserId            :: UserId
  , vxProcessTaskInfoStatus            :: ProcessTaskStatus
  , vxProcessTaskInfoResult            :: ProcessTaskResult
  , vxProcessTaskInfoCreateTime        :: Maybe ZonedTime
  -- ^ 实测这有可能不出现．比如流程有两个环节时就会这样
  , vxProcessTaskInfoFinishTime        :: Maybe ZonedTime

  -- 这两个 url 看上去是一样的
  -- 而且其实不是完整url，只是一段 query string
  -- 如下例
  -- ?procInsId=31991348-cfd8-4a3b-aa29-0e28fbe01c97&taskId=66748124356&businessId=202012232101000537487
  , vxProcessTaskInfoMobileUrl         :: Text
  , vxProcessTaskInfoPcUrl             :: Text

  -- 文档说有，但实测并没有
  -- , vxProcessTaskInfoProcessInstanceId :: ProcessInstanceId

  , vxProcessTaskInfoActivityId        :: ProcessActivityId
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 17 }) ''VxProcessTaskInfo)

instance HackFixZonedTime VxProcessTaskInfo where
  hackFixZonedTime x = x { vxProcessTaskInfoCreateTime = hackFixZonedTime (vxProcessTaskInfoCreateTime x)
                         , vxProcessTaskInfoFinishTime = hackFixZonedTime (vxProcessTaskInfoFinishTime x)
                         }


vxProcessInstInfoFormLookup :: Text -> VxProcessInstInfo -> Maybe VxFormInput
vxProcessInstInfoFormLookup n =
  find ((== Just n) . vxFormInputName) . vxProcessInstInfoFormComponentValues

lookupVxProcessInstFormInputValue :: VxProcessInstInfo -> Text -> Maybe (Maybe Text)
lookupVxProcessInstFormInputValue pii name =
  fmap vxFormInputValue $ find ((== Just name) . vxFormInputName) (vxProcessInstInfoFormComponentValues pii)


-- | 旧版里时间是 LocalTime，现在变成 ZonedTime
data VxProcessInstInfo = VxProcessInstInfo
  { vxProcessInstInfoTitle                      :: Text
  , vxProcessInstInfoCreateTime                 :: ZonedTime
  , vxProcessInstInfoFinishTime                 :: Maybe ZonedTime
  , vxProcessInstInfoOriginatorUserId           :: UserId
  , vxProcessInstInfoOriginatorDeptId           :: DeptId
  , vxProcessInstInfoStatus                     :: ProcessInstStatus
  , vxProcessInstInfoApproverUserIds            :: Maybe [UserId]
  , vxProcessInstInfoCcUserIds                  :: Maybe [UserId]
  , vxProcessInstInfoFormComponentValues        :: [VxFormInput]
  , vxProcessInstInfoResult                     :: Maybe ProcessInstResult
  , vxProcessInstInfoBusinessId                 :: ProcessBizId
  , vxProcessInstInfoOperationRecords           :: [VxProcessOpRecord]
  , vxProcessInstInfoTasks                      :: [VxProcessTaskInfo]
  , vxProcessInstInfoOriginatorDeptName         :: Text
  , vxProcessInstInfoBizAction                  :: ProcessBizAction

  -- 文档说有，但实测并没有
  -- , vxProcessInstInfoMainProcessInstanceId      :: ProcessInstanceId

  , vxProcessInstInfoAttachedProcessInstanceIds :: [ ProcessInstanceId ]
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 17 }) ''VxProcessInstInfo)

instance HackFixZonedTime VxProcessInstInfo where
  hackFixZonedTime x = x { vxProcessInstInfoCreateTime = hackFixZonedTime (vxProcessInstInfoCreateTime x)
                         , vxProcessInstInfoFinishTime = hackFixZonedTime (vxProcessInstInfoFinishTime x)
                         }

-- | 审批通过的时间
vxProcessInstInfoApprovedTime :: VxProcessInstInfo -> Maybe ZonedTime
vxProcessInstInfoApprovedTime inst_info = do
  guard $ vxProcessInstInfoStatus inst_info == ProcessInstCompleted
  guard $ vxProcessInstInfoResult inst_info == Just ProcessApproved
  vxProcessInstInfoFinishTime inst_info


-- | XXX: VxProcessInstInfo 居然没有 ProcessInstanceId 的字段，跟旧版接口类似问题
-- 但task有个 url 字段，包含形如 ?procInsId=XXXX&taskId=XXXX&businessId=XXX 的字串
-- 可以从中提取 ProcessInstanceId
vxProcessInstInfoId :: VxProcessInstInfo -> ProcessInstanceId
vxProcessInstInfoId (VxProcessInstInfo {..}) =
  fromMaybe (error $ "cannot get procInstId from url: " <> unpack url) $ do
    let url1 = T.dropWhile (/= '?') url
    s1 <- T.stripPrefix "?procInsId=" url1
    let (pid, others) = T.breakOn "&" s1
    guard $ not $ null others
    pure $ ProcessInstanceId pid
  where task = fromMaybe (error "empty tasks in dingtalk process instance info") $ listToMaybe vxProcessInstInfoTasks
        url = vxProcessTaskInfoMobileUrl task


-- | 获取单个审批实例详情
apiVxGetProcessInstanceInfo :: HttpCallMonad env m
                            => ProcessInstanceId
                            -> ApiVxRpcWithAtk m VxProcessInstInfo
apiVxGetProcessInstanceInfo inst_id =
  apiVxGetCallInResult "v1.0" "/workflow/processInstances"
        [ "processInstanceId" &= inst_id
        ]


-- | 添加审批评论
-- TODO: 支持文件附件
apiVxProcessInstanceAddComment :: HttpCallMonad env m
                               => ProcessInstanceId
                               -> UserId
                               -> Text
                               -> [Text] -- ^ Photos URLs
                               -> ApiVxRpcWithAtk m Bool
apiVxProcessInstanceAddComment process_id user_id text photo_urls = do
  apiVxPostCallInResult "v1.0" "/workflow/processInstances/comments"
    []
    request_jv
    where
      file_jv = do
        guard $ not (null photo_urls)
        pure $ object [ "photos" .= photo_urls
                      ]

      request_jv = object $ catMaybes
          [ pure $ "processInstanceId" .= process_id
          , pure $ "commentUserId" .= user_id
          , pure $ "text" .= text
          , ("file" .=) <$> file_jv
          ]


-- vim: set foldmethod=marker:
