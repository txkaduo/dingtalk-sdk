module DingTalk.VxAPI.Process where

-- {{{1 imports
import           ClassyPrelude
-- import           Control.Monad.Logger
import           Control.Monad.Except hiding (mapM_, mapM)
import           Data.Aeson           as A
import           Data.Aeson.TH                 (deriveJSON)
import           Data.Conduit
import           Data.List.NonEmpty   (NonEmpty(..))
-- import           Data.Proxy
import qualified Data.Text            as T
-- import           Data.Time
-- import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))

import DingTalk.Types
import DingTalk.Helpers
import DingTalk.VxAPI.Basic

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Concurrent (threadDelay)
#endif
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
  , vxProcessInfoExGmtModified    :: UTCTime
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
apiVxSourceProcessListByUser delay_sec m_user_id = loop Nothing
  where size = maxApiVxUserVisibleProcessBatchSize
        delay_us = round $ delay_sec * 1000 * 1000
        delay = liftIO $ threadDelay delay_us

        loop m_next_token = do
          resp <- lift $ ExceptT $ apiVxGetProcessListByUser m_user_id m_next_token size
          mapM_ yield (vxUserVisibleProcessProcessList resp)
          mapM_ (\ x -> delay >> loop (Just x)) (vxUserVisibleProcessNextToken resp)



maxApiVxGetProcessInstBatchSize :: Int
maxApiVxGetProcessInstBatchSize = 20


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
apiVxGetProcessInstanceIdList proc_code start_time m_end_time m_user_ids m_status_list m_next_token batch_size =
  apiVxPostCallInResult "v1.0" "/workflow/processes/instanceIds/query"
    []
    ( object $ catMaybes
        [ Just $ "processCode" .= proc_code
        , Just $ "startTime" .= start_time
        , ("endTime" .=) <$> m_end_time
        , ("userIds" .=) . map toParamValue <$> m_user_ids
        , ("statuses" .=) . map toParamValue <$> m_status_list
        , Just $ "nextToken" .= fromMaybe 0 m_next_token
        , Just $ "maxResults" .= min maxApiVxGetProcessInstBatchSize batch_size
        ]
    )
-- }}}1


-- | 获取审批实例ID列表, conduit版本
apiVxSourceProcessInstId :: HttpCallMonad env m
                         => ProcessCode
                         -> Timestamp
                         -> Maybe Timestamp
                         -> Maybe (NonEmpty UserId)
                         -> Maybe ( NonEmpty ProcessInstStatus )
                         -> ApiVxRpcWithAtkSource m ProcessInstanceId
apiVxSourceProcessInstId proc_code start_time m_end_time m_user_ids m_status_list = loop Nothing
  where size = maxApiVxGetProcessInstBatchSize

        loop m_next_token = do
          resp <- lift $ ExceptT $ apiVxGetProcessInstanceIdList proc_code start_time m_end_time m_user_ids m_status_list m_next_token size
          mapM_ yield (vxProcessInstListList resp)
          mapM_ (loop . Just) (vxProcessInstListNextToken resp)


data VxProcessOpRecord = VxProcessOpRecord
  { vxProcessOpUserId    :: UserId
  , vxProcessOpDate      :: UTCTime
  , vxProcessOpType      :: ProcessOpType
  , vxProcessOpResult    :: ProcessOpResult
  , vxProcessOpRemark    :: Maybe Text
  , vxProcessOpCcUserIds :: Maybe Text
  -- TODO: 未实现字段
  -- , vxProcessOpAttachments :: [ ?? ]
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 11 }) ''VxProcessOpRecord)


data VxProcessTaskInfo = VxProcessTaskInfo
  { vxProcessTaskInfoTaskId            :: VxProcessTaskId
  , vxProcessTaskInfoUserId            :: UserId
  , vxProcessTaskInfoStatus            :: ProcessTaskStatus
  , vxProcessTaskInfoResult            :: ProcessTaskResult
  , vxProcessTaskInfoCreateTime        :: Maybe UTCTime
  -- ^ 实测这有可能不出现．比如流程有两个环节时就会这样
  , vxProcessTaskInfoFinishTime        :: Maybe UTCTime

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


-- | 旧版里时间是 LocalTime，现在变成 UTCTime
data VxProcessInstInfo = VxProcessInstInfo
  { vxProcessInstInfoTitle                      :: Text
  , vxProcessInstInfoCreateTime                 :: UTCTime
  , vxProcessInstInfoFinishTime                 :: Maybe UTCTime
  , vxProcessInstInfoOriginatorUserId           :: UserId
  , vxProcessInstInfoOriginatorDeptId           :: DeptId
  , vxProcessInstInfoStatus                     :: ProcessInstStatus
  , vxProcessInstInfoApproverUserIds            :: [UserId]
  , vxProcessInstInfoCcUserIds                  :: [UserId]
  , vxProcessInstInfoFormComponentValues        :: [FormComponentInput]
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


-- | 审批通过的时间
vxProcessInstInfoApprovedTime :: VxProcessInstInfo -> Maybe UTCTime
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
    s1 <- T.stripPrefix "?procInsId=" url
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


-- vim: set foldmethod=marker:
