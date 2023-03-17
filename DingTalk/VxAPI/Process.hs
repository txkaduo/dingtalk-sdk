module DingTalk.VxAPI.Process where

-- {{{1 imports
import           ClassyPrelude
-- import           Control.Monad.Logger
import           Control.Monad.Except hiding (mapM_, mapM)
import           Data.Aeson           as A
import           Data.Aeson.TH                 (deriveJSON)
#if !MIN_VERSION_aeson(1, 4, 7)
import           Data.Aeson.Types              (camelTo2)
#endif
import           Data.Conduit
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Proxy
import           Data.Time
import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))

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
  { axProcessInfoName    :: Text
  , axProcessInfoCode    :: ProcessCode
  , axProcessInfoIconUrl :: Text
  , axProcessInfoUrl     :: Text
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }) ''VxProcessInfo)


-- | 获取指定用户可见的审批表单列表 报文
data VxUserVisibleProcessResponse = VxUserVisibleProcessResponse
  { axUserVisibleProcessNextToken :: Maybe Int
  , axUserVisibleProcessItems     :: [VxProcessInfo]
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }) ''VxUserVisibleProcessResponse)


-- | 获取当前企业所有可管理的表单的报文关键内容，相比 "获取指定用户可见的审批表单列表" 包含更多字段
data VxProcessInfoEx = VxProcessInfoEx
  { axProcessInfoExIconName       :: Text
  , axProcessInfoExFlowTitle      :: Text
  , axProcessInfoExProcessCode    :: ProcessCode
  , axProcessInfoExNewProcess     :: Bool
  , axProcessInfoExIconUrl        :: Text
  , axProcessInfoExAttendanceType :: Int
  , axProcessInfoExGmtModified    :: UTCTime
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }) ''VxProcessInfoEx)


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
        , "nextToken" &?= m_next_token
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
          mapM_ yield (axUserVisibleProcessItems resp)
          mapM_ (\ x -> delay >> loop (Just x)) (axUserVisibleProcessNextToken resp)


-- | 审批实例状态
-- 基本跟旧版一样，但多了个 canceled
data VxProcessInstStatus = VxProcessInstNew
                         | VxProcessInstRunning
                         | VxProcessInstTerminated
                         | VxProcessInstCompleted
                         | VxProcessInstCanceled
                         -- | ProcessInstError -- ^ undocumented
                         deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1
instance ParamValue VxProcessInstStatus where
  toParamValue VxProcessInstNew        = "NEW"
  toParamValue VxProcessInstRunning    = "RUNNING"
  toParamValue VxProcessInstTerminated = "TERMINATED"
  toParamValue VxProcessInstCompleted  = "COMPLETED"
  toParamValue VxProcessInstCanceled   = "CANCELED"

instance ToJSON VxProcessInstStatus where
  toJSON = toJSON . toParamValue

instance FromJSON VxProcessInstStatus where
  parseJSON = parseJsonParamValueEnumBounded "VxProcessInstStatus"

instance PersistField VxProcessInstStatus where
  toPersistValue = toPersistValue . toParamValue

  fromPersistValue pv = do
    t <- fromPersistValue pv
    case parseEnumParamValueText t of
      Nothing -> Left $ "Invalid VxProcessInstStatus: " <> t
      Just s -> pure s

instance PersistFieldSql VxProcessInstStatus where
  sqlType _ = sqlType (Proxy :: Proxy Text)
-- }}}1


axProcessInstStatusIsFinished :: VxProcessInstStatus -> Bool
axProcessInstStatusIsFinished VxProcessInstNew        = False
axProcessInstStatusIsFinished VxProcessInstRunning    = False
axProcessInstStatusIsFinished VxProcessInstTerminated = True
axProcessInstStatusIsFinished VxProcessInstCompleted  = True
axProcessInstStatusIsFinished VxProcessInstCanceled   = True


axProcessInstStatusListFinished :: [ VxProcessInstStatus ]
axProcessInstStatusListFinished = filter axProcessInstStatusIsFinished [ minBound .. maxBound ]


maxApiVxGetProcessInstBatchSize :: Int
maxApiVxGetProcessInstBatchSize = 20


data VxProcessInstListResponse = VxProcessInstListResponse
  { axProcessInstListNextToken :: Maybe Int
  , axProcessInstListList      :: [ProcessInstanceId]
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 17 }) ''VxProcessInstListResponse)


-- | 获取审批实例ID列表, 分页版本
-- 新版增加了状态过滤参数
apiVxGetProcessInstanceIdList :: HttpCallMonad env m
                             => ProcessCode
                             -> Timestamp
                             -> Maybe Timestamp
                             -> Maybe (NonEmpty UserId)
                             -> Maybe ( NonEmpty VxProcessInstStatus )
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
        , ("nextToken" .=) <$> m_next_token
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
                         -> Maybe ( NonEmpty VxProcessInstStatus )
                         -> ApiVxRpcWithAtkSource m ProcessInstanceId
apiVxSourceProcessInstId proc_code start_time m_end_time m_user_ids m_status_list = loop Nothing
  where size = maxApiVxGetProcessInstBatchSize

        loop m_next_token = do
          resp <- lift $ ExceptT $ apiVxGetProcessInstanceIdList proc_code start_time m_end_time m_user_ids m_status_list m_next_token size
          mapM_ yield (axProcessInstListList resp)
          mapM_ (loop . Just) (axProcessInstListNextToken resp)



data VxProcessInstInfo = VxProcessInstInfo
  { axProcessInstInfoTitle                  :: Text
  , axProcessInstInfoCreateTime             :: LocalTime
  , axProcessInstInfoFinishTime             :: Maybe LocalTime
  , axProcessInstInfoOriginatorUserId       :: UserId
  , axProcessInstInfoOriginatorDeptId       :: DeptId
  , axProcessInstInfoStatus                 :: VxProcessInstStatus
  , axProcessInstInfoApproverUserIds        :: [UserId]
  , axProcessInstInfoCcUserIds              :: [UserId]
  , axProcessInstInfoFormComponentKeyValues :: [FormComponentInput]
  , axProcessInstInfoResult                 :: Maybe ProcessInstResult
  , axProcessInstInfoBizId                  :: ProcessBizId
  , axProcessInstInfoOpRecords              :: [ProcessOpRecord]
  , axProcessInstInfoTasks                  :: [ProcessTaskInfo]
  , axProcessInstInfoOriginatorDeptName     :: Text
  , axProcessInstInfoBizAction              :: ProcessBizAction
  , axProcessInstInfoAttachedProcessInstIds :: [ProcessInstanceId]
  , axProcessInstInfoMainProcessInstId      :: Maybe ProcessInstanceId
  }

-- | 获取单个审批实例详情

-- vim: set foldmethod=marker:
