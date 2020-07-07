module DingTalk.OAPI.Report
  ( oapiGetVisibleReportTemplates, ReportTemplateInfo(..), ReportTemplatesResult(..), ReportTemplatesResultWrap(..)
  , oapiGetVisibileReportTemplatesMaxBatchSize
  , oapiSourceVisibleReportTemplates
  , ReportContentEntry(..), ReportInfo(..), GetReportInfoResult(..), GetReportInfoResultWrap(..)
  , oapiGetReportsMaxBatchSize, oapiGetReports, oapiSourceReports
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Except hiding (mapM_)
import           Data.Aeson           as A
import           Data.Aeson.TH
import           Data.Conduit

import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


data ReportTemplateInfo = ReportTemplateInfo
  { rtiName       :: Text
  , rtiReportCode :: ReportCode
  , rtiIconUrl    :: Text
  , rtiUrl        :: Text
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }) ''ReportTemplateInfo)

data ReportTemplatesResult = ReportTemplatesResult
  { rtrTemplateList :: [ReportTemplateInfo]
  , rtrNextCursor   :: Maybe Int
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }) ''ReportTemplatesResult)

data ReportTemplatesResultWrap = ReportTemplatesResultWrap
  { rtrwResult :: ReportTemplatesResult
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }) ''ReportTemplatesResultWrap)


-- | 获取用户可见的日志模板
oapiGetVisibleReportTemplates :: HttpCallMonad env m
                              => Maybe Int
                              -> Int
                              -> Maybe UserId
                              -> OapiRpcWithAtk m ReportTemplatesResultWrap
oapiGetVisibleReportTemplates m_offset batch_size m_user_id = do
  oapiGetCallWithAtk "/topapi/report/template/listbyuserid"
    ( catMaybes
      [ "userid" &?= m_user_id
      , Just $ "offset" &= fromMaybe 0 m_offset
      , Just $ "size" &= min oapiGetVisibileReportTemplatesMaxBatchSize batch_size
      ]
    )

oapiGetVisibileReportTemplatesMaxBatchSize :: Int
oapiGetVisibileReportTemplatesMaxBatchSize = 100

oapiSourceVisibleReportTemplates :: HttpCallMonad env m
                                 => Maybe UserId
                                 -> OapiRpcWithAtkSource m ReportTemplateInfo
-- {{{1
oapiSourceVisibleReportTemplates m_user_id = src Nothing
  where
    size = oapiGetVisibileReportTemplatesMaxBatchSize

    src m_offset = do
      ReportTemplatesResultWrap {..} <- lift $ ExceptT $ oapiGetVisibleReportTemplates m_offset size m_user_id
      let ReportTemplatesResult {..} = rtrwResult

      mapM_ yield rtrTemplateList

      case rtrNextCursor of
        Nothing -> pure ()
        Just new_offset -> src (Just new_offset)
-- }}}1


-- | 'contents' 字段里的条目
-- XXX: 暂时不进一步分辨具体的字段类型，全部只当作文本处理
data ReportContentEntry = ReportContentEntry
  { rceKey   :: Text
  , rceValue :: Text
  , rceSort  :: Text
  , rceType  :: Text
  }
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }) ''ReportContentEntry)

data ReportInfo = ReportInfo
  { riReportId     :: ReportId
  , riCreatorName  :: Text
  , riCreatorId    :: UserId
  , riCreateTime   :: Timestamp
  , riModifiedTime :: Timestamp
  -- 测试时似乎有时候会无这个字段
  -- , riDeptName     :: Text
  , riTemplateName :: Text
  , riRemark       :: Text
  , riContents     :: [ReportContentEntry]
  }
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }) ''ReportInfo)

data GetReportInfoResult = GetReportInfoResult
  { grirDataList   :: [ReportInfo]
  , grirSize       :: Int
  , grirNextCursor :: Maybe Int
  , grirHasMore    :: Bool
  }
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }) ''GetReportInfoResult)

data GetReportInfoResultWrap = GetReportInfoResultWrap
  { grirwResult   :: GetReportInfoResult
  }
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }) ''GetReportInfoResultWrap)


oapiGetReportsMaxBatchSize :: Int
oapiGetReportsMaxBatchSize = 20

oapiGetReports :: HttpCallMonad env m
               => (Timestamp, Timestamp)  -- ^ (start_time, end_time)
               -> Maybe Int -- ^ offset, cursor
               -> Int -- ^ batch size
               -> Maybe UserId
               -> Maybe Text  -- ^ template name
               -> OapiRpcWithAtk m GetReportInfoResultWrap
-- {{{1
oapiGetReports (start_time, end_time) m_offset batch_size m_user_id m_template_name = do
  oapiGetCallWithAtk "/topapi/report/list"
    ( catMaybes
      [ "userid" &?= m_user_id
      , "template_name" &?= m_template_name
      , Just $ "cursor" &= fromMaybe 0 m_offset
      , Just $ "size" &= min oapiGetReportsMaxBatchSize batch_size
      , Just $ "start_time" &= start_time
      , Just $ "end_time" &= end_time
      ]
    )
-- }}}1

oapiSourceReports :: HttpCallMonad env m
                  => (Timestamp, Timestamp)  -- ^ (start_time, end_time)
                  -> Maybe UserId
                  -> Maybe Text  -- ^ template name
                  -> OapiRpcWithAtkSource m ReportInfo
-- {{{1
oapiSourceReports time_range m_user_id m_template_name = src Nothing
  where
    size = oapiGetReportsMaxBatchSize

    src m_offset = do
      GetReportInfoResultWrap {..} <- lift $ ExceptT $ oapiGetReports time_range m_offset size m_user_id m_template_name
      let GetReportInfoResult {..} = grirwResult

      mapM_ yield grirDataList

      case (grirHasMore, grirNextCursor) of
        (True, Just new_offset) -> src (Just new_offset)
        _ -> pure ()
-- }}}1


-- vim: set foldmethod=marker:
