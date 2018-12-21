module DingTalk.OAPI.Attendance
  ( AttendCheckType(..), PunchSourceType(..), PunchTimeResult(..), PunchLocationResult(..)
  , AttendPunchDetails(..), oapiGetAttendPunchDetails
  , AttendPunchResult(..), oapiGetAttendPunchResults
  , oapiGetAttendPunchResultsMaxBatch, oapiSourceAttendPunchResults
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Except hiding (mapM_, mapM)
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.Conduit
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Proxy

import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


-- | 考勤类型
data AttendCheckType = AttendCheckOnDuty -- ^ 上班
                     | AttendCheckOffDuty  -- ^ 下班
                     deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue AttendCheckType where
  toParamValue AttendCheckOnDuty  = "OnDuty"
  toParamValue AttendCheckOffDuty = "OffDuty"

instance ToJSON AttendCheckType where toJSON = toJSON . toParamValue
instance FromJSON AttendCheckType where parseJSON = parseJsonParamValueEnumBounded "AttendCheckType"
-- }}}1


-- | 数据来源
data PunchSourceType = PST_ATM
                     | PST_IBeacon
                     | PST_DingATM
                     | PST_User
                     | PST_Boss
                     | PST_Approve
                     | PST_System
                     | PST_AutoCheck
                     deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue PunchSourceType where
  toParamValue PST_ATM       = "ATM"
  toParamValue PST_IBeacon   = "BEACON"
  toParamValue PST_DingATM   = "DING_ATM"
  toParamValue PST_User      = "USER"
  toParamValue PST_Boss      = "BOSS"
  toParamValue PST_Approve   = "APPROVE"
  toParamValue PST_System    = "SYSTEM"
  toParamValue PST_AutoCheck = "AUTO_CHECK"

instance ToJSON PunchSourceType where toJSON = toJSON . toParamValue
instance FromJSON PunchSourceType where parseJSON = parseJsonParamValueEnumBounded "PunchSourceType"
-- }}}1


-- | 时间结果
data PunchTimeResult = PunchTimeNormal
                     | PunchTimeEarly
                     | PunchTimeLate
                     | PunchTimeSeriousLate
                     | PunchTimeAbsent
                     | PunchTimeNotSigned
                     deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue PunchTimeResult where
  toParamValue PunchTimeNormal      = "Normal"
  toParamValue PunchTimeEarly       = "Early"
  toParamValue PunchTimeLate        = "Late"
  toParamValue PunchTimeSeriousLate = "SeriousLate"
  toParamValue PunchTimeAbsent      = "Absenteeism"
  toParamValue PunchTimeNotSigned   = "NotSigned"

instance ToJSON PunchTimeResult where toJSON = toJSON . toParamValue
instance FromJSON PunchTimeResult where parseJSON = parseJsonParamValueEnumBounded "PunchTimeResult"
-- }}}1


data PunchLocationResult = PunchLocationNormal
                         | PunchLocationOutside
                         | PunchLocationNotSigned
                         deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue PunchLocationResult where
  toParamValue PunchLocationNormal    = "Normal"
  toParamValue PunchLocationOutside   = "Outside"
  toParamValue PunchLocationNotSigned = "NotSigned"

instance ToJSON PunchLocationResult where toJSON = toJSON . toParamValue
instance FromJSON PunchLocationResult where
  parseJSON = parseJsonParamValueEnumBounded "PunchLocationResult"
-- }}}1


-- | 打卡详情
data AttendPunchDetails = AttendPunchDetails
  { attendPunchDetailsId              :: AttendPunchDetailsId
  , attendPunchDetailsCorpId          :: CorpId
  , attendPunchDetailsGmtCreatedTime  :: Timestamp
  , attendPunchDetailsGmtModifiedTime :: Timestamp
  , attendPunchDetailsGroupId         :: Maybe AttendGroupId
  , attendPunchDetailsPlanId          :: Maybe AttendPlanId
  , attendPunchDetailsWorkDate        :: Timestamp
  , attendPunchDetailsUserId          :: UserId
  , attendPunchDetailsCheckType       :: AttendCheckType
  , attendPunchDetailsSourceType      :: PunchSourceType
  , attendPunchDetailsTimeResult      :: PunchTimeResult
  , attendPunchDetailsLocationResult  :: PunchLocationResult
  -- , attendPunchDetailsProcessCode :: Maybe ProcessCode
  -- 文档写的是 关联的审批id，当该字段非空时，表示打卡记录与请假、加班等审批有关
  -- 但审批接口并没有id这个概念，只有 ProcessCode
  -- 而且有了下面的 ProcessInstanceId 这个字段也算是冗余信息了
  , attendPunchDetailsProcInstId      :: Maybe ProcessInstanceId
  , attendPunchDetailsBaseCheckTime   :: Timestamp
  -- ^ 计算迟到和早退，基准时间；也可作为排班打卡时间
  , attendPunchDetailsUserCheckTime   :: Timestamp
  , attendPunchDetailsClassId         :: Maybe AttendClassId
  , attendPunchDetailsIsValid         :: Bool
  , attendPunchDetailsLocationMethod  :: Text
  , attendPunchDetailsDeviceId        :: Text
  , attendPunchDetailsUserAddress     :: Maybe Text
  , attendPunchDetailsUserLongitude   :: Double
  , attendPunchDetailsUserLatitude    :: Double
  , attendPunchDetailsUserAccuracy    :: Double
  , attendPunchDetailsUserSsid        :: Maybe Text
  , attendPunchDetailsUserMacAddr     :: Maybe Text
  , attendPunchDetailsBaseAddress     :: Maybe Text
  , attendPunchDetailsBaseLongitude   :: Maybe Double
  , attendPunchDetailsBaseLatitude    :: Maybe Double
  , attendPunchDetailsBaseAccuracy    :: Maybe Double
  , attendPunchDetailsBaseSsid        :: Maybe Text
  , attendPunchDetailsBaseMacAddr     :: Maybe Text
  , attendPunchDetailsPlanCheckTime   :: Timestamp
  , attendPunchDetailsOutsiteRemark   :: Maybe Text
  }

-- {{{1 instances
instance FromJSON AttendPunchDetails where
  parseJSON = withObject "AttendPunchDetails" $ \ o -> do
                AttendPunchDetails <$> o .: "id"
                                   <*> o .: "corpId"
                                   <*> o .: "gmtCreate"
                                   <*> o .: "gmtModified"
                                   <*> o .: "groupId"
                                   <*> o .: "planId"
                                   <*> o .: "workDate"
                                   <*> o .: "userId"
                                   <*> o .: "checkType"
                                   <*> o .: "sourceType"
                                   <*> o .: "timeResult"
                                   <*> o .: "locationResult"
                                   <*> o .:? "procInstId"
                                   <*> o .: "baseCheckTime"
                                   <*> o .: "userCheckTime"
                                   <*> o .:? "classId"
                                   <*> (o .: "isLegal" >>= txt_bool)
                                   <*> o .: "locationMethod"
                                   <*> o .: "deviceId"
                                   <*> o .:? "userAddress"
                                   <*> o .: "userLongitude"
                                   <*> o .: "userLatitude"
                                   <*> o .: "userAccuracy"
                                   <*> o .:? "userSsid"
                                   <*> o .:? "userMacAddr"
                                   <*> o .:? "baseAddress"
                                   <*> o .:? "baseLongitude"
                                   <*> o .:? "baseLatitude"
                                   <*> o .:? "baseAccuracy"
                                   <*> o .:? "baseSsid"
                                   <*> o .:? "baseMacAddr"
                                   <*> o .: "planCheckTime"
                                   <*> o .:? "outsideRemark"
                               where txt_bool "Y" = pure True
                                     txt_bool "N" = pure False
                                     txt_bool t = fail $ "unknown bool string: " <> unpack (asText t)

instance ToJSON AttendPunchDetails where
  toJSON (AttendPunchDetails {..}) =
    object $ catMaybes
      [ Just $ "id" .= attendPunchDetailsId
      , Just $ "corpId" .= attendPunchDetailsCorpId
      , Just $ "gmtCreate" .= attendPunchDetailsGmtCreatedTime
      , Just $ "gmtModified" .= attendPunchDetailsGmtModifiedTime
      , Just $ "groupId" .= attendPunchDetailsGroupId
      , Just $ "planId" .= attendPunchDetailsPlanId
      , Just $ "workDate" .= attendPunchDetailsWorkDate
      , Just $ "userId" .= attendPunchDetailsUserId
      , Just $ "checkType" .= attendPunchDetailsCheckType
      , Just $ "sourceType" .= attendPunchDetailsSourceType
      , Just $ "timeResult" .= attendPunchDetailsTimeResult
      , Just $ "locationResult" .= attendPunchDetailsLocationResult
      , ("procInstId" .=) <$> attendPunchDetailsProcInstId
      , Just $ "baseCheckTime" .= attendPunchDetailsBaseCheckTime
      , Just $ "userCheckTime" .= attendPunchDetailsUserCheckTime
      , ("classId" .=) <$> attendPunchDetailsClassId
      , Just $ "isLegal" .= asText (bool "N" "Y" attendPunchDetailsIsValid)
      , Just $ "locationMethod" .= attendPunchDetailsLocationMethod
      , Just $ "deviceId" .= attendPunchDetailsDeviceId
      , ("userAddress" .=) <$> attendPunchDetailsUserAddress
      , Just $ "userLongitude" .= attendPunchDetailsUserLongitude
      , Just $ "userLatitude" .= attendPunchDetailsUserLatitude
      , Just $ "userAccuracy" .= attendPunchDetailsUserAccuracy
      , ("userSsid" .=) <$> attendPunchDetailsUserSsid
      , ("userMacAddr" .=) <$> attendPunchDetailsUserMacAddr
      , ("baseAddress" .=) <$> attendPunchDetailsBaseAddress
      , ("baseLongitude" .=) <$> attendPunchDetailsBaseLongitude
      , ("baseLatitude" .=) <$> attendPunchDetailsBaseLatitude
      , ("baseAccuracy" .=) <$> attendPunchDetailsBaseAccuracy
      , ("baseSsid" .=) <$> attendPunchDetailsBaseSsid
      , ("baseMacAddr" .=) <$> attendPunchDetailsBaseMacAddr
      , Just $ "planCheckTime" .= attendPunchDetailsPlanCheckTime
      , ("outsideRemark" .=) <$> attendPunchDetailsOutsiteRemark
      ]
-- }}}1


-- | 获取打卡详情
oapiGetAttendPunchDetails :: HttpCallMonad env m
                          => Maybe Bool
                          -> NonEmpty UserId
                          -> (Day, Day)
                          -> OapiRpcWithAtk m [AttendPunchDetails]
-- {{{1
oapiGetAttendPunchDetails m_if_i18n user_ids (begin_day0, end_day0) =
  oapiPostCallWithAtk "/attendance/listRecord"
    []
    ( object $ catMaybes
        [ Just $ "userIds" .= user_ids
        , Just $ "checkDateFrom" .= day_str begin_day
        , Just $ "checkDateTo" .= day_str end_day
        , ("isI18n" .=) <$> m_if_i18n
        ]
    )
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "recordresult"))
  where (begin_day, end_day) = (uncurry min &&& uncurry max) (begin_day0, end_day0)
        day_str = formatTime defaultTimeLocale "%Y-%m-%d 00:00:00"
-- }}}1


-- | 打卡结果
data AttendPunchResult = AttendPunchResult
  { attendPunchResId             :: AttendPunchResId
  -- , attendPunchResDetailsId      :: AttendPunchDetailsId
  , attendPunchResCorpId         :: CorpId
  , attendPunchResGroupId        :: AttendGroupId
  , attendPunchResPlanId         :: AttendPlanId
  , attendPunchResWorkDate       :: Timestamp
  , attendPunchResUserId         :: UserId
  , attendPunchResCheckType      :: AttendCheckType
  , attendPunchResTimeResult     :: PunchTimeResult
  , attendPunchResLocationResult :: PunchLocationResult
  , attendPunchResProcInstId     :: Maybe ProcessInstanceId
  , attendPunchResBaseCheckTime  :: Timestamp
  , attendPunchResUserCheckTime  :: Timestamp
  , attendPunchResSourceType     :: PunchSourceType
  }

-- {{{1 instances
instance FromJSON AttendPunchResult where
  parseJSON = withObject "AttendPunchResult" $ \ o -> do
                AttendPunchResult <$> o .: "id"
                                  -- <*> o .: "recordId"
                                  <*> o .: "corpId"
                                  <*> o .: "groupId"
                                  <*> o .: "planId"
                                  <*> o .: "workDate"
                                  <*> o .: "userId"
                                  <*> o .: "checkType"
                                  <*> o .: "timeResult"
                                  <*> o .: "locationResult"
                                  <*> o .:? "procInstId"
                                  <*> o .: "baseCheckTime"
                                  <*> o .: "userCheckTime"
                                  <*> o .: "sourceType"

instance ToJSON AttendPunchResult where
  toJSON (AttendPunchResult {..}) =
    object $ catMaybes
      [ Just $ "id" .= attendPunchResId
      -- , Just $ "recordId" .= attendPunchResDetailsId
      , Just $ "corpId" .= attendPunchResCorpId
      , Just $ "groupId" .= attendPunchResGroupId
      , Just $ "planId" .= attendPunchResPlanId
      , Just $ "workDate" .= attendPunchResWorkDate
      , Just $ "userId" .= attendPunchResUserId
      , Just $ "checkType" .= attendPunchResCheckType
      , Just $ "timeResult" .= attendPunchResTimeResult
      , Just $ "locationResult" .= attendPunchResLocationResult
      , ("procInstId" .=) <$> attendPunchResProcInstId
      , Just $ "baseCheckTime" .= attendPunchResBaseCheckTime
      , Just $ "userCheckTime" .= attendPunchResUserCheckTime
      , Just $ "sourceType" .= attendPunchResSourceType
      ]
-- }}}1


-- | 获取打卡结果
oapiGetAttendPunchResults :: HttpCallMonad env m
                          => Maybe Bool
                          -> NonEmpty UserId
                          -> (Day, Day)
                          -> Int  -- ^ limit
                          -> Int -- ^ offset
                          -> OapiRpcWithAtk m ([AttendPunchResult], Bool)
-- {{{1
oapiGetAttendPunchResults m_if_i18n user_ids (begin_day0, end_day0) limit offset = runExceptT $ do
  jv <- ExceptT $ oapiPostCallWithAtk "/attendance/list"
          []
          ( object $ catMaybes
              [ Just $ "userIdList" .= user_ids
              , Just $ "workDateFrom" .= day_str begin_day
              , Just $ "workDateTo" .= day_str end_day
              , Just $ "offset" .= offset
              , Just $ "limit" .= limit
              , ("isI18n" .=) <$> m_if_i18n
              ]
          )

  (,) <$> flip getJsonField "recordresult" jv <*> flip getJsonField "hasMore" jv
  where (begin_day, end_day) = (uncurry min &&& uncurry max) (begin_day0, end_day0)
        day_str = formatTime defaultTimeLocale "%Y-%m-%d 00:00:00"
-- }}}1


oapiGetAttendPunchResultsMaxBatch :: Int
oapiGetAttendPunchResultsMaxBatch = 50


oapiSourceAttendPunchResults :: HttpCallMonad env m
                             => Maybe Bool
                             -> NonEmpty UserId
                             -> (Day, Day)
                             -> OapiRpcWithAtkSource m AttendPunchResult
-- {{{1
oapiSourceAttendPunchResults m_if_i18n user_ids (begin_day, end_day) = loop 0
  where limit = oapiGetAttendPunchResultsMaxBatch
        loop offset = do
          (list, has_more) <- lift $ ExceptT $ oapiGetAttendPunchResults m_if_i18n user_ids (begin_day, end_day) limit offset
          mapM_ yield list
          when has_more $ loop (offset + limit)
-- }}}1

-- vim: set foldmethod=marker:
