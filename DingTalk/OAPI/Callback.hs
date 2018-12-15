module DingTalk.OAPI.Callback
  ( CallbackEvent(..)
  , SomeCallbackEvent(..)
  , callbackEventEqByTag
  , allKnownCallbackEvents, isKnownCallbackTag
  , parseCallbackDataJson
  , GenericCallbackEvent(..)
  , CheckUrl(..)
  , UserChangeData(..), DeptChangeData(..)
  , UserAddOrg(..), UserModifyOrg(..), UserLeaveOrg(..)
  , LabelUserChange(..)
  , OrgDeptCreate(..), OrgDeptModify(..), OrgDeptRemove(..)
  , ProcessInstanceChangeData(..), ProcessInstanceChange(..)
  , ProcessTaskChangeData(..), ProcessTaskChange(..)
  , oapiRegisterCallback, oapiUpdateCallback, oapiRegisterOrUpdateCallback
  , oapiDeleteCallback
  , oapiGetCallbackFailedList, CallbackFailedItem(..), CallbackGetFailedResponse(..)
  , decryptCallbackPostBody, CallbackEventInput(..)
  , parseCallbackEventData, handleCallbackEventDataOrLog
  , mkCallbackRespose
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Arrow
import           Control.Monad.Except hiding (forM_)
import           Control.Monad.Logger
import           Data.Aeson as A
import qualified Data.Aeson.Extra     as AE
import           Data.List.NonEmpty (NonEmpty)
import           Data.Proxy
import           Data.Time.Clock.POSIX

import DingTalk.OAPI.Basic
import DingTalk.OAPI.Crypto
import DingTalk.OAPI.ErrorCode
import DingTalk.Helpers
-- }}}1


-- | XXX: 要手工保证每个实例类型都记录在 allKnownCallbackEvents 列表里
class (FromJSON (CallbackData a)) => CallbackEvent a where
  type CallbackData a :: *
  callbackTag :: a -> Text


parseCallbackDataJson :: CallbackEvent a => Proxy a -> Value -> A.Result (CallbackData a)
parseCallbackDataJson _ = fromJSON


-- | XXX: 要手工保证这里包含所有已实现的 CallbackEvent 类型
-- 不在此列表里的tag会让handleDingTalkCallback产生一条错误日志
allKnownCallbackEvents :: [SomeCallbackEvent]
allKnownCallbackEvents =
  [ SomeCallbackEvent CheckUrl
  , SomeCallbackEvent ProcessInstanceChange
  , SomeCallbackEvent ProcessTaskChange
  , SomeCallbackEvent UserAddOrg
  , SomeCallbackEvent UserModifyOrg
  , SomeCallbackEvent UserLeaveOrg
  , SomeCallbackEvent LabelUserChange
  , SomeCallbackEvent OrgDeptCreate
  , SomeCallbackEvent OrgDeptModify
  , SomeCallbackEvent OrgDeptRemove
  ]


isKnownCallbackTag :: Text -> Bool
isKnownCallbackTag tag = isJust $ find match_tag allKnownCallbackEvents
  where match_tag (SomeCallbackEvent x) = callbackTag x == tag


data SomeCallbackEvent = forall a. CallbackEvent a => SomeCallbackEvent a

callbackEventEqByTag :: SomeCallbackEvent -> SomeCallbackEvent -> Bool
callbackEventEqByTag (SomeCallbackEvent x) (SomeCallbackEvent y) = callbackTag x == callbackTag y


-- | 当 tag 未知或未有对应类型时的占位. 因此不用加到 allKnownCallbackEvents 去
data GenericCallbackEvent = GenericCallbackEvent Text

instance CallbackEvent GenericCallbackEvent where
  callbackTag (GenericCallbackEvent t) = t
  type CallbackData GenericCallbackEvent = Value


-- | 测试回调
data CheckUrl = CheckUrl

instance CallbackEvent CheckUrl where
  callbackTag _ = "check_url"
  type CallbackData CheckUrl = ()


-- | 文档没有具体分每个具体的通讯录事件回调的数据
-- 这类型打算用于所有跟用户相关的变化
data UserChangeData = UserChangeData
  { userChangeCorpId  :: CorpId
  , userChangeUserIds :: [UserId]
  , userChangeTime    :: Timestamp
  }

instance FromJSON UserChangeData where
  parseJSON = withObject "UserChangeData" $ \ o -> do
                UserChangeData <$> o .: "CorpId"
                               <*> o .: "UserId"
                               <*> o .: "TimeStamp"

-- | 文档没有具体分每个具体的通讯录事件回调的数据
-- 这类型打算用于所有跟用户相关的变化
data DeptChangeData = DeptChangeData
  { deptChangeCorpId  :: CorpId
  , deptChangeDeptIds :: [DeptId]
  , deptChangeTime    :: Timestamp
  }

instance FromJSON DeptChangeData where
  parseJSON = withObject "DeptChangeData" $ \ o -> do
                DeptChangeData <$> o .: "CorpId"
                               <*> o .: "DeptId"
                               <*> o .: "TimeStamp"


-- | 通讯录用户增加
data UserAddOrg = UserAddOrg

instance CallbackEvent UserAddOrg where
  callbackTag _ = "user_add_org"
  type CallbackData UserAddOrg = UserChangeData


-- | 通讯录用户更改
data UserModifyOrg = UserModifyOrg

instance CallbackEvent UserModifyOrg where
  callbackTag _ = "user_modify_org"
  type CallbackData UserModifyOrg = UserChangeData


-- | 通讯录用户离职
data UserLeaveOrg = UserLeaveOrg

instance CallbackEvent UserLeaveOrg where
  callbackTag _ = "user_leave_org"
  type CallbackData UserLeaveOrg = UserChangeData


-- | 员工角色信息发生变更
data LabelUserChange = LabelUserChange

instance CallbackEvent LabelUserChange where
  callbackTag _ = "label_user_change"
  type CallbackData LabelUserChange = UserChangeData


-- | 通讯录企业部门创建
data OrgDeptCreate = OrgDeptCreate

instance CallbackEvent OrgDeptCreate where
  callbackTag _ = "org_dept_create"
  type CallbackData OrgDeptCreate = DeptChangeData


-- | 通讯录企业部门修改
data OrgDeptModify = OrgDeptModify

instance CallbackEvent OrgDeptModify where
  callbackTag _ = "org_dept_modify"
  type CallbackData OrgDeptModify = DeptChangeData


-- | 通讯录企业部门删除
data OrgDeptRemove = OrgDeptRemove

instance CallbackEvent OrgDeptRemove where
  callbackTag _ = "org_dept_remove"
  type CallbackData OrgDeptRemove = DeptChangeData



-- | 审批实例开始，结束
data ProcessInstanceChangeData =
      ProcessInstanceStartData
        { processInstanceStartInstanceId    :: ProcessInstanceId
        , processInstanceStartCorpId        :: CorpId
        , processInstanceStartBizCategoryId :: Maybe BizCategoryId
        , processInstanceStartTitle         :: Text
        , processInstanceStartApplicant     :: UserId
        , processInstanceStartUrl           :: Text
        , processInstanceStartCreatedTime   :: Timestamp
        }
    | ProcessInstanceFinishData
        { processInstanceFinishInstanceId    :: ProcessInstanceId
        , processInstanceFinishCorpId        :: CorpId
        , processInstanceFinishBizCategoryId :: Maybe BizCategoryId
        , processInstanceFinishTitle         :: Text
        , processInstanceFinishApplicant     :: UserId
        , processInstanceFinishUrl           :: Text
        , processInstanceFinishCreatedTime   :: Timestamp
        , processInstanceFinishFinishTime    :: Timestamp
        , processInstanceFinishResult        :: ProcessInstResult
        }

-- {{{1 instances
instance FromJSON ProcessInstanceChangeData where
  parseJSON = withObject "ProcessInstanceChangeData" $ \ o -> do
    typ <- fmap asText $ o .: "type"
    case typ of
      "start" -> ProcessInstanceStartData <$> o .: "processInstanceId"
                                          <*> o .: "corpId"
                                          <*> o .:? "bizCategoryId"
                                          <*> o .: "title"
                                          <*> o .: "staffId"
                                          <*> o .: "url"
                                          <*> o .: "createTime"

      "finish" -> ProcessInstanceFinishData <$> o .: "processInstanceId"
                                            <*> o .: "corpId"
                                            <*> o .:? "bizCategoryId"
                                            <*> o .: "title"
                                            <*> o .: "staffId"
                                            <*> o .: "url"
                                            <*> o .: "createTime"
                                            <*> o .: "finishTime"
                                            <*> o .: "result"

      _ -> fail $ "unknown ProcessInstanceChangeData type: " <> unpack typ
-- }}}1


-- | 审批任务开始，结束，转交
data ProcessTaskChangeData =
      ProcessTaskStartData
        { processTaskStartInstanceId    :: ProcessInstanceId
        , processTaskStartCorpId        :: CorpId
        , processTaskStartBizCategoryId :: Maybe BizCategoryId
        , processTaskStartTitle         :: Text
        , processTaskStartStaff         :: UserId
        , processTaskStartCreatedTime   :: Timestamp
        }
    | ProcessTaskFinishData
        { processTaskFinishInstanceId    :: ProcessInstanceId
        , processTaskFinishCorpId        :: CorpId
        , processTaskFinishBizCategoryId :: Maybe BizCategoryId
        , processTaskFinishTitle         :: Text
        , processTaskFinishStaff         :: UserId
        , processTaskFinishCreatedTime   :: Timestamp
        , processTaskFinishFinishTime    :: Timestamp
        , processTaskFinishResult        :: ProcessInstResult
        , processTaskFinishRemark        :: Maybe Text
        }


-- {{{1 instances
instance FromJSON ProcessTaskChangeData where
  parseJSON = withObject "ProcessTaskChangeData" $ \ o -> do
    typ <- fmap asText $ o .: "type"
    case typ of
      "start" -> ProcessTaskStartData <$> o .: "processInstanceId"
                                      <*> o .: "corpId"
                                      <*> o .:? "bizCategoryId"
                                      <*> o .: "title"
                                      <*> o .: "staffId"
                                      <*> o .: "createTime"

      "finish" -> do
        result <- o .: "result"
        ProcessTaskFinishData <$> o .: "processInstanceId"
                              <*> o .: "corpId"
                              <*> o .:? "bizCategoryId"
                              <*> o .: "title"
                              <*> o .: "staffId"
                              <*> o .: "createTime"
                              <*> o .: "finishTime"
                              <*> o .: result
                              <*> o .:? "remark"

      _ -> fail $ "unknown ProcessTaskChangeData type: " <> unpack typ
-- }}}1


data ProcessInstanceChange = ProcessInstanceChange

instance CallbackEvent ProcessInstanceChange where
  callbackTag _ = "bpms_instance_change"
  type CallbackData ProcessInstanceChange = ProcessInstanceChangeData


data ProcessTaskChange = ProcessTaskChange

instance CallbackEvent ProcessTaskChange where
  callbackTag _ = "bpms_task_change"
  type CallbackData ProcessTaskChange = ProcessTaskChangeData



oapiRegisterCallback :: HttpCallMonad env m
                     => EncodingAesKey
                     -> CallbackToken
                     -> Text  -- ^ URL
                     -> NonEmpty SomeCallbackEvent
                     -> OapiRpcWithAtk m ()
-- {{{1
oapiRegisterCallback aes_key token url tags =
  oapiPostCallWithAtk "/call_back/register_call_back"
    []
    ( object
        [ "aes_key" .= aes_key
        , "token" .= token
        , "url" .= url
        , "call_back_tag" .= map ( \ (SomeCallbackEvent p) -> callbackTag p) (toList tags)
        ]
    )
    >>= return . right unUnit
-- }}}1


oapiUpdateCallback :: HttpCallMonad env m
                   => EncodingAesKey
                   -> CallbackToken
                   -> Text  -- ^ URL
                   -> NonEmpty SomeCallbackEvent
                   -> OapiRpcWithAtk m ()
-- {{{1
oapiUpdateCallback aes_key token url tags =
  oapiPostCallWithAtk "/call_back/update_call_back"
    []
    ( object
        [ "aes_key" .= aes_key
        , "token" .= token
        , "url" .= url
        , "call_back_tag" .= map ( \ (SomeCallbackEvent p) -> callbackTag p) (toList tags)
        ]
    )
    >>= return . right unUnit
-- }}}1


oapiRegisterOrUpdateCallback :: HttpCallMonad env m
                             => EncodingAesKey
                             -> CallbackToken
                             -> Text  -- ^ URL
                             -> NonEmpty SomeCallbackEvent
                             -> OapiRpcWithAtk m ()
-- {{{1
oapiRegisterOrUpdateCallback aes_key token url tags = runExceptT $ do
  catchOapiError oapiEcCallbackAlreadyExists
    (ExceptT (oapiRegisterCallback aes_key token url tags))
    (ExceptT $ oapiUpdateCallback aes_key token url tags)
-- }}}1


oapiDeleteCallback :: HttpCallMonad env m
                   => OapiRpcWithAtk m ()
oapiDeleteCallback =
  oapiGetCallWithAtk "/call_back/delete_call_back" []
    >>= return . right unUnit


data CallbackFailedItem = CallbackFailedItem
  { cbFailedItemEventTime    :: Timestamp
  , cbFailedItemCorpId       :: CorpId
  , cbFailedItemUserId       :: [UserId]
  , cbFailedItemDeptId       :: [DeptId]
  , cbFailedItemCallbackTag  :: Text
  , cbFailedItemCallbackData :: Value
  }

-- {{{1 instances
instance FromJSON CallbackFailedItem where
  parseJSON = withObject "CallbackFailedItem" $ \ o -> do
    CallbackFailedItem <$> o .: "event_time"
                       <*> o .: "corpid"
                       <*> o .:? "userid" .!= []
                       <*> o .:? "deptid" .!= []
                       <*> o .: "call_back_tag"
                       <*> ( o .: "callbackData"
                              >>= either fail return . eitherDecode' . fromStrict . encodeUtf8 . asText
                           )
                           -- 文档说 callbackData 里面是一个json结构的字符串
                           -- 但没有具体例子
-- }}}1


data CallbackGetFailedResponse = CallbackGetFailedResponse
  { cbGetFailedHasMore :: Bool
  , cbGetFailedItems   :: [CallbackFailedItem]
  }

instance FromJSON CallbackGetFailedResponse where
  parseJSON = withObject "CallbackGetFailedResponse" $ \ o -> do
    CallbackGetFailedResponse <$> o .: "has_more"
                              <*> o .: "failed_list"


oapiGetCallbackFailedList :: HttpCallMonad env m
                          => OapiRpcWithAtk m CallbackGetFailedResponse
oapiGetCallbackFailedList =
  oapiGetCallWithAtk "/call_back/get_call_back_failed_result" []


data CallbackEventInput = CallbackEventInput
  { cbEventInputTag            :: Text
  , cbEventInputCorpOrSuiteKey :: Text
  , cbEventInputData           :: Value
  }


-- | 响应回调时，解释 HTTP 的 post body，得到所需的 CallbackData 及 CorpId/SuiteKey
decryptCallbackPostBody :: AesEnv
                        -> CallbackToken
                        -> Text -- ^ signature
                        -> Timestamp
                        -> Nonce
                        -> Value
                        -> Either Text CallbackEventInput
-- {{{1
decryptCallbackPostBody aes_env token signature timestamp nonce input_jv = do
  input_enc <- fromJSON'Message input_jv
  let enc_bs = encodeUtf8 $ AE.getSingObject (Proxy :: Proxy "encrypt") input_enc
      my_sign = signForProcessApi token timestamp nonce enc_bs

  unless (signature == my_sign) $ do
    Left $ "Signature error, expected was: " <> my_sign

  (payload_bs, id_or_key) <- left fromString $ decryptForProcessApi aes_env enc_bs
  payload_jv <- left fromString $ eitherDecode' (fromStrict payload_bs)
  evt_type <- AE.getSingObject (Proxy :: Proxy "EventType") <$> fromJSON'Message payload_jv
  payload <- fromJSON'Message payload_jv
  return $ CallbackEventInput evt_type id_or_key payload
-- }}}1


parseCallbackEventData :: CallbackEvent a
                       => CallbackEventInput
                       -> a
                       -> Maybe (Either String (CallbackData a))
                       -- ^ Nothing: event tag not matched
                       -- ^ Just (Left xx): failed to parse json data
-- {{{1
parseCallbackEventData (CallbackEventInput{..}) evt = do
  guard $ cbEventInputTag == callbackTag evt
  return $
    case A.fromJSON cbEventInputData of
      A.Error err -> Left err
      A.Success x -> Right x
-- }}}1


handleCallbackEventDataOrLog :: (CallbackEvent a, MonadLogger m)
                             => CallbackEventInput
                             -> a
                             -> (CallbackData a -> m ())
                             -> m ()
handleCallbackEventDataOrLog cb_input evt f =
  forM_ (parseCallbackEventData cb_input evt) $ \ err_or_dat -> do
    case err_or_dat of
      Left err -> $logErrorS logSourceName $ "Failed to parse callback event data: " <> fromString err
      Right dat -> f dat


-- | 正常回复回调只有一种方式
mkCallbackRespose :: MonadIO m
                  => AesEnv
                  -> CallbackToken
                  -> Either CorpId SuiteKey
                  -> m Value
-- {{{1
mkCallbackRespose aes_env token corp_or_key = do
  encrypt_msg <- encryptForProcessApi' aes_env corp_or_key (fromString "success")
  ts <- liftIO $ fmap timestampFromPOSIXTime getPOSIXTime
  nonce <- oapiMakeNonce 8
  let signature = asString $ signForProcessApi token ts nonce encrypt_msg
  return $
    object [ "encrypt" .= asText (decodeUtf8 encrypt_msg)
           , "msg_signature" .= signature
           , "timeStamp" .= ts
           , "nonce" .= nonce
           ]
-- }}}1




-- vim: set foldmethod=marker:
