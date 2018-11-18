module DingTalk.OAPI.Callback
  ( CallbackEvent(..)
  , SomeCallbackEvent(..)
  , allKnownCallbackEvents, isKnownCallbackTag
  , parseCallbackDataJson
  , ApprovalResult(..)
  , GenericCallbackEvent(..)
  , CheckUrl(..)
  , ProcessInstanceChangeData(..), ProcessInstanceChange(..)
  , ProcessTaskChangeData(..), ProcessTaskChange(..)
  , oapiPostCallbackRegister
  , oapiPostCallbackDelete
  , oapiGetCallbackFailedList, CallbackFailedItem(..), CallbackGetFailedResponse(..)
  , decryptCallbackPostBody, CallbackEventInput(..)
  , mkCallbackRespose
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Data.Aeson as A
import           Data.List.NonEmpty (NonEmpty)
import           Data.Proxy
import           Data.Time.Clock.POSIX

import DingTalk.OAPI.Basic
import DingTalk.OAPI.Crypto
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
  ]


isKnownCallbackTag :: Text -> Bool
isKnownCallbackTag tag = isJust $ find match_tag allKnownCallbackEvents
  where match_tag (SomeCallbackEvent x) = callbackTag x == tag


data SomeCallbackEvent = forall a. CallbackEvent a => SomeCallbackEvent a

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


data ApprovalResult = Approved | Denied | Redirect
  deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue ApprovalResult where
  toParamValue Approved = "agree"
  toParamValue Denied   = "refuse"
  toParamValue Redirect = "redirect"

instance ToJSON ApprovalResult where
  toJSON = toJSON . toParamValue

instance FromJSON ApprovalResult where
  parseJSON = withText "ApprovalResult" $ maybe mzero return . parseEnumParamValueText
-- }}}1


-- | 审批实例开始，结束
data ProcessInstanceChangeData =
      ProcessInstanceStartData
        { processInstanceStartInstanceId    :: ProcessInstanceId
        , processInstanceStartCorpId        :: CorpId
        , processInstanceStartBizCategoryId :: BizCategoryId
        , processInstanceStartTitle         :: Text
        , processInstanceStartStaff         :: UserId
        , processInstanceStartUrl           :: Text
        , processInstanceStartCreatedTime   :: POSIXTime
        }
    | ProcessInstanceFinishData
        { processInstanceFinishInstanceId    :: ProcessInstanceId
        , processInstanceFinishCorpId        :: CorpId
        , processInstanceFinishBizCategoryId :: BizCategoryId
        , processInstanceFinishTitle         :: Text
        , processInstanceFinishStaff         :: UserId
        , processInstanceFinishUrl           :: Text
        , processInstanceFinishCreatedTime   :: POSIXTime
        , processInstanceFinishFinishTime    :: POSIXTime
        , processInstanceFinishResult        :: ApprovalResult
        }

-- {{{1 instances
instance FromJSON ProcessInstanceChangeData where
  parseJSON = withObject "ProcessInstanceChangeData" $ \ o -> do
    typ <- fmap asText $ o .: "type"
    case typ of
      "start" -> ProcessInstanceStartData <$> o .: "processInstanceId"
                                          <*> o .: "corpId"
                                          <*> o .: "bizCategoryId"
                                          <*> o .: "title"
                                          <*> o .: "staffId"
                                          <*> o .: "url"
                                          <*> o .: "createTime"

      "finish" -> ProcessInstanceFinishData <$> o .: "processInstanceId"
                                            <*> o .: "corpId"
                                            <*> o .: "bizCategoryId"
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
        , processTaskStartBizCategoryId :: BizCategoryId
        , processTaskStartTitle         :: Text
        , processTaskStartStaff         :: UserId
        , processTaskStartCreatedTime   :: POSIXTime
        }
    | ProcessTaskFinishData
        { processTaskFinishInstanceId    :: ProcessInstanceId
        , processTaskFinishCorpId        :: CorpId
        , processTaskFinishBizCategoryId :: BizCategoryId
        , processTaskFinishTitle         :: Text
        , processTaskFinishStaff         :: UserId
        , processTaskFinishCreatedTime   :: POSIXTime
        , processTaskFinishFinishTime    :: POSIXTime
        , processTaskFinishResult        :: ApprovalResult
        , processTaskFinishRemark        :: Maybe Text
        }


-- {{{1 instances
instance FromJSON ProcessTaskChangeData where
  parseJSON = withObject "ProcessTaskChangeData" $ \ o -> do
    typ <- fmap asText $ o .: "type"
    case typ of
      "start" -> ProcessTaskStartData <$> o .: "processInstanceId"
                                      <*> o .: "corpId"
                                      <*> o .: "bizCategoryId"
                                      <*> o .: "title"
                                      <*> o .: "staffId"
                                      <*> o .: "createTime"

      "finish" -> do
        result <- o .: "result"
        ProcessTaskFinishData <$> o .: "processInstanceId"
                              <*> o .: "corpId"
                              <*> o .: "bizCategoryId"
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



oapiPostCallbackRegister :: HttpCallMonad env m
                         => EncodingAesKey
                         -> CallbackToken
                         -> Text  -- ^ URL
                         -> NonEmpty SomeCallbackEvent
                         -> ReaderT AccessToken m (Either OapiError ())
oapiPostCallbackRegister aes_key token url tags =
  oapiPostCallWithAtk "/call_back/register_call_back"
    []
    ( object
        [ "aes_key" .= aes_key
        , "token" .= token
        , "url" .= url
        , "tags" .= map ( \ (SomeCallbackEvent p) -> callbackTag p) (toList tags)
        ]
    )


oapiPostCallbackDelete :: HttpCallMonad env m
                       => ReaderT AccessToken m (Either OapiError ())
oapiPostCallbackDelete =
  oapiPostCallWithAtk "/call_back/delete_call_back"
    []
    (mempty :: ByteString)



data CallbackFailedItem = CallbackFailedItem
  { cbFailedItemEventTime    :: POSIXTime
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
                          => ReaderT AccessToken m (Either OapiError CallbackGetFailedResponse)
oapiGetCallbackFailedList =
  oapiGetCallWithAtk "/call_back/get_call_back_failed_result" []


data CallbackPostBody = CallbackPostBody
  { cbPostBodyEventType :: Text
  , cbPostBodyEncrypt   :: Text
  }

instance FromJSON CallbackPostBody where
  parseJSON = withObject "CallbackPostBody" $ \ o -> do
                CallbackPostBody <$> o .: "EventType"
                                 <*> o .: "encrypt"


data CallbackEventInput = CallbackEventInput
  { cbEventInputTag            :: Text
  , cbEventInputCorpOrSuiteKey :: Text
  , cbEventInputData           :: Value
  }


-- | 响应回调时，解释 HTTP 的 post body，得到所需的 CallbackData 及 CorpId/SuiteKey
decryptCallbackPostBody :: DingTalkAesEnv
                        -> Value
                        -> Either String CallbackEventInput
-- {{{1
decryptCallbackPostBody aes_env input_jv = do
  case fromJSON input_jv of
    A.Error err -> Left $ "Failed to parse JSON structure of input: " <> show err
    A.Success pb -> do
      let enc_bs = encodeUtf8 $ cbPostBodyEncrypt pb
      (payload_bs, id_or_key) <- decryptForProcessApi aes_env enc_bs
      payload <- eitherDecode' (fromStrict payload_bs)
      return $ CallbackEventInput (cbPostBodyEventType pb) id_or_key payload
-- }}}1


-- | 正常回复回调只有一种方式
mkCallbackRespose :: MonadIO m
                  => DingTalkAesEnv
                  -> CallbackToken
                  -> Either CorpId SuiteKey
                  -> m Value
-- {{{1
mkCallbackRespose aes_env token corp_or_key = do
  encrypt_msg <- encryptForProcessApi' aes_env corp_or_key (fromString "success")
  ts <- liftIO getPOSIXTime
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
