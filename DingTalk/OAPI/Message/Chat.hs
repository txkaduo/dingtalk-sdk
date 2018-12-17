module DingTalk.OAPI.Message.Chat where

-- {{{1 imports
import           ClassyPrelude
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Proxy

import DingTalk.Types
import DingTalk.OAPI.Basic
import DingTalk.OAPI.Message.Types
-- }}}1


data ChatCreateResp = ChatCreateResp
  { chatCreateRespId :: ChatId
  , chatCreateRespTag :: Int
  }

instance FromJSON ChatCreateResp where
  parseJSON = withObject "ChatCreateResp" $ \ o ->
                ChatCreateResp <$> o .: "chatid"
                               <*> o .: "conversationTag"


-- | 创建会话
oapiChatCreate :: HttpCallMonad env m
               => Text
               -> Bool    -- ^ 新成员是否可以查看聊天历史消息
               -> NonEmpty UserId -- ^ first one is the owner
               -> OapiRpcWithAtk m ChatCreateResp
oapiChatCreate name show_history (owner :| user_id_list)  =
  oapiPostCallWithAtk "/chat/create" []
    (object [ "name" .= name, "showHistoryType" .= bool (asText "0") "1" show_history
            , "owner" .= owner, "useridlist" .= user_id_list
            ])


-- | 发送群消息
oapiChatSend :: HttpCallMonad env m
             => ChatId
             -> SomeMessage
             -> OapiRpcWithAtk m MessageId
oapiChatSend chat_id (SomeMessage real_msg) =
  oapiPostCallWithAtk "/chat/send" []
    (object [ "chatid" .= chat_id
            , "msgtype" .= mt
            , mt .= real_msg
            ])
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "messageId"))
  where
    mt = toParamValue $ messageType real_msg




-- vim: set foldmethod=marker:
