module DingTalk.OAPI.Message.CorpConversation where

-- {{{1 imports
import           ClassyPrelude
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.Either (partitionEithers)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Proxy

import DingTalk.Types
import DingTalk.OAPI.Basic
import DingTalk.OAPI.Message.Types
-- }}}1


data CorpConversationReceiver = CorpConversationAll
                              | CorpConversationUserOrDept (NonEmpty (Either UserId DeptId))
                              deriving (Eq, Show)

-- | 发送工作通知消息
oapiCorpConversationAsyncSend :: HttpCallMonad env m
                              => AgentId
                              -> CorpConversationReceiver
                              -> SomeMessage
                              -> OapiRpcWithAtk m CorpConversationAsyncSendId
-- {{{1
oapiCorpConversationAsyncSend agent_id receiver msg =
  oapiPostCallWithAtk "/topapi/message/corpconversation/asyncsend_v2" []
    (object $ catMaybes
            [ Just $ "agent_id" .= agent_id
            , Just $ "msg" .= msg
            , Just $ "to_all_user" .= (CorpConversationAll == receiver)
            , if null user_id_list
                 then mzero
                 else Just $ "userid_list" .= intercalate "," (map toParamValue user_id_list)
            , if null dept_id_list
                 then mzero
                 else Just $ "dept_id_list" .= intercalate "," (map toParamValue dept_id_list)
            ])
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "task_id"))
  where
    target_list = case receiver of
                    CorpConversationAll -> []
                    CorpConversationUserOrDept x -> toList x

    (user_id_list, dept_id_list) = partitionEithers target_list
-- }}}1



-- vim: set foldmethod=marker:
