module DingTalk.ECO.Message.CorpConversation where

-- {{{1
import           ClassyPrelude
import           Data.Aeson            as A
import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))
import           Text.Blaze.Html       (ToMarkup (..))
import           Text.Parsec.TX.Utils  (SimpleEncode (..))
import           Text.Shakespeare.I18N (ToMessage (..))

import DingTalk.Types
import DingTalk.ECO.Basic
import DingTalk.ECO.Message.Types
-- }}}1



#define NEWTYPE_TEXT_DERIVING \
  deriving (Show, Eq, Ord, Typeable, ToMessage, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           )

newtype CorpConversationAsyncSendTaskId = CorpConversationAsyncSendTaskId { unCorpConversationAsyncSendTaskId :: Text }
  NEWTYPE_TEXT_DERIVING

data CorpConversationAsyncSendResp =
  CorpConversationAsyncSendResp
    CorpConversationAsyncSendTaskId

-- {{{1 instances
instance FromJSON CorpConversationAsyncSendResp where
  parseJSON = withObject "CorpConversationAsyncSendResp" $ \ o -> do
    o2 <- o .: "dingtalk_corp_message_corpconversation_asyncsend_response"
    o3 <- o2 .: "result"
    CorpConversationAsyncSendResp <$> o3 .: "task_id"
-- }}}1


ecoCorpConversationAsyncSend :: (HttpCallMonad env m)
                             => EcoCommonParam
                             -> AgentId
                             -> Message
                             -> [Either UserId DeptId]
                             -> m (Either EcoError CorpConversationAsyncSendResp)
-- {{{1
ecoCorpConversationAsyncSend eco_common agent_id msg to_recvs = do
  ecoApiCall eco_common "dingtalk.corp.message.corpconversation.asyncsend" extra_params
  where
    user_ids = lefts to_recvs
    dept_ids = rights to_recvs

    extra_params = catMaybes
      [ Just ("agent_id", unAgentId agent_id)
      , Just ("msgtype", fromString (simpleEncode $ getMessageType msg))
      , Just ("msgcontent", toStrict (decodeUtf8 (A.encode $ messageContentToJSON msg)))

      , if null user_ids
           then Nothing
           else Just ("userid_list", intercalate "," (map unUserId user_ids))

      , if null dept_ids
           then Nothing
           else Just ("dept_id_list", intercalate "," (map tshow dept_ids))

      , if null to_recvs
           then Just ("to_all_user", "true")
           else Nothing
      ]
-- }}}1



-- vim: set foldmethod=marker:
