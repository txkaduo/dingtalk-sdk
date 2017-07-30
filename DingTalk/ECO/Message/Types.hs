module DingTalk.ECO.Message.Types where

-- {{{1 imports
import ClassyPrelude
import Data.Aeson
import Text.Parsec.TX.Utils                 ( SimpleEncode(..)
                                            , derivePersistFieldS, deriveSimpleStringRepEnumBounded
                                            , deriveJsonS
                                            )
import Yesod.Helpers.Parsec                 ( derivePathPieceS )

-- }}}1


data MessageType = MessageTypeText
                 | MessageTypeImage
                 | MessageTypeVoice
                 | MessageTypeLink
                 | MessageTypeMarkdown
                 deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
$(derivePersistFieldS "MessageType")
$(derivePathPieceS "MessageType")
$(deriveJsonS "MessageType")
$(deriveSimpleStringRepEnumBounded "MessageType")

instance SimpleEncode MessageType where
  simpleEncode MessageTypeText     = "text"
  simpleEncode MessageTypeImage    = "image"
  simpleEncode MessageTypeVoice    = "voice"
  simpleEncode MessageTypeLink     = "link"
  simpleEncode MessageTypeMarkdown = "markdown"
-- }}}1


-- | 消息类型
-- TODO: missing: voice, image, file, link, OA
data Message = MessageText Text
  deriving (Show)

getMessageType :: Message -> MessageType
getMessageType (MessageText {}) = MessageTypeText

messageContentToJSON :: Message -> Value
messageContentToJSON (MessageText t) = object [ "content" .= t ]



-- vim: set foldmethod=marker:
