{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.OAPI.Message.Types where

-- {{{1 imports
import           ClassyPrelude
import           Data.Aeson           as A
import           Data.List.NonEmpty (NonEmpty)
import           Network.HTTP.Types (renderSimpleQuery)

import DingTalk.Types
-- }}}1


-- | 用于消息里的URL
data MessageUrl = MessageUrlExternal String -- ^ 外跳到浏览器打开的URL
                | MessageUrlInternal String -- ^ 直接在PC客户端中打开的URL
                | MessageUrlDefault String -- ^ 不处理URL
                deriving (Show)

-- {{{1 instances
instance ParamValue MessageUrl where
  toParamValue (MessageUrlExternal url) = decodeUtf8 $ encodeMessageUrlOpenType False (fromString url)
  toParamValue (MessageUrlInternal url) = decodeUtf8 $ encodeMessageUrlOpenType True (fromString url)
  toParamValue (MessageUrlDefault url) = fromString url

instance ToJSON MessageUrl where
  toJSON = toJSON . toParamValue
-- }}}1


encodeMessageUrlOpenType :: Bool -> ByteString -> ByteString
encodeMessageUrlOpenType in_client url = base_url <> qs
  where base_url = "dingtalk://dingtalkclient/page/link"
        qs = renderSimpleQuery True [ ("url", url), ("pc_slide", bool "false" "true" in_client) ]


-- | 消息类型
data MessageType = MessageTypeText
                 | MessageTypeImage
                 | MessageTypeVoice
                 | MessageTypeFile
                 | MessageTypeLink
                 | MessageTypeOA
                 | MessageTypeMarkdown
                 | MessageTypeActionCard
                 deriving (Show, Eq, Ord, Enum, Bounded)


-- {{{1 instances
instance ParamValue MessageType where
  toParamValue MessageTypeText       = "text"
  toParamValue MessageTypeImage      = "image"
  toParamValue MessageTypeVoice      = "voice"
  toParamValue MessageTypeFile       = "voice"
  toParamValue MessageTypeLink       = "link"
  toParamValue MessageTypeOA         = "oa"
  toParamValue MessageTypeMarkdown   = "markdown"
  toParamValue MessageTypeActionCard = "action_card"
-- }}}1

data OaHeader = OaHeader
  { oaHeaderBgColor :: Text
  , oaHeaderText    :: Text
  }

instance ToJSON OaHeader where
  toJSON (OaHeader bg_color txt) = object [ "bgcolor" .= bg_color, "text" .= txt ]

data OaForm = OaForm
  { oaFormKey :: Text
  , oaFormValue :: Text
  }

instance ToJSON OaForm where
  toJSON (OaForm k v) = object [ "key" .= k, "value" .= v ]

data OaRich = OaRich
  { oaRichNum  :: Text
  , oaRichUnit :: Text
  }

instance ToJSON OaRich where
  toJSON (OaRich num unit) = object [ "num" .= num, "unit" .= unit ]

data OaBody = OaBody
  { oaBodyTitle     :: Text
  , oaBodyContent   :: Maybe Text
  , oaBodyImage     :: Maybe MediaId
  , oaBodyFileCount :: Maybe Int
  , oaBodyAuthor    :: Maybe Int
  , oaBodyForms     :: [OaForm]
  , oaBodyRich      :: Maybe OaRich
  }

-- {{{1 instances
instance ToJSON OaBody where
  toJSON (OaBody{..}) =
    object $ catMaybes $
      [ Just $ "title" .= oaBodyTitle
      , Just $ "form" .= oaBodyForms
      , ("rich" .=) <$> oaBodyRich
      , ("content" .=) <$> oaBodyContent
      , ("image" .=) <$> oaBodyImage
      , ("file_count" .=) <$> oaBodyFileCount
      , ("author" .=) <$> oaBodyAuthor
      ]
-- }}}1


defaultOaBody :: OaBody
defaultOaBody = OaBody mempty empty empty empty empty empty empty


data ActionCardOrientation = ActionCardHorizontal
                           | ActionCardVertical
                           deriving (Eq, Ord, Show, Enum, Bounded)

-- {{{1 instances
instance ParamValue ActionCardOrientation where
  toParamValue ActionCardHorizontal = "1"
  toParamValue ActionCardVertical   = "0"

instance ToJSON ActionCardOrientation where
  toJSON = toJSON . toParamValue
-- }}}1


-- | 消息
data Message (a :: MessageType) where
  MessageText       :: Text -> Message 'MessageTypeText
  MessageImage      :: MediaId -> Message 'MessageTypeImage
  MessageVoice      :: MediaId -> Int -> Message 'MessageTypeVoice
  MessageFile       :: MediaId -> Message 'MessageTypeFile
  MessageLink       :: MessageUrl -> MessageUrl -> Text -> Text -> Message 'MessageTypeLink
  MessageOA         :: MessageUrl -> OaHeader -> OaBody -> Message 'MessageTypeOA
  MessageMarkdown   :: Text -> Text -> Message 'MessageTypeMarkdown
  MessageActionCardSingle :: Text -> Text -> Text -> MessageUrl -> Message 'MessageTypeActionCard
  MessageActionCardMultiple :: Text -> Text -> ActionCardOrientation -> NonEmpty (Text, MessageUrl) -> Message 'MessageTypeActionCard

-- {{{1 instances
instance ToJSON (Message a) where
  toJSON (MessageText t)                      = object [ "content" .= t ]
  toJSON (MessageImage media_id)              = object [ "media_id" .= media_id ]
  toJSON (MessageVoice media_id duration)     = object [ "media_id" .= media_id, "duration" .= duration ]
  toJSON (MessageFile media_id)               = object [ "media_id" .= media_id ]
  toJSON (MessageLink url pic_url title body) = object [ "messageUrl" .= url, "picUrl" .= pic_url, "title" .= title, "text" .= body ]
  toJSON (MessageOA url header body)          = object [ "message_url" .= url, "head" .= header, "body" .= body ]
  toJSON (MessageMarkdown title txt)          = object [ "title" .= title, "text" .= txt ]
  toJSON (MessageActionCardSingle title markdown action_title action_url)
                                              = object [ "title" .= title, "markdown" .= markdown
                                                       , "single_title" .= action_title
                                                       , "single_url" .= action_url
                                                       ]
  toJSON (MessageActionCardMultiple title markdown orientation title_urls)
    = object $ [ "title" .= title, "markdown" .= markdown, "orientation" .= orientation ]
                <> flip concatMap (toList title_urls)
                    (\ (action_title, action_url) -> [ "title" .= action_title, "action_url" .= action_url ])
-- }}}1


messageType :: Message a -> MessageType
-- {{{1
messageType (MessageText {})               = MessageTypeText
messageType (MessageImage {})              = MessageTypeImage
messageType (MessageVoice {})              = MessageTypeVoice
messageType (MessageFile {})               = MessageTypeFile
messageType (MessageLink {})               = MessageTypeLink
messageType (MessageOA {})                 = MessageTypeOA
messageType (MessageMarkdown {})           = MessageTypeMarkdown
messageType (MessageActionCardSingle {})   = MessageTypeActionCard
messageType (MessageActionCardMultiple {}) = MessageTypeActionCard
-- }}}1


-- | 这是真正在报文中收发的消息类型
data SomeMessage = forall a. SomeMessage (Message a)

-- {{{1
instance ToJSON SomeMessage where
  toJSON (SomeMessage m) =
    object [ "msgtype" .= mt
           , mt .= m
           ]
    where mt = toParamValue $ messageType m
-- }}}1



-- vim: set foldmethod=marker:
