module DingTalk.OAPI.Media where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Reader (asks)
import           Data.Aeson           as A
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Client  (RequestBody)
import           Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import           Network.Mime         (MimeType)
import           Network.Wreq
import qualified Network.Wreq.Session as WS

import DingTalk.Types
import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


data MediaType = MediaTypeImage
               | MediaTypeVoice
               | MediaTypeFile
               deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue MediaType where
  toParamValue MediaTypeImage = "image"
  toParamValue MediaTypeVoice = "voice"
  toParamValue MediaTypeFile  = "file"

instance ToJSON MediaType where
  toJSON = toJSON . toParamValue

instance FromJSON MediaType where
  parseJSON = parseJsonParamValueEnumBounded "MediaType"
-- }}}1


data UploadMediaResp =
  UploadMediaResp
    MediaId
    MediaType
    Timestamp

-- {{{1
instance FromJSON UploadMediaResp where
  parseJSON = withObject "UploadMediaResp" $ \ o -> do
    UploadMediaResp <$> o .: "media_id"
                    <*> o .: "type"
                    <*> o .: "created_at"
-- }}}1


oapiUploadMedia :: HttpCallMonad env m
                => MediaType
                -> RequestBody
                -> Maybe FilePath
                -> Maybe MimeType
                -> OapiRpcWithAtk m UploadMediaResp
-- {{{1
oapiUploadMedia media_type file_content m_file_path m_mime_type = do
  oapiPostCallWithAtk "/media/upload"
    [ "type" &= media_type
    ]
    file_part
  where
    file_part = partFileRequestBody "media" "" file_content
                  & maybe id (\ fp -> partFileName .~ Just fp) m_file_path
                  & maybe id (\ mime_type -> partContentType .~ Just mime_type) m_mime_type
-- }}}1


oapiDownloadMedia :: HttpCallMonad env m
                  => MediaId
                  -> OapiRpcWithAtk m (Response LB.ByteString)
-- {{{1
oapiDownloadMedia media_id = do
  sess <- lift $ asks getWreqSession
  atk <- ask
  let opts = defaults & applyParamKvListInQs [ "access_token" &= atk, "media_id" &= media_id ]

  resp <- liftIO $ WS.getWith opts sess url
  if "application/json" `isPrefixOf` (resp ^. responseHeader "Content-Type")
     then do
       v <- fmap (view responseBody) $ asJSON resp
       case A.fromJSON v of
         A.Error err -> do
           $logErrorS logSourceName $
                      "cannot parse response body as error message: " <> fromString err
                      <> ", body is:\n" <>
                      toStrict (decodeUtf8 (resp ^. responseBody))

           throwM $ DatagramError "cannot parse response body"

         A.Success x -> return $ Left x

     else return $ Right resp

  where
    url = oapiUrlBase <> "/media/downloadFile"
-- }}}1


-- vim: set foldmethod=marker:
