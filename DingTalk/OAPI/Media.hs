module DingTalk.OAPI.Media where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Reader (asks)
import           Data.Aeson           as A
import qualified Data.ByteString.Lazy as LB
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client  (RequestBody)
import           Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import           Network.Mime         (MimeType)
import           Network.Wreq
import qualified Network.Wreq.Session as WS
import           Text.Parsec.TX.Utils (SimpleEncode (..), deriveJsonS,
                                       derivePersistFieldS,
                                       deriveSimpleStringRepEnumBounded)
import           Yesod.Helpers.Parsec (derivePathPieceS)

import DingTalk.Types
import DingTalk.OAPI.Basic
-- }}}1


data MediaType = MediaTypeImage
               | MediaTypeVoice
               | MediaTypeFile
               deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
$(derivePersistFieldS "MediaType")
$(derivePathPieceS "MediaType")
$(deriveJsonS "MediaType")
$(deriveSimpleStringRepEnumBounded "MediaType")

instance SimpleEncode MediaType where
  simpleEncode MediaTypeImage = "image"
  simpleEncode MediaTypeVoice = "voice"
  simpleEncode MediaTypeFile  = "file"
-- }}}1


data UploadMediaResp =
  UploadMediaResp
    MediaID
    MediaType
    POSIXTime

-- {{{1
instance FromJSON UploadMediaResp where
  parseJSON = withObject "UploadMediaResp" $ \ o -> do
    UploadMediaResp <$> o .: "media_id"
                    <*> o .: "type"
                    <*> ((fromIntegral :: Int64 -> POSIXTime) <$> o .: "created_at")
-- }}}1


oapiUploadMedia :: HttpCallMonad env m
                => AccessToken
                -> MediaType
                -> RequestBody
                -> Maybe FilePath
                -> Maybe MimeType
                -> m (Either OapiError UploadMediaResp)
-- {{{1
oapiUploadMedia atk media_type file_content m_file_path m_mime_type = do
  sess <- asks getWreqSession
  liftIO (WS.postWith opts sess url file_part)
    >>= asJSON
    >>= return . view responseBody
    >>= oapiToPayload
  where
    url = oapiUrlBase <> "/media/upload"
    opts = defaults & param "access_token" .~ [ unAccessToken atk ]
                    & param "type" .~ [ fromString (simpleEncode media_type) ]

    file_part = partFileRequestBody "media" "" file_content
                  & maybe id (\ fp -> partFileName .~ Just fp) m_file_path
                  & maybe id (\ mime_type -> partContentType .~ Just mime_type) m_mime_type
-- }}}1


oapiDownloadMedia :: HttpCallMonad env m
                  => AccessToken
                  -> MediaID
                  -> m (Either OapiError (Response LB.ByteString))
-- {{{1
oapiDownloadMedia atk media_id = do
  sess <- asks getWreqSession
  resp <- liftIO $ WS.getWith opts sess url
  if "application/json" `isPrefixOf` (resp ^. responseHeader "Content-Type")
     then do
       v <- fmap (view responseBody) $ asJSON resp
       case A.fromJSON v of
         A.Error err -> do
           $logError $ "cannot parse response body as error message: " <> fromString err
                      <> ", body is:\n" <>
                      toStrict (decodeUtf8 (resp ^. responseBody))

           throwM $ DatagramError "cannot parse response body"

         A.Success x -> return $ Left x

     else return $ Right resp

  where
    url = oapiUrlBase <> "/media/downloadFile"
    opts = defaults & param "access_token" .~ [ unAccessToken atk ]
                    & param "media_id" .~ [ unMediaID media_id ]
-- }}}1

-- vim: set foldmethod=marker:
