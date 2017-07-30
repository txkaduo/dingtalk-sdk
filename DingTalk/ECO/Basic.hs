module DingTalk.ECO.Basic where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Reader (asks)
import           Data.Aeson           as A
import           Data.Time
import           Network.Wreq
import qualified Network.Wreq.Session as WS

import DingTalk.Types
-- }}}1


ecoApiUrl :: IsString s => s
ecoApiUrl = "https://eco.taobao.com/router/rest"


data EcoCommonParam =
  EcoCommonParam
    AccessToken


data EcoError = EcoError
  { ecoErrorCode    :: Int
  , ecoErrorMsg     :: Text
  , ecoErrorSubCode :: Text
  , ecoErrorSubMsg  :: Text
  }

-- {{{1 instances
instance FromJSON EcoError where
  parseJSON = withObject "EcoError" $ \ o -> do
    EcoError <$> o .: "code"
             <*> o .: "msg"
             <*> o .: "sub_code"
             <*> o .: "sub_msg"
-- }}}1


data EcoCallResp a =  EcoCallResp { unEcoCallResp :: Either EcoError a }

-- {{{1 instances
instance FromJSON a => FromJSON (EcoCallResp a) where
  parseJSON v = fmap EcoCallResp $ parse_as_error <|> parse_as_x
    where
      parse_as_error = fmap Left $ parseJSON v
      parse_as_x = fmap Right $ parseJSON v
-- }}}1


ecoToPayload :: (MonadLogger m, MonadThrow m, FromJSON a)
             => Value
             -> m (Either EcoError a)
-- {{{1
ecoToPayload v = do
  case fromJSON v of
    A.Error err -> do
      $logError $ "Could not parse response body to payload: " <> fromString err
      throwM $ DatagramError err

    A.Success (EcoCallResp x) -> return x
-- }}}1


ecoApiCall :: (FromJSON a, HttpCallMonad env m)
           => EcoCommonParam
           -> Text
           -> [(Text, Text)]
           -> m (Either EcoError a)
-- {{{1
ecoApiCall (EcoCommonParam atk) method extra_params = do
  sess <- asks getWreqSession
  now <- liftIO getCurrentTime
  let tz = hoursToTimeZone 8
      now_t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (utcToLocalTime tz now)

  let extra_params' = map (uncurry (.=)) extra_params
  let val = object $ ("timestamp" .= now_t) : extra_params' <> common_params

  liftIO (WS.post sess url $ toJSON val)
    >>= asJSON
    >>= return . view responseBody
    >>= ecoToPayload
  where
    url = ecoApiUrl
    common_params = [ "method" .= method
                    , "session" .= atk
                    , "format" .= asText "json"
                    , "v" .= asText "2.0"
                    ]
-- }}}1


-- vim: set foldmethod=marker:
