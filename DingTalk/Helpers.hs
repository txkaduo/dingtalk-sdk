module DingTalk.Helpers where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Data.Aeson.Lens      (key)
import qualified Data.Aeson           as A
import qualified Data.Aeson.Text      as A
import           Network.Wreq

import DingTalk.Types
-- }}}1


(&=) :: ParamValue a => Text -> a -> (Text, SomeParamValue)
infix 3 &=
(&=) k v = (k, SomeParamValue v)


(&?=) :: ParamValue a => Text -> Maybe a -> Maybe (Text, SomeParamValue)
infix 3 &?=
(&?=) k v = (k &=) <$> v


type ParamKvList = [(Text, SomeParamValue)]

applyParamKvListInQs :: ParamKvList -> Options -> Options
applyParamKvListInQs kv_list opts = foldl' f opts kv_list
  where f o (k, SomeParamValue v) = o & param k .~ [ toParamValue v ]


getJsonField :: (MonadIO m, A.FromJSON a, MonadLogger m)
             => A.Value
             -> Text
             -> m a
getJsonField jv field = do
  case jv ^? key field of
    Nothing -> do
      $logErrorS logSourceName $ "field '" <> field <> "' does not exist in JSON: " <> jv_txt
      liftIO $ throwM $ DatagramError $ unpack $ "field '" <> field <> "' does not exist in JSON"

    Just jv2 -> do
      case A.fromJSON jv2 of
        A.Success x -> return x
        A.Error err -> do
          $logErrorS logSourceName $ "failed to decode field '" <> field <> "' in JSON: "
                                      <> fromString err
                                      <> ". JSON was:\n" <> jv_txt
          liftIO $ throwM $ DatagramError $ unpack $
                                  "failed to decode field '"
                                  <> field <> "' in JSON: " <> fromString err

  where jv_txt = toStrict (A.encodeToLazyText jv)

-- vim: set foldmethod=marker:
