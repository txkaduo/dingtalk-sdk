{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.Helpers where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Data.Aeson.Lens      (key)
import qualified Data.Aeson           as A
import qualified Data.Aeson.Text      as A
import qualified Data.Aeson.Types     as A
import qualified Data.ByteString.Base64.URL as B64L
import qualified Data.ByteString.Char8      as C8
import           Data.List            ((!!))
import qualified Data.Text            as T
import           Network.Wreq hiding (Proxy)
import           System.Random              (randomIO, randomRIO)
-- }}}1


class ParamValue a where
  toParamValue :: a -> Text

instance ParamValue Text where
  toParamValue = id

instance ParamValue Bool where
  toParamValue = bool "false" "true"

instance ParamValue Int where toParamValue = tshow


data SomeParamValue = forall a. ParamValue a => SomeParamValue a

instance ParamValue SomeParamValue where
  toParamValue (SomeParamValue v) = toParamValue v



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
-- {{{1
getJsonField jv field = do
  mx <- getJsonFieldMay jv field
  case mx of
    Just x -> return x
    Nothing -> do
      $logErrorS logSourceName $ "field '" <> field <> "' does not exist in JSON: " <> jv_txt
      liftIO $ throwM $ DatagramError $ unpack $ "field '" <> field <> "' does not exist in JSON"
  where jv_txt = toStrict (A.encodeToLazyText jv)
-- }}}1


getJsonFieldMay :: (MonadIO m, A.FromJSON a, MonadLogger m)
                => A.Value
                -> Text
                -> m (Maybe a)
-- {{{1
getJsonFieldMay jv field = do
  case jv ^? key field of
    Nothing -> return Nothing

    Just jv2 -> fmap Just $ do
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
-- }}}1


aesonParseSepText :: Text -> (Text -> A.Parser a) -> Text -> A.Parser [a]
aesonParseSepText sep f t =
  mapM f $ filter (not . null) $ T.splitOn sep t


aesonParseSepTextOrList :: A.FromJSON a => Text -> (Text -> A.Parser a) -> A.Value -> A.Parser [a]
aesonParseSepTextOrList sep f (A.String t) = aesonParseSepText sep f t
aesonParseSepTextOrList _ _ v = A.parseJSON v


aesonParseBarSepText :: (Text -> A.Parser a) -> Text -> A.Parser [a]
aesonParseBarSepText = aesonParseSepText "|"


aesonParseBarSepNested :: forall a. A.FromJSON a => Text -> A.Parser [a]
aesonParseBarSepNested = aesonParseSepNested "|"


aesonParseSepNested :: forall a. A.FromJSON a => Text -> Text -> A.Parser [a]
aesonParseSepNested sep t =
  mapM p_t $ filter (not . null) $ T.splitOn sep t
  where
    p_t :: Text -> A.Parser a
    p_t s = case A.eitherDecode (fromStrict $ encodeUtf8 s) of
              Left err -> fail err
              Right x -> pure x


nullTextAsNothing :: Applicative m => Maybe Text -> m (Maybe Text)
nullTextAsNothing mt =
  case mt of
    Just t | null t -> pure Nothing
           | otherwise -> pure $ Just t
    _ -> pure Nothing


parseEnumParamValueText :: (ParamValue a, Enum a, Bounded a) => Text -> Maybe a
parseEnumParamValueText = flip lookup table
  where table = map (toParamValue &&& id) [minBound .. maxBound]


parseJsonParamValueEnumBounded :: (ParamValue a, Enum a, Bounded a) => String -> A.Value -> A.Parser a
parseJsonParamValueEnumBounded name = A.withText name $ \ t -> do
  maybe (fail $ "unknown value string: " <> unpack t) return $ parseEnumParamValueText t


-- | 生成随机字串: 字串使用base64相同的字符集
oapiMakeString :: MonadIO m
               => Int
               -> m String
-- {{{1
oapiMakeString salt_len = liftIO $ do
  liftM (C8.unpack . take salt_len . B64L.encode . pack) $
      replicateM gen_len randomIO
  where gen_len = salt_len  -- long enough
-- }}}1


randomAlphaNumString :: MonadIO m
                     => Int
                     -> m String
randomAlphaNumString len = do
  liftIO $ replicateM len $ fmap (chars !!) $ randomRIO (0, chars_len)
  where chars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
        chars_len = length chars


data DatagramError = DatagramError String
  deriving (Show)

instance Exception DatagramError

logSourceName :: Text
logSourceName = "dingtalk-sdk"


-- vim: set foldmethod=marker:
