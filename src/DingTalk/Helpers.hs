{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.Helpers where

-- {{{1 imports
import           ClassyPrelude
#if MIN_VERSION_base(4, 13, 0)
import           Control.Monad (MonadFail(..))
#else
#endif

import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import qualified Control.Monad.Logger.CallStack as LCS
import           Data.Aeson.Lens      (key)
import qualified Data.Aeson           as A
import qualified Data.Aeson.Key       as A
import qualified Data.Aeson.Text      as A
import qualified Data.Aeson.Types     as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Base64.URL as B64L
import qualified Data.ByteString.Char8      as C8
import qualified Data.Char as Char
import           Data.List            ((!!))
import           Data.Time
import qualified Data.Text            as T
import           Network.Wreq hiding (Proxy)
import           System.Random              (randomIO, randomRIO)

import           GHC.Stack (HasCallStack)
-- }}}1


newtype Unit = Unit { unUnit :: () }

instance A.FromJSON Unit where
  parseJSON = A.withObject "Unit" $ const $ pure $ Unit ()


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

(&!=) :: ParamValue a => Text -> a -> Maybe (Text, SomeParamValue)
infix 3 &!=
(&!=) k v = Just $ k &= v


type ParamKvList = [(Text, SomeParamValue)]

applyParamKvListInQs :: ParamKvList -> Options -> Options
applyParamKvListInQs kv_list opts = foldl' f opts kv_list
  where f o (k, SomeParamValue v) = o & param k .~ [ toParamValue v ]


#if MIN_VERSION_aeson(2, 0, 0)
type AesonKey = A.Key
#else
type AesonKey = Text
#endif

aesonKeyToText :: AesonKey -> Text
#if MIN_VERSION_aeson(2, 0, 0)
aesonKeyToText = A.toText
#else
aesonKeyToText = id
#endif

aesonKeyFromText :: Text -> AesonKey
#if MIN_VERSION_aeson(2, 0, 0)
aesonKeyFromText = A.fromText
#else
aesonKeyFromText = id
#endif


getJsonField :: (MonadIO m, A.FromJSON a, MonadLogger m)
             => A.Value
             -> AesonKey
             -> m a
-- {{{1
getJsonField jv field = do
  mx <- getJsonFieldMay jv field
  case mx of
    Just x -> return x
    Nothing -> do
      $logErrorS logSourceName $ "field '" <> aesonKeyToText field <> "' does not exist in JSON: " <> jv_txt
      liftIO $ throwIO $ DatagramError $ unpack $ "field '" <> aesonKeyToText field <> "' does not exist in JSON"
  where jv_txt = toStrict (A.encodeToLazyText jv)
-- }}}1


getJsonFieldMay :: (MonadIO m, A.FromJSON a, MonadLogger m)
                => A.Value
                -> AesonKey
                -> m (Maybe a)
-- {{{1
getJsonFieldMay jv field = do
  case jv ^? key field of
    Nothing -> return Nothing

    Just jv2 -> fmap Just $ do
      case A.fromJSON jv2 of
        A.Success x -> return x
        A.Error err -> do
          $logErrorS logSourceName $ "failed to decode field '" <> aesonKeyToText field <> "' in JSON: "
                                      <> fromString err
                                      <> ". JSON was:\n" <> jv_txt
          liftIO $ throwIO $ DatagramError $ unpack $
                                  "failed to decode field '"
                                  <> aesonKeyToText field <> "' in JSON: " <> fromString err

  where jv_txt = toStrict (A.encodeToLazyText jv)
-- }}}1


fromJSON'Message :: (A.FromJSON a)
                 => A.Value
                 -> Either Text a
-- {{{1
fromJSON'Message jv =
  case A.fromJSON jv of
    A.Success x -> return x
    A.Error err -> Left $ "Failed to parse JSON structure: " <> fromString err
                          <> "\ndata was:\n"
                          <> toStrict (decodeUtf8 $ AP.encodePretty jv)
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
  maybe (fail $ "unknown value string: " <> show t) return $ parseEnumParamValueText t


-- | 生成随机字串: 字串使用base64相同的字符集
randomBase64String :: MonadIO m
                   => Int
                   -> m String
randomBase64String salt_len = liftIO $ do
  liftM (C8.unpack . take salt_len . B64L.encodeBase64' . pack) $
      replicateM gen_len randomIO
  where gen_len = salt_len  -- long enough


randomAlphaNumString :: MonadIO m
                     => Int
                     -> m String
randomAlphaNumString len = do
  liftIO $ replicateM len $ fmap (chars !!) $ randomRIO (0, chars_len - 1)
  where chars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
        chars_len = length chars


data DatagramError = DatagramError String
  deriving (Show)

instance Exception DatagramError

logSourceName :: Text
logSourceName = "dingtalk-sdk"


logUnexpectedEmptyResult :: (MonadLogger m, HasCallStack, MonoFoldable a)
                         => Text
                         -> a
                         -> m a
-- {{{1
logUnexpectedEmptyResult err_msg res = do
  when (null res) $ LCS.logError err_msg
  return res
-- }}}1


lowerFirst :: String -> String
lowerFirst (x:xs) = Char.toLower x : xs
lowerFirst []     = []


-- | 发现至少部分接口返回的报文里时间字串的时区其实是错的
-- 例如 2023-06-17T17:18:00Z 其实就是一个北京时间
-- 由于不确定未来什么时候报文的这个bug会被修正，暂时使用一种 hacking 的方式避开
-- 假定只要返回的字串只是时区标识写错了，所以解释得到零时区就认为是bug，修正为北京时区
class HackFixZonedTime a where
  hackFixZonedTime :: a -> a

instance HackFixZonedTime ZonedTime where
  hackFixZonedTime zt =
    if timeZoneMinutes (zonedTimeZone zt) == 0
       then zt { zonedTimeZone = hoursToTimeZone 8 }
       else zt

instance HackFixZonedTime a => HackFixZonedTime [a] where
  hackFixZonedTime = map hackFixZonedTime

instance HackFixZonedTime a => HackFixZonedTime (Maybe a) where
  hackFixZonedTime = fmap hackFixZonedTime

-- vim: set foldmethod=marker:
