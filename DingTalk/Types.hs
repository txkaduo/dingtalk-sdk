{-# LANGUAGE CPP #-}
module DingTalk.Types where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Logger
import           Data.Aeson            (FromJSON, ToJSON)
import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))
import qualified Network.Wreq.Session  as WS
import           Text.Blaze.Html       (ToMarkup (..))
import           Text.Shakespeare.I18N (ToMessage (..))
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



#define NEWTYPE_TEXT_DERIVING \
  deriving (Show, Eq, Ord, Typeable, ToMessage, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           , ParamValue \
           )

newtype CorpId = CorpId { unCorpId :: Text }
  NEWTYPE_TEXT_DERIVING

newtype CorpSecret = CorpSecret { unCorpSecret :: Text }
  NEWTYPE_TEXT_DERIVING

newtype AccessToken = AccessToken { unAccessToken :: Text }
  NEWTYPE_TEXT_DERIVING

newtype JsapiTicket = JsapiTicket { unJsapiTicket :: Text }
  NEWTYPE_TEXT_DERIVING

newtype Nonce = Nonce { unNonce :: Text }
  NEWTYPE_TEXT_DERIVING

newtype UserID = UserID { unUserID :: Text }
  NEWTYPE_TEXT_DERIVING

newtype DeptID = DeptID { unDeptID :: Int64 }
  deriving (Show, Eq, Ord, Typeable, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           )

rootDeptID :: DeptID
rootDeptID = DeptID 1

instance ParamValue DeptID where
  toParamValue = tshow . unDeptID


newtype DeviceID = DeviceID { unDeviceID :: Text }
  NEWTYPE_TEXT_DERIVING

newtype MediaID = MediaID { unMediaID :: Text }
  NEWTYPE_TEXT_DERIVING

newtype AgentID = AgentID { unAgentID :: Text }
  NEWTYPE_TEXT_DERIVING


class HasAccessToken a where
  getAccessToken ::  a -> AccessToken


class HasWreqSession a where
    getWreqSession :: a -> WS.Session

instance HasWreqSession WS.Session where
    getWreqSession = id


data DatagramError = DatagramError String
  deriving (Show)

instance Exception DatagramError

type HttpCallMonad r m = (MonadIO m, MonadLogger m, MonadThrow m
                         , MonadReader r m
                         , HasWreqSession r
                         )

logSourceName :: Text
logSourceName = "dingtalk-sdk"

-- vim: set foldmethod=marker:
