{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmP cc -optP -E -optP -undef -optP -std=c89 #-}
-- 上面这个命令行是为了使用 ## 这样的CPP操作符. ghc ticket 12516
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

#define NEWTYPE_DEF(t1, t2) newtype t1 = t1 { un ## t1 :: t2 }

#define NEWTYPE_DEF_TEXT(t1) NEWTYPE_DEF(t1, Text) NEWTYPE_TEXT_DERIVING

NEWTYPE_DEF_TEXT(AccessToken)
NEWTYPE_DEF_TEXT(SnsAccessToken)
NEWTYPE_DEF_TEXT(CorpId)
NEWTYPE_DEF_TEXT(CorpSecret)
NEWTYPE_DEF_TEXT(JsapiTicket)
NEWTYPE_DEF_TEXT(Nonce)
NEWTYPE_DEF_TEXT(UserId)
NEWTYPE_DEF_TEXT(SnsAppId)
NEWTYPE_DEF_TEXT(SnsAppSecret)
NEWTYPE_DEF_TEXT(SnsTmpAuthCode)
NEWTYPE_DEF_TEXT(SnsToken)
NEWTYPE_DEF_TEXT(DeviceId)
NEWTYPE_DEF_TEXT(MediaId)
NEWTYPE_DEF_TEXT(AgentId)
NEWTYPE_DEF_TEXT(OpenId)
NEWTYPE_DEF_TEXT(UnionId)
NEWTYPE_DEF_TEXT(SnsPersistentAuthCode)



NEWTYPE_DEF(RoleId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           )

NEWTYPE_DEF(DeptId, Int64)
  deriving (Show, Eq, Ord, Typeable, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           )

-- | 根部门代表的就是整个企业
rootDeptId :: DeptId
rootDeptId = DeptId 1

instance ParamValue DeptId where
  toParamValue = tshow . unDeptId


class HasAccessToken a where
  getAccessToken ::  a -> AccessToken


class HasWreqSession a where
    getWreqSession :: a -> WS.Session

instance HasWreqSession WS.Session where
    getWreqSession = id


class HasDingTalkCorpId a where
  getDingTalkCorpId :: a -> CorpId

class HasDingTalkLoginAppId a where
  getDingTalkLoginAppId :: a -> SnsAppId


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
