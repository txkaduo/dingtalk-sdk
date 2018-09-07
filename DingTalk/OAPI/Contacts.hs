{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.OAPI.Contacts
  ( oapiGetDeptListIds
  , DeptInfo(..), oapiGetDeptList
  , UserSimpleInfo(..), DeptUserSortOrder(..), oapiGetDeptUserSimpleList
  , AdminSimpleInfo(..), AdminLevel(..), oapiGetAdminList, oapiSourceDeptUserSimpleInfo
  , DeptDetails(..), oapiGetDeptSubForest
  , oapiGetAdminDeptList, oapiGetDeptDetails
  , oapiMaxPageSize
  , oapiGetUserIdByUnionId
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Except hiding (mapM_, mapM)
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.Conduit
import           Data.Tree
import           Data.Proxy

import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


-- | 获取部门ID列表．不受授权范围限制
oapiGetDeptListIds :: HttpCallMonad env m
                   => DeptId
                   -> ReaderT AccessToken m (Either OapiError [ DeptId ])
oapiGetDeptListIds parent_id =
  oapiGetCallWithAtk "/department/list_ids"
    [ "id" &= parent_id
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "sub_dept_id_list"))



data DeptInfo = DeptInfo
  { deptInfoId          :: DeptId
  , deptInfoParent      :: Maybe DeptId
  , deptInfoName        :: Text
  , deptInfoCreateGroup :: Bool
  , deptInfoAutoAddUser :: Bool
  }
  deriving (Show)

-- {{{1 instances
instance FromJSON DeptInfo where
  parseJSON = withObject "DeptInfo" $ \ o ->
    DeptInfo <$> o .: "id"
             <*> o .:? "parentid"
             <*> o .: "name"
             <*> o .: "createDeptGroup"
             <*> o .: "autoAddUser"
-- }}}1


-- | 获取部门列表．
oapiGetDeptList :: HttpCallMonad env m
                => Bool
                -> DeptId
                -> ReaderT AccessToken m (Either OapiError [DeptInfo])
oapiGetDeptList recursive parent_id =
  oapiGetCallWithAtk "/department/list"
    [ "id" &= parent_id
    , "fetch_child" &= recursive
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "department"))


oapiGetDeptSubForest :: HttpCallMonad env m
                     => DeptId
                     -> ReaderT AccessToken m (Either OapiError (Forest DeptInfo))
-- {{{1
oapiGetDeptSubForest parent_id = runExceptT $ do
  ExceptT (oapiGetDeptList False parent_id) >>= mapM get_tree
  where
    get_tree p_dept_info = do
      sub_forest <- ExceptT (oapiGetDeptList False (deptInfoId p_dept_info))
                      >>= mapM get_tree
      return $ Node p_dept_info sub_forest
-- }}}1


data DeptDetails = DeptDetails
  { deptDetailsId               :: DeptId
  , deptDetailsName             :: Text
  , deptDetailsParentId         :: Maybe DeptId
  , deptDetailsOrder            :: Int
  , deptDetailsCreateDeptGroup  :: Bool
  , deptDetailsAutoAddUser      :: Bool
  , deptDetailsHiding           :: Bool
  , deptDetailsGroupContainSub  :: Bool
  , deptDetailsDeptGroupOwner   :: UserId
  , deptDetailsManagerUserIds   :: [UserId]
  , deptDetailsSourceIdentifier :: Maybe Text
  }
  deriving (Show)

-- {{{1 instances
instance FromJSON DeptDetails where
  parseJSON = withObject "DeptDetails" $ \ o ->
                DeptDetails <$> o .: "id"
                            <*> o .: "name"
                            <*> o .:? "parentid"
                            <*> o .: "order"
                            <*> o .: "createDeptGroup"
                            <*> o .: "autoAddUser"
                            <*> o .: "deptHiding"
                            <*> o .: "groupContainSubDept"
                            <*> o .: "orgDeptOwner"
                            <*> (o .: "deptManagerUseridList" >>= aesonParseBarSepText (pure . UserId))
                            <*> o .:? "sourceIdentifier"
-- }}}1


oapiGetDeptDetails :: HttpCallMonad env m
                   => DeptId
                   -> ReaderT AccessToken m (Either OapiError DeptDetails)
oapiGetDeptDetails dept_id = do
  oapiGetCallWithAtk "/department/get" [ "id" &= dept_id ]


data UserSimpleInfo = UserSimpleInfo
  { userSimpleInfoId   :: UserId
  , userSimpleInfoName :: Text
  }

instance FromJSON UserSimpleInfo where
  parseJSON = withObject "UserSimpleInfo" $ \ o ->
                UserSimpleInfo <$> o .: "userid"
                               <*> o .: "name"


data DeptUserSortOrder = DeptUserSortEntryAsc
                       | DeptUserSortEntryDesc
                       | DeptUserSortModifyAsc
                       | DeptUserSortModifyDesc
                       | DeptUserSortCustom
                       deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
instance ParamValue DeptUserSortOrder where
  toParamValue DeptUserSortEntryAsc   = "entry_asc"
  toParamValue DeptUserSortEntryDesc  = "entry_desc"
  toParamValue DeptUserSortModifyAsc  = "modify_asc"
  toParamValue DeptUserSortModifyDesc = "modify_desc"
  toParamValue DeptUserSortCustom     = "custom"
-- }}}1


-- | 分页时每页的最大数量．不知道是不是所有接口都有一样的限制
oapiMaxPageSize :: Int
oapiMaxPageSize = 100


-- | 获取部门用户
oapiGetDeptUserSimpleList :: HttpCallMonad env m
                          => Maybe DeptUserSortOrder
                          -> Maybe (Int, Int)
                          -- ^ paging: size, offset
                          -> DeptId
                          -> ReaderT AccessToken m (Either OapiError ([UserSimpleInfo], Bool))
-- {{{1
oapiGetDeptUserSimpleList m_sort_order m_size_offset parent_id = runExceptT $ do
  jv <- ExceptT $ oapiGetCallWithAtk "/user/simplelist" params
  user_list <- getJsonField jv "userlist"
  has_more <- fmap (fromMaybe False) $ getJsonFieldMay jv "hasMore"
  return (user_list, has_more)
  where
    params = catMaybes [ Just $ "department_id" &= parent_id
                       , "size" &?= fmap fst m_size_offset
                       , "offset" &?= fmap snd m_size_offset
                       , "order" &?= m_sort_order
                       ]
-- }}}1


oapiSourceDeptUserSimpleInfo :: HttpCallMonad env m
                             => Maybe DeptUserSortOrder
                             -> DeptId
                             -> Source (ExceptT OapiError (ReaderT AccessToken m)) UserSimpleInfo
-- {{{1
oapiSourceDeptUserSimpleInfo m_sort_order parent_id = src Nothing
  where
    size = oapiMaxPageSize

    src m_offset = do
      let m_size_offset = (size,) <$> m_offset
      (user_list, has_more) <- lift $ ExceptT $ oapiGetDeptUserSimpleList m_sort_order m_size_offset parent_id
      mapM_ yield user_list
      when has_more $ do
        let old_offset = fromMaybe 0 $ fmap snd m_size_offset
            new_offset = old_offset + 1
        src (Just new_offset)
-- }}}1


data AdminLevel = AdminLevelPrimary
                | AdminLevelSub

-- {{{1 instances
instance FromJSON AdminLevel where
  parseJSON v = do i <- parseJSON v
                   case i of
                    (1 :: Int) -> pure AdminLevelPrimary
                    2 -> pure AdminLevelSub
                    _ -> fail $ "unknown admin level: " <> show i
-- }}}1
  

data AdminSimpleInfo = AdminSimpleInfo
  { adminSimpleInfoLevel :: AdminLevel
  , adminSimpleInfoId    :: UserId
  }

instance FromJSON AdminSimpleInfo where
  parseJSON = withObject "AdminSimpleInfo" $ \ o ->
    AdminSimpleInfo <$> o .: "sys_level"
                    <*> o .: "userid"


-- | 获取管理员列表．
oapiGetAdminList :: HttpCallMonad env m
                 => ReaderT AccessToken m (Either OapiError [AdminSimpleInfo])
oapiGetAdminList =
  oapiGetCallWithAtk "/user/get_admin" []
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "admin_list"))



-- | 获取管理员通讯录权限范围
oapiGetAdminDeptList :: HttpCallMonad env m
                     => UserId
                     -> ReaderT AccessToken m (Either OapiError [DeptId])
oapiGetAdminDeptList user_id =
  oapiGetCallWithAtk "/user/get_admin_scope"
    [ "userid" &= user_id
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "dept_ids"))


oapiGetUserIdByUnionId :: HttpCallMonad env m
                       => UnionId
                       -> ReaderT AccessToken m (Either OapiError UserId)
oapiGetUserIdByUnionId union_id =
  oapiGetCallWithAtk "/user/getUseridByUnionid"
    [ "unionid" &= union_id
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "userid"))


-- vim: set foldmethod=marker:
