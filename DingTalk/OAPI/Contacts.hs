{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.OAPI.Contacts
  ( oapiGetDeptListIds
  , DeptInfo(..), oapiGetSubDeptList
  , Role(..), UserDetails(..), oapiGetUserDetails
  , UserSimpleInfo(..), DeptUserSortOrder(..), oapiGetDeptUserSimpleList
  , AdminSimpleInfo(..), AdminLevel(..), oapiGetAdminList
  , oapiSourceDeptUserSimpleInfo, oapiSourceDeptUserSimpleInfoRecursive
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
import           Text.Show.Unicode (ushow)

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
  deriving (Show, Eq)

-- {{{1 instances
instance FromJSON DeptInfo where
  parseJSON = withObject "DeptInfo" $ \ o ->
    DeptInfo <$> o .: "id"
             <*> o .:? "parentid"
             <*> o .: "name"
             <*> o .: "createDeptGroup"
             <*> o .: "autoAddUser"

instance ToJSON DeptInfo where
  toJSON (DeptInfo {..}) = object $
    catMaybes [ Just $ "id" .= deptInfoId
              , ("parentid" .=) <$> deptInfoParent
              , Just $ "name" .= deptInfoName
              , Just $ "createDeptGroup" .= deptInfoCreateGroup
              , Just $ "autoAddUser" .= deptInfoAutoAddUser
              ]
-- }}}1


-- | 获取部门列表．
oapiGetSubDeptList :: HttpCallMonad env m
                   => Bool
                   -> DeptId
                   -> ReaderT AccessToken m (Either OapiError [DeptInfo])
oapiGetSubDeptList recursive parent_id =
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
  ExceptT (oapiGetSubDeptList False parent_id) >>= mapM get_tree
  where
    get_tree p_dept_info = do
      sub_forest <- ExceptT (oapiGetSubDeptList False (deptInfoId p_dept_info))
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
  deriving (Show, Eq)

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

instance ToJSON DeptDetails where
  toJSON (DeptDetails {..}) = object $
    catMaybes [ Just $ "id" .= deptDetailsId
              , Just $ "name" .= deptDetailsName
              , Just $ "parentid" .= deptDetailsParentId
              , Just $ "order" .= deptDetailsOrder
              , Just $ "createDeptGroup" .= deptDetailsCreateDeptGroup
              , Just $ "autoAddUser" .= deptDetailsAutoAddUser
              , Just $ "deptHiding" .= deptDetailsHiding
              , Just $ "groupContainSubDept" .= deptDetailsGroupContainSub
              , Just $ "orgDeptOwner" .= deptDetailsDeptGroupOwner
              , Just $ "deptManagerUseridList" .= intercalate "|" (map toParamValue deptDetailsManagerUserIds)
              , ("sourceIdentifier" .=) <$> deptDetailsSourceIdentifier
              ]
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

-- {{{1 instances
instance FromJSON UserSimpleInfo where
  parseJSON = withObject "UserSimpleInfo" $ \ o ->
                UserSimpleInfo <$> o .: "userid"
                               <*> o .: "name"

instance ToJSON UserSimpleInfo where
  toJSON (UserSimpleInfo {..}) = object
                                  [ "userid" .= userSimpleInfoId
                                  , "name" .= userSimpleInfoName
                                  ]
-- }}}1

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


oapiSourceDeptUserSimpleInfoRecursive :: HttpCallMonad env m
                                      => DeptId
                                      -> Source (ExceptT OapiError (ReaderT AccessToken m)) UserSimpleInfo
oapiSourceDeptUserSimpleInfoRecursive dept_id = do
  sub_dept_ids <- fmap (map deptInfoId) $ lift $ ExceptT $ oapiGetSubDeptList True dept_id
  mconcat (map (oapiSourceDeptUserSimpleInfo Nothing) $ dept_id : sub_dept_ids)


data Role = Role
  { roleId        :: RoleId
  , roleName      :: Text
  , roleGroupName :: Text
  -- XXX: 实际返回还有个 type 字段，是个数字，意义不明
  }
  deriving (Eq)

-- {{{1 instances
instance Show Role where
  show (Role xid name grp_name) =
    "Role {"
      <> intercalate ", " [  "id=" <> show xid, "name=" <> ushow name, "groupName=" <> ushow grp_name ]
      <> " }"

instance FromJSON Role where
  parseJSON = withObject "Role" $ \ o ->
                Role <$> o .: "id"
                     <*> o .: "name"
                     <*> o .: "groupName"

instance ToJSON Role where
  toJSON (Role {..}) = object
                        [ "id" .= roleId
                        , "name" .= roleName
                        , "groupName" .= roleGroupName
                        ]
-- }}}1


data UserDetails = UserDetails
  { userDetailsUserId      :: UserId
  , userDetailsUnionId     :: UnionId
  , userDetailsName        :: Text
  , userDetailsMobile      :: Maybe Text
  , userDetailsOrgEmail    :: Maybe Text
  , userDetailsActive      :: Bool
  , userDetailsIsAdmin     :: Bool
  , userDetailsIsBoss      :: Bool
  , userDetailsIsSenior    :: Bool
  , userDetailsDepartments :: [DeptId]
  , userDetailsAvatar      :: Maybe Text
  , userDetailsHiredTime   :: Maybe Timestamp
  , userDetailsRoles       :: [Role]
  -- XXX: 还有许多字段没有反映在这个类型里
  }
  deriving (Show, Eq)

-- {{{1 instances
instance FromJSON UserDetails where
  parseJSON = withObject "UserDetails" $ \ o ->
                UserDetails <$> o .: "userid"
                            <*> o .: "unionid"
                            <*> o .: "name"
                            <*> (o .:? "mobile" >>= nullTextAsNothing)
                            <*> (o .:? "orgEmail" >>= nullTextAsNothing)
                            <*> o .: "active"
                            <*> o .: "isAdmin"
                            <*> o .: "isBoss"
                            <*> o .: "isSenior"
                            <*> o .: "department"
                            <*> (o .:? "avatar" >>= nullTextAsNothing)
                            <*> o .:? "hiredDate"
                            <*> o .: "roles"

instance ToJSON UserDetails where
  toJSON (UserDetails {..}) = object $ catMaybes
                              [ Just $ "userid" .= userDetailsUserId
                              , Just $ "unionid" .= userDetailsUnionId
                              , Just $ "name" .= userDetailsName
                              , ("mobile" .=) <$> userDetailsMobile
                              , ("orgEmail" .=) <$> userDetailsOrgEmail
                              , Just $ "active" .= userDetailsActive
                              , Just $ "isAdmin" .= userDetailsIsAdmin
                              , Just $ "isBoss" .= userDetailsIsBoss
                              , Just $ "isSenior" .= userDetailsIsSenior
                              , Just $ "department" .= userDetailsDepartments
                              , ("avatar" .=) <$> userDetailsAvatar
                              , Just $ "hiredDate" .= userDetailsHiredTime
                              , Just $ "roles" .= userDetailsRoles
                              ]
-- }}}1

oapiGetUserDetails :: HttpCallMonad env m
                   => UserId
                   -> ReaderT AccessToken m (Either OapiError (Maybe UserDetails))
oapiGetUserDetails user_id =
  oapiGetCallWithAtk "/user/get"
    [ "userid" &= user_id
    ]


data AdminLevel = AdminLevelPrimary
                | AdminLevelSub

-- {{{1 instances
instance FromJSON AdminLevel where
  parseJSON v = do i <- parseJSON v
                   case i of
                    (1 :: Int) -> pure AdminLevelPrimary
                    2 -> pure AdminLevelSub
                    _ -> fail $ "unknown admin level: " <> show i

instance ToJSON AdminLevel where
  toJSON AdminLevelPrimary = toJSON (1 :: Int)
  toJSON AdminLevelSub     = toJSON (2 :: Int)
-- }}}1


data AdminSimpleInfo = AdminSimpleInfo
  { adminSimpleInfoLevel :: AdminLevel
  , adminSimpleInfoId    :: UserId
  }

-- {{{1 instances
instance FromJSON AdminSimpleInfo where
  parseJSON = withObject "AdminSimpleInfo" $ \ o ->
    AdminSimpleInfo <$> o .: "sys_level"
                    <*> o .: "userid"

instance ToJSON AdminSimpleInfo where
  toJSON (AdminSimpleInfo {..}) = object
                                  [ "sys_level" .= adminSimpleInfoLevel
                                  , "userid" .= adminSimpleInfoId
                                  ]
-- }}}1

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
