{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.OAPI.Contacts
  ( oapiGetDeptListIds
  , DeptInfo(..), oapiGetSubDeptList
  , oapiGetDeptInfoSubForest, oapiGetDeptInfoTree, oapiGetDeptSimpleInfo
  , Role(..), UserDetails(..), oapiGetUserDetails', oapiGetUserDetails
  , UserSimpleInfo(..), DeptUserSortOrder(..), oapiGetDeptUserSimpleList
  , AdminSimpleInfo(..), AdminLevel(..), oapiGetAdminList
  , oapiSourceDeptUserSimpleInfo, oapiSourceDeptUserSimpleInfoRecursive
  , DeptInfoWithUser(..), oapiGetDeptInfoWithUserForest, oapiGetDeptInfoWithUserTree
  , DeptDetails(..), deptDetailsToInfo
  , oapiGetAdminDeptList, oapiGetDeptDetails
  , oapiDeptUserSimpleInfoMaxPageSize
  , oapiGetUserIdByUnionId
  , oapiGetUserParentDeptIds, oapiGetDeptParentDeptIds
  , oapiDeleteUser
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Arrow
import           Control.Monad.Except hiding (mapM_, mapM)
import           Control.Monad.Trans.Maybe
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as LNE
import           Data.Tree
import           Data.Proxy
import           Text.Show.Unicode (ushow)

import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


-- | 获取部门ID列表．不受授权范围限制
oapiGetDeptListIds :: HttpCallMonad env m
                   => DeptId
                   -> OapiRpcWithAtk m [ DeptId ]
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
                   -> OapiRpcWithAtk m (Maybe [DeptInfo])
-- {{{1
oapiGetSubDeptList recursive parent_id =
  fmap oapiNotFoundToMaybe $
    oapiGetCallWithAtk "/department/list"
      [ "id" &= parent_id
      , "fetch_child" &= recursive
      ]
      >>= return . fmap (AE.getSingObject (Proxy :: Proxy "department"))
-- }}}1


oapiGetDeptInfoSubForest :: HttpCallMonad env m
                         => DeptId
                         -> OapiRpcWithAtk m (Maybe (Forest DeptInfo))
-- {{{1
oapiGetDeptInfoSubForest parent_id = runExceptT $ runMaybeT $ do
  MaybeT (ExceptT (oapiGetSubDeptList False parent_id)) >>= mapM get_tree
  where
    get_tree p_dept_info = do
      let dept_id = deptInfoId p_dept_info
      sub_depts <- lift (ExceptT (oapiGetSubDeptList False dept_id))
                    >>= logUnexpectedEmptyResult
                          ("oapiGetSubDeptList should not return Nothing for dept_id: " <> toParamValue dept_id)
                    >>= return . fromMaybe []

      sub_forest <- mapM get_tree sub_depts
      return $ Node p_dept_info sub_forest
-- }}}1


oapiGetDeptInfoTree :: HttpCallMonad env m
                    => DeptId
                    -> OapiRpcWithAtk m (Maybe (Tree DeptInfo))
-- {{{1
oapiGetDeptInfoTree dept_id = runExceptT $ runMaybeT $ do
  dept_info <- MaybeT $ ExceptT $ oapiGetDeptSimpleInfo dept_id
  sub_forest <- MaybeT $ ExceptT $ oapiGetDeptInfoSubForest dept_id
  return $ Node dept_info sub_forest
-- }}}1


-- | 钉钉不提供能根据dept id直接取得 DeptInfo 的接口，
-- 这里用 oapiGetDeptParentDeptIds/oapiGetDeptDetails 间接实现
-- XXX: 若此部门的兄弟部门非常多，就有一点浪费
oapiGetDeptSimpleInfo :: HttpCallMonad env m
                      => DeptId
                      -> OapiRpcWithAtk m (Maybe DeptInfo)
-- {{{1
oapiGetDeptSimpleInfo dept_id = runExceptT $ do
  if dept_id == rootDeptId
     then fmap (fmap  deptDetailsToInfo) $ ExceptT (oapiGetDeptDetails dept_id)
     else do
          parent_ids <- ExceptT $ oapiGetDeptParentDeptIds dept_id
          fmap join $
            forM (listToMaybe $ LNE.tail parent_ids) $ \ real_parent_id -> do
              ExceptT (oapiGetSubDeptList False real_parent_id)
                >>= logUnexpectedEmptyResult
                      ("oapiGetSubDeptList should not return Nothing for dept_id: " <> toParamValue real_parent_id)
                >>= return . find ((== dept_id) . deptInfoId) . fromMaybe []
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

deptDetailsToInfo :: DeptDetails -> DeptInfo
-- {{{1
deptDetailsToInfo (DeptDetails {..}) =
  DeptInfo
    deptDetailsId
    deptDetailsParentId
    deptDetailsName
    deptDetailsCreateDeptGroup
    deptDetailsAutoAddUser
-- }}}1


oapiGetDeptDetails :: HttpCallMonad env m
                   => DeptId
                   -> OapiRpcWithAtk m (Maybe DeptDetails)
oapiGetDeptDetails dept_id = do
  fmap oapiNotFoundToMaybe $
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


-- | 分页时每页的最大数量．oapiGetDeptUserSimpleList 专用
oapiDeptUserSimpleInfoMaxPageSize :: Int
oapiDeptUserSimpleInfoMaxPageSize = 100


-- | 获取部门用户
oapiGetDeptUserSimpleList :: HttpCallMonad env m
                          => Maybe DeptUserSortOrder
                          -> Maybe (Int, Int)
                          -- ^ paging: size, offset
                          -> DeptId
                          -> OapiRpcWithAtk m ([UserSimpleInfo], Bool)
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
                             -> OapiRpcWithAtkSource m UserSimpleInfo
-- {{{1
oapiSourceDeptUserSimpleInfo m_sort_order parent_id = src Nothing
  where
    size = oapiDeptUserSimpleInfoMaxPageSize

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
                                      -> OapiRpcWithAtkSource m UserSimpleInfo
oapiSourceDeptUserSimpleInfoRecursive dept_id = do
  sub_dept_ids <- fmap (map deptInfoId . fromMaybe []) $ lift $ ExceptT $ oapiGetSubDeptList True dept_id
  mconcat (map (oapiSourceDeptUserSimpleInfo Nothing) $ dept_id : sub_dept_ids)


-- | 收集了 DeptInfo & UserSimpleInfo
data DeptInfoWithUser = DeptInfoWithUser
  { deptInfoUserDeptInfo :: DeptInfo
  , deptInfoUserUsers    :: [UserSimpleInfo]
  }

-- {{{1 instances
instance FromJSON DeptInfoWithUser where
  parseJSON = withObject "DeptInfoWithUser" $ \ o -> do
                DeptInfoWithUser <$> o .: "info"
                                 <*> o .: "users"

instance ToJSON DeptInfoWithUser where
  toJSON (DeptInfoWithUser {..}) =
    object [ "info" .= deptInfoUserDeptInfo
           , "users" .= deptInfoUserUsers
           ]
-- }}}1


oapiGetDeptInfoWithUserForest :: HttpCallMonad env m
                              => Maybe DeptUserSortOrder
                              -> DeptId
                              -> OapiRpcWithAtk m (Maybe (Forest DeptInfoWithUser))
-- {{{1
oapiGetDeptInfoWithUserForest m_dept_user_order parent_dept_id = runExceptT $ runMaybeT $ do
  sub_dept_infos <- MaybeT $ ExceptT $ oapiGetSubDeptList False parent_dept_id
  forM sub_dept_infos $ \ dept_info -> do
    let dept_id = deptInfoId dept_info
    users <- lift $ runConduit $ oapiSourceDeptUserSimpleInfo m_dept_user_order dept_id .| CL.consume
    sub_forest <- lift (ExceptT $ oapiGetDeptInfoWithUserForest m_dept_user_order dept_id)
                    >>= logUnexpectedEmptyResult
                          ("oapiGetDeptInfoWithUserForest should not return Nothing for dept_id: " <> toParamValue dept_id)
                    >>= return . fromMaybe []

    return $ Node (DeptInfoWithUser dept_info users) sub_forest
-- }}}1


oapiGetDeptInfoWithUserTree :: HttpCallMonad env m
                            => Maybe DeptUserSortOrder
                            -> DeptId
                            -> OapiRpcWithAtk m (Maybe (Tree DeptInfoWithUser))
-- {{{1
oapiGetDeptInfoWithUserTree m_dept_user_order dept_id = runExceptT $ runMaybeT $ do
  dept_info <- MaybeT $ ExceptT $ oapiGetDeptSimpleInfo dept_id
  users <- lift $ runConduit $ oapiSourceDeptUserSimpleInfo m_dept_user_order dept_id .| CL.consume
  sub_forest <- MaybeT $ ExceptT $ oapiGetDeptInfoWithUserForest m_dept_user_order dept_id
  return $ Node (DeptInfoWithUser dept_info users) sub_forest
-- }}}1


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
                            <*> (o .:? "roles" .!= [])

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


oapiGetUserDetails' :: HttpCallMonad env m
                    => UserId
                    -> OapiRpcWithAtk m UserDetails
oapiGetUserDetails' user_id =
    oapiGetCallWithAtk "/user/get"
      [ "userid" &= user_id
      ]

oapiGetUserDetails :: HttpCallMonad env m
                   => UserId
                   -> OapiRpcWithAtk m (Maybe UserDetails)
oapiGetUserDetails = fmap oapiNotFoundToMaybe . oapiGetUserDetails'


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
                 => OapiRpcWithAtk m [AdminSimpleInfo]
oapiGetAdminList =
  oapiGetCallWithAtk "/user/get_admin" []
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "admin_list"))



-- | 获取管理员通讯录权限范围
oapiGetAdminDeptList :: HttpCallMonad env m
                     => UserId
                     -> OapiRpcWithAtk m [DeptId]
oapiGetAdminDeptList user_id =
  oapiGetCallWithAtk "/user/get_admin_scope"
    [ "userid" &= user_id
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "dept_ids"))


oapiGetUserIdByUnionId :: HttpCallMonad env m
                       => UnionId
                       -> OapiRpcWithAtk m (Maybe UserId)
oapiGetUserIdByUnionId union_id =
  fmap oapiNotFoundToMaybe $
    oapiGetCallWithAtk "/user/getUseridByUnionid"
      [ "unionid" &= union_id
      ]
      >>= return . fmap (AE.getSingObject (Proxy :: Proxy "userid"))


-- | 查询指定用户的所有上级父部门路径
oapiGetUserParentDeptIds :: HttpCallMonad env m
                         => UserId
                         -> OapiRpcWithAtk m [NonEmpty DeptId]
oapiGetUserParentDeptIds user_id =
  oapiGetCallWithAtk "/department/list_parent_depts"
    [ "userId" &= user_id
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "department"))


-- | 查询部门的所有上级父部门路径
oapiGetDeptParentDeptIds :: HttpCallMonad env m
                         => DeptId
                         -> OapiRpcWithAtk m (NonEmpty DeptId)
oapiGetDeptParentDeptIds dept_id =
  oapiGetCallWithAtk "/department/list_parent_depts_by_dept"
    [ "id" &= dept_id
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "parentIds"))


-- | 删除用户
oapiDeleteUser :: HttpCallMonad env m
               => UserId
               -> OapiRpcWithAtk m ()
oapiDeleteUser user_id =
  oapiGetCallWithAtk "/user/delete"
    [ "userid" &= user_id
    ]
    >>= return . right unUnit


-- vim: set foldmethod=marker:
