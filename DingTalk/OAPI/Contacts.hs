{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.OAPI.Contacts
  ( oapiGetDeptListIds
  , DeptInfo(..), oapiGetDeptList
  , UserSimpleInfo(..), DeptUserSortOrder(..), oapiGetDeptUserSimpleList
  , AdminSimpleInfo(..), AdminLevel(..), oapiGetAdminList, oapiSourceDeptUserSimpleInfo
  , oapiGetAdminDeptList
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Except hiding (mapM_)
import           Data.Aeson           as A
import qualified Data.Aeson.Extra     as AE
import           Data.Conduit
import           Data.Proxy

import DingTalk.OAPI.Basic
import DingTalk.Helpers
-- }}}1


-- | 获取部门ID列表．不受授权范围限制
oapiGetDeptListIds :: HttpCallMonad env m
                   => Maybe DeptID
                   -> ReaderT AccessToken m (Either OapiError [ DeptID ])
oapiGetDeptListIds m_parent_id =
  oapiGetCallWithAtk "/department/list_ids"
    [ "id" &= fromMaybe rootDeptID m_parent_id
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "sub_dept_id_list"))



data DeptInfo = DeptInfo
  { deptInfoId          :: DeptID
  , deptInfoParent      :: DeptID
  , deptInfoName        :: Text
  , deptInfoCreateGroup :: Bool
  , deptInfoAutoAddUser :: Bool
  }

-- {{{1 instances
instance FromJSON DeptInfo where
  parseJSON = withObject "DeptInfo" $ \ o ->
    DeptInfo <$> o .: "id"
             <*> o .: "parentid"
             <*> o .: "name"
             <*> o .: "createDeptGroup"
             <*> o .: "autoAddUser"
-- }}}1



-- | 获取部门列表．
oapiGetDeptList :: HttpCallMonad env m
                   => Bool
                   -> Maybe DeptID
                   -> ReaderT AccessToken m (Either OapiError [DeptInfo])
oapiGetDeptList recursive m_parent_id =
  oapiGetCallWithAtk "/department/list"
    [ "id" &= fromMaybe rootDeptID m_parent_id
    , "fetch_child" &= recursive
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "department"))


data UserSimpleInfo = UserSimpleInfo
  {_userSimpleInfoId    :: UserID
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


-- | 获取部门用户
oapiGetDeptUserSimpleList :: HttpCallMonad env m
                          => Maybe DeptID
                          -> Maybe DeptUserSortOrder
                          -> Maybe (Int, Int)
                          -- ^ paging: size, offset
                          -> ReaderT AccessToken m (Either OapiError ([UserSimpleInfo], Bool))
-- {{{1
oapiGetDeptUserSimpleList m_parent_id m_sort_order m_size_offset = runExceptT $ do
  jv <- ExceptT $ oapiGetCallWithAtk "/user/simplelist" params
  user_list <- getJsonField jv "userlist"
  has_more <- getJsonField jv "hasMore"
  return (user_list, has_more)
  where
    params = catMaybes [ Just $ "id" &= fromMaybe rootDeptID m_parent_id
                       , "size" &?= fmap fst m_size_offset
                       , "offset" &?= fmap snd m_size_offset
                       , "order" &?= m_sort_order
                       ]
-- }}}1


oapiSourceDeptUserSimpleInfo :: HttpCallMonad env m
                             => Maybe DeptID
                             -> Maybe DeptUserSortOrder
                             -> Int
                             -> Source (ExceptT OapiError (ReaderT AccessToken m)) UserSimpleInfo
-- {{{1
oapiSourceDeptUserSimpleInfo m_parent_id m_sort_order size = src Nothing
  where
    src m_offset = do
      let m_size_offset = (size,) <$> m_offset
      (user_list, has_more) <- lift $ ExceptT $ oapiGetDeptUserSimpleList m_parent_id m_sort_order m_size_offset
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
  , adminSimpleInfoId    :: UserID
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
                    => UserID
                    -> ReaderT AccessToken m (Either OapiError [DeptID])
oapiGetAdminDeptList user_id =
  oapiGetCallWithAtk "/user/get_admin_scope"
    [ "userid" &= user_id
    ]
    >>= return . fmap (AE.getSingObject (Proxy :: Proxy "dept_ids"))


-- vim: set foldmethod=marker:
