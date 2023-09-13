{-# LANGUAGE ScopedTypeVariables #-}
module DingTalk.VxAPI.Drive where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=))
import           Control.Monad.Logger
import           Control.Monad.Except hiding (mapM_, forM_)
import           Data.Aeson           as A hiding (Options)
-- import qualified Data.Aeson.Extra     as AE
import           Data.Aeson.TH                 (deriveJSON)
import           Data.Aeson.Extra.SingObject (getSingObject)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Monoid (Endo(..))
import           Data.Time
import           Data.Default (Default(..))
import           System.FilePath.Posix
import           Data.Proxy
import           Data.List (dropWhileEnd)
import qualified Data.List.NonEmpty as LNE
import           Data.List.NonEmpty (nonEmpty, NonEmpty(..))
import           Network.Wreq hiding (Proxy)
import qualified Network.Wreq.Session as WS
import           Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))

import DingTalk.Types
import DingTalk.Helpers
import DingTalk.VxAPI.Basic
-- }}}1


data DriveSpaceInfo = DriveSpaceInfo
  { spaceInfoSpaceId        :: DriveSpaceId
  , spaceInfoSpaceName      :: Text
  , spaceInfoSpaceType      :: DriveSpaceType
  , spaceInfoQuota          :: Int64
  , spaceInfoUsedQuota      :: Int64
  , spaceInfoPermissionMode :: Text
  , spaceInfoCreateTime     :: ZonedTime
  , spaceInfoModifyTime     :: ZonedTime
  }
  deriving (Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 9 }) ''DriveSpaceInfo)


data DriveSpaceListResp = DriveSpaceListResp
  { dslrSpaces    :: [DriveSpaceInfo]
  , dslrNextToken :: Maybe Text
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 4 }) ''DriveSpaceListResp)


apiVxGetDriveSpacesList
  :: HttpCallMonad env m
  => UnionId
  -> DriveSpaceType
  -> Maybe Text
  -> Int
  -> ApiVxRpcWithAtk m DriveSpaceListResp
apiVxGetDriveSpacesList user_id space_type m_next_token batch_size =
  apiVxGetCall "v1.0" "/drive/spaces"
    (catMaybes
        [ "unionId" &!= user_id
        , "spaceType" &!= space_type
        , "nextToken" &?= m_next_token
        , "maxResults" &!= batch_size
        ]
    )


apiVxSourceDriveSpaces
  :: HttpCallMonad env m
  => Float  -- ^ seconds. delay between iterations
  -> UnionId
  -> DriveSpaceType
  -> ApiVxRpcWithAtkSource m DriveSpaceInfo
apiVxSourceDriveSpaces delay_sec union_id space_type =
  apiVxSourceByStep
    dslrSpaces
    (join . nullTextAsNothing . dslrNextToken)
    delay_sec
    (\ m_next_token -> apiVxGetDriveSpacesList union_id space_type m_next_token 20)



data DriveEntryInfo = DriveEntryInfo
  { driveEntryInfoId         :: DriveEntryId
  , driveEntryInfoSpaceId    :: DriveSpaceId
  , driveEntryInfoParentId   :: DriveEntryId
  , driveEntryInfoType       :: DriveEntryType
  , driveEntryInfoName       :: Text
  , driveEntryInfoSize       :: Maybe Int64
  , driveEntryInfoPath       :: Text
  , driveEntryInfoVersion    :: Int64
  , driveEntryInfoStatus     :: DriveEntryStatus
  , driveEntryInfoExtension  :: Maybe Text
  , driveEntryInfoCreatorId  :: UnionId
  , driveEntryInfoModifierId :: UnionId
  , driveEntryInfoUuid       :: DriveEntryUuid
  }
  deriving (Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 14 }) ''DriveEntryInfo)

data GetDriveEntriesResp = GetDriveEntriesResp
  { getDriveEntriesRespDentries :: [DriveEntryInfo]
  , getDriveEntriesRespNextToken :: Maybe Text
  }
$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 19 }) ''GetDriveEntriesResp)


data VxGetDriveEntriesOpts = VxGetDriveEntriesOpts
  { vxgdeOrderBy       :: Maybe DriveEntryOrderBy
  , vxgdeOrder         :: Maybe DriveEntryOrder
  , vxgdeWithThumbnail :: Maybe Bool
  }

instance Default VxGetDriveEntriesOpts where
  def = VxGetDriveEntriesOpts
    { vxgdeOrderBy       = Nothing
    , vxgdeOrder         = Nothing
    , vxgdeWithThumbnail = Nothing
    }


apiVxGetDriveEntriesUnder
  :: HttpCallMonad env m
  => UnionId
  -> DriveSpaceId
  -> DriveEntryId -- ^ parent id
  -> VxGetDriveEntriesOpts
  -> Maybe Text
  -> Int
  -> ApiVxRpcWithAtk m GetDriveEntriesResp
apiVxGetDriveEntriesUnder union_id space_id parent_id list_opts m_next_token batch_size0 =
  apiVxGetCall "v1.0" url_path
    (catMaybes
        [ "unionId" &!= union_id
        , "parentId" &!= parent_id
        , "nextToken" &?= m_next_token
        , "maxResults" &!= batch_size
        , "orderBy" &?= vxgdeOrderBy list_opts
        , "order" &?= vxgdeOrder list_opts
        , "withThumbnail" &?= (asText . bool "false" "true" <$> vxgdeWithThumbnail list_opts)
        ]
    )

  where
    url_path = unpack $ "/storage/spaces/" <> toParamValue space_id <> "/dentries"
    batch_size = min 50 batch_size0


-- | 列出在父节点下的子节点
apiVxSourceDriveEntriesUnder
  :: HttpCallMonad env m
  => Float  -- ^ seconds. delay between iterations
  -> UnionId
  -> DriveSpaceId
  -> DriveEntryId -- ^ parent id
  -> VxGetDriveEntriesOpts
  -> ApiVxRpcWithAtkSource m DriveEntryInfo
apiVxSourceDriveEntriesUnder delay_sec union_id space_id parent_id list_opts =
  apiVxSourceByStep
    getDriveEntriesRespDentries
    (join . nullTextAsNothing . getDriveEntriesRespNextToken)
    delay_sec
    (\ m_next_token -> apiVxGetDriveEntriesUnder union_id space_id parent_id list_opts m_next_token 50)


apiVxSourceDriveEntriesUnderPath
  :: HttpCallMonad env m
  => Float  -- ^ seconds. delay between iterations
  -> UnionId
  -> DriveSpaceId
  -> FilePath
  -> VxGetDriveEntriesOpts
  -> ApiVxRpcWithAtkSourceT
        (ExceptT
          (DriveEntryId, FilePath)  -- 最后成功的节点，及其之下想找但找不到的节点
        )
        m
        DriveEntryInfo
apiVxSourceDriveEntriesUnderPath delay_sec union_id space_id parent_path list_opts = do
  parent_dentry_id <-
    if parent_path == "/"
       then pure driveRootEntryId
       else lift $ fmap driveEntryInfoId $ ExceptT $ apiVxGetDriveEntryByPath union_id space_id parent_path

  transPipe lift $ apiVxSourceDriveEntriesUnder delay_sec union_id space_id parent_dentry_id list_opts


driveRootEntryId :: DriveEntryId
driveRootEntryId = DriveEntryId "0"


-- | 获取文件或文件夹信息
apiVxGetDriveEntry
  :: HttpCallMonad env m
  => UnionId
  -> DriveSpaceId
  -> DriveEntryId -- ^ parent id
  -> ApiVxRpcWithAtk m DriveEntryInfo
apiVxGetDriveEntry union_id space_id entry_id =
    fmap (fmap $ getSingObject (Proxy @"dentry")) $ do
      apiVxPostCall "v1.0" url_path
        (catMaybes
              [ "unionId" &!= union_id
              ]
          )
        (object [])  --- TODO: optional 'options' not implemented
  where
    url_path = unpack $ "/storage/spaces/" <> toParamValue space_id <> "/dentries/" <> toParamValue entry_id <> "/query"


apiVxGetDriveEntryMaybe
  :: HttpCallMonad env m
  => UnionId
  -> DriveSpaceId
  -> DriveEntryId -- ^ parent id
  -> ApiVxRpcWithAtkExcept m (Maybe DriveEntryInfo)
apiVxGetDriveEntryMaybe union_id space_id entry_id =
  apiVxNotFoundToMaybeExcept $ ExceptT $ apiVxGetDriveEntry union_id space_id entry_id


-- | 根据路径获取节点信息
-- CAUTION: 为找到节点的 dentryid，需逐级从权目录找父目录的 dentryid , 但有可能某级节点的父目录用户无权限访问
-- 返回:
-- * Left _: 搜索到这个节点不能继续了，有两种可能：这个节点是文件，另一种是下一级节点不存在（或权限不够）
-- * Right _: 成功
apiVxGetDriveEntryByPath
  :: forall env m. HttpCallMonad env m
  => UnionId
  -> DriveSpaceId
  -> FilePath
  -> ApiVxRpcWithAtkExcept m
        (Either
          (DriveEntryId, FilePath)  -- 最后成功的节点，及其之下想找但找不到的节点
          DriveEntryInfo
        )
-- {{{1
apiVxGetDriveEntryByPath union_id space_id path = runExceptT $ do
  case nonEmpty (splitPath path) of
    Nothing -> get_root
    Just path_pieces1 -> do
      case path_pieces1 of
        "/" :| path_pieces2 -> do
          case nonEmpty path_pieces2 of
            Nothing -> get_root
            Just path_pieces3 -> get_by_path_pieces path_pieces3

        _ -> do
          -- does not start with "/", which means this is not a absolute path
          $logErrorS logSourceName $ "not an absolute path: " <> fromString path
          throwError (driveRootEntryId, LNE.head path_pieces1)
  where
    -- XXX: 这个调用会出错，而且没有有效的报错信息
    get_root = lift $ ExceptT $ apiVxGetDriveEntry union_id space_id driveRootEntryId

    get_by_path_pieces path_pieces = do
      parent_id <- foldlM go driveRootEntryId (LNE.init path_pieces)
      let last_part = LNE.last path_pieces
      entries <- lift $ runConduit $ apiVxSourceDriveEntriesUnder loop_delay union_id space_id parent_id def .| CL.consume
      case find ((== fromString last_part) . driveEntryInfoName) entries of
        Just entry -> pure entry
        Nothing -> throwError (parent_id, last_part)

    go pid path_piece0 = do
      let path_piece = dropWhileEnd isPathSeparator path_piece0
      entries <- lift $ runConduit $ apiVxSourceDriveEntriesUnder loop_delay union_id space_id pid def .| CL.consume
      case find ((== fromString path_piece) . driveEntryInfoName) entries of
        Just entry -> do
          if driveEntryInfoType entry == DriveEntryTypeFile
             then throwError (driveEntryInfoId entry, path_piece)
             else pure $ driveEntryInfoId entry
        Nothing -> throwError (pid, path_piece)

    loop_delay = 0.01
-- }}}1


-- | 根据路径获取节点信息
-- 与 apiVxGetDriveEntryByPath 不同的是，调用者可以提供一些额外的手段来读取上级目录的信息
-- 返回的报错信息仅是文字描述
apiVxGetDriveEntryByPath'
  :: forall env m. HttpCallMonad env m
  => (DriveSpaceId -> FilePath -> ApiVxRpcWithAtkExcept m (Maybe (DriveEntryId, DriveEntryType)))
  -- ^ 某一目录的节点信息: 无法提供时返回 Nothing
  -> (DriveSpaceId -> DriveEntryInfo -> ApiVxRpcWithAtkExcept m ())
  -- ^ 保存某一目录的节点信息
  -> UnionId
  -> DriveSpaceId
  -> FilePath
  -> ApiVxRpcWithAtkExcept m (Either Text DriveEntryInfo)
-- {{{1
apiVxGetDriveEntryByPath' get_cached_info save_dentry_info union_id space_id path' = runExceptT $ do
  (dir_entry_id, entry_type) <-
    lift (get_cached_info space_id dir_name)
      >>= flip maybe pure
            ( if dir_name == "/"
                 then pure (driveRootEntryId, DriveEntryTypeFolder)
                 else do
                      info <- ExceptT $ apiVxGetDriveEntryByPath' get_cached_info save_dentry_info union_id space_id dir_name
                      lift $ save_dentry_info space_id info

                      pure $ (driveEntryInfoId &&& driveEntryInfoType) info
            )

  unless (entry_type == DriveEntryTypeFolder) $ do
    throwError $ "Not a folder: " <> fromString dir_name

  entries <- lift $ runConduit $ apiVxSourceDriveEntriesUnder loop_delay union_id space_id dir_entry_id def .| CL.consume
  lift $ mapM_ (save_dentry_info space_id) entries

  case find ((== fromString base_name) . driveEntryInfoName) entries of
    Just entry -> pure entry
    Nothing -> throwError $ "Not found: " <> fromString base_name

  where
    path = normalise path'
    (dir_name', base_name) = splitFileName path
    dir_name = fromMaybe dir_name' $ stripSuffix "/" dir_name'
    loop_delay = 0.01
-- }}}1


-- | 类似于 apiVxGetDriveEntryByPath，但只返回 dentryid 之类最核心信息
-- 因此可以先主动调用调用者提供的 get_cached_info
apiVxGetDriveEntryCoreInfoByPath'
  :: forall env m. HttpCallMonad env m
  => (DriveSpaceId -> FilePath -> ApiVxRpcWithAtkExcept m (Maybe (DriveEntryId, (DriveEntryType, DriveEntryId))))
  -- ^ 某一目录的节点信息: 无法提供时返回 Nothing
  -> (DriveSpaceId -> DriveEntryInfo -> ApiVxRpcWithAtkExcept m ())
  -- ^ 保存某一目录的节点信息
  -> UnionId
  -> DriveSpaceId
  -> FilePath
  -> ApiVxRpcWithAtkExcept m (Either Text (DriveEntryId, (DriveEntryType, DriveEntryId)))
-- {{{1
apiVxGetDriveEntryCoreInfoByPath' get_cached_info save_dentry_info union_id space_id path' = runExceptT $ do
  lift (get_cached_info space_id path)
    >>= flip maybe pure
            (fmap (driveEntryInfoId &&& (driveEntryInfoType &&& driveEntryInfoParentId)) $
              ExceptT $ apiVxGetDriveEntryByPath' get_cached_info2 save_dentry_info union_id space_id path)
  where
    path = normalise path'
    get_cached_info2 = (fmap (fmap (second fst)) .) . get_cached_info


data HeaderSignatureInfo = HeaderSignatureInfo
  { headerSignatureInfoExpirationSeconds    :: Int
  , headerSignatureInfoHeaders              :: Map Text Text
  , headerSignatureInfoRegion               :: Text
  , headerSignatureInfoResourceUrls         :: [ Text ]
  , headerSignatureInfoInternalResourceUrls :: [ Text ]
  }
$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 19 }) ''HeaderSignatureInfo)


data DriveEntryDownloadInfo = DriveEntryDownloadInfo
  { driveEntryDownloadInfoProtocol :: Text
  , driveEntryDownloadInfoHeaderSignatureInfo :: Maybe HeaderSignatureInfo
  }
$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 22 }) ''DriveEntryDownloadInfo)


-- | 获取文件下载信息
apiVxGetDriveEntryDownload
  :: HttpCallMonad env m
  => UnionId
  -> DriveSpaceId
  -> DriveEntryId -- ^ parent id
  -> ApiVxRpcWithAtk m DriveEntryDownloadInfo
apiVxGetDriveEntryDownload union_id space_id entry_id =
  apiVxPostCall "v1.0" url_path
    (catMaybes
          [ "unionId" &!= union_id
          ]
      )
    (object [])  --- TODO: optional 'options' not implemented
  where
    url_path = unpack $ "/storage/spaces/" <> toParamValue space_id <> "/dentries/" <> toParamValue entry_id <> "/downloadInfos/query"


-- | 获取文件下载信息后直接访问 URL 下载
apiVxFetchDriveEntry
  :: HttpCallMonad env m
  => UnionId
  -> DriveSpaceId
  -> DriveEntryId -- ^ parent id
  -> ApiVxRpcWithAtkExcept m (Either Text (Response LByteString))
apiVxFetchDriveEntry union_id space_id entry_id = runExceptT $ do
  lift (ExceptT $ apiVxGetDriveEntryDownload union_id space_id entry_id)
    >>= ExceptT . lift . lift . apiVxFetchDriveEntryByDownloadInfo


-- | Download file by DriveEntryDownloadInfo
-- Maybe throw excepton
apiVxFetchDriveEntryByDownloadInfo
  :: (MonadReader env m, HasWreqSession env, MonadLogger m, MonadIO m)
  => DriveEntryDownloadInfo
  -> m (Either Text (Response LByteString))
apiVxFetchDriveEntryByDownloadInfo download_info = runExceptT $ do
  case driveEntryDownloadInfoHeaderSignatureInfo download_info of
    Nothing -> do
      $logErrorS logSourceName $ "no signature info, protocol was: " <> driveEntryDownloadInfoProtocol download_info
      throwError "no signature info"

    Just sign_info -> do
      case nonEmpty (headerSignatureInfoResourceUrls sign_info) of
        Nothing -> do
          throwError "no resource url in signature info"

        Just urls -> do
          let url = LNE.head urls
          let add_headers = appEndo $ mconcat $
                flip map (mapToList $ headerSignatureInfoHeaders sign_info) $
                  \ (k, v) -> Endo $ header (fromString $ unpack k) .~ [ encodeUtf8 v ]

          let check_response request resp0 = do
                let status_code = resp0 ^. responseStatus . statusCode
                unless (status_code == 200) $ do
                  start_bs <- liftIO $ resp0 ^. responseBody
                  throwIO $ HttpExceptionRequest request (StatusCodeException (fmap (const ()) resp0) start_bs)

          let opts = defaults & add_headers
                              & checkResponse .~ Just check_response

          sess <- lift $ asks getWreqSession
          liftIO $ WS.getWith opts sess (unpack url)


-- vim: set foldmethod=marker:
