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
import           System.FilePath
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
                start_bs <- liftIO $ resp0 ^. responseBody
                throwIO $ HttpExceptionRequest request (StatusCodeException (fmap (const ()) resp0) start_bs)

          let opts = defaults & add_headers
                              & checkResponse .~ Just check_response

          sess <- lift $ asks getWreqSession
          liftIO $ WS.getWith opts sess (unpack url)


-- vim: set foldmethod=marker:
