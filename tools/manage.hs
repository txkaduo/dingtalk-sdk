module Main where

-- {{{1 imports
import           ClassyPrelude
#if MIN_VERSION_base(4, 13, 0)
import           Control.Monad (MonadFail(..))
#else
#endif
import           Control.Lens         hiding ((.=), argument)
import           Control.Monad.Trans.Except
import           Control.Monad.Logger
import           Data.Aeson (ToJSON)
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default (def)
import           Data.List.NonEmpty (some1, NonEmpty)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Tree
import           Network.HTTP.Client   (streamFile)
import           Network.Wreq         (responseBody, responseHeader, responseStatus, statusCode)
import qualified Network.Wreq.Session  as WS
import           Options.Applicative
import           System.IO             (hPutStrLn)
import           System.Log.FastLogger (LoggerSet, newStderrLoggerSet,
                                        pushLogStr)
import           System.Exit
import           Text.Show.Unicode (ushow)
import qualified Data.Set.NonEmpty as NES

import DingTalk
import DingTalk.Helpers

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Monad.Trans.Control
#endif
-- }}}1

data TimestampSpec = TS_TimeOfDay TimeOfDay
                   | TS_Day Day
                   | TS_LocalTime LocalTime
                   deriving (Show)

readTimeOfDay :: ReadM TimeOfDay
readTimeOfDay = str >>= parseTimeM True defaultTimeLocale "%R"

readLocalTime :: ReadM LocalTime
readLocalTime = str >>= parseTimeM True defaultTimeLocale "%F %R"

readDay :: ReadM Day
readDay = str >>= parseTimeM True defaultTimeLocale "%Y-%m-%d"

readTimestampSpec :: ReadM TimestampSpec
readTimestampSpec = fmap TS_TimeOfDay readTimeOfDay
                <|> fmap TS_Day readDay
                <|> fmap TS_LocalTime readLocalTime

timestampFromSpec :: MonadIO m => TimestampSpec -> m Timestamp
-- {{{1
timestampFromSpec spec = do
  tz <- liftIO getCurrentTimeZone
  fmap (timestampFromPOSIXTime . utcTimeToPOSIXSeconds . localTimeToUTC tz) $
    case spec of
      TS_LocalTime lt -> pure lt
      TS_Day d -> pure $ LocalTime d midnight
      TS_TimeOfDay tod -> do
        now <- liftIO getCurrentTime
        let today = localDay $ utcToLocalTime tz now
        pure $ LocalTime today tod
-- }}}1


data ManageCmd = Scopes
               | SearchUser Text
               | ShowUserDetailsById UserId
               | SearchDept Text
               | DeptTree (Maybe DeptId)
               | DeptTreeWithUser (Maybe DeptId)
               | ShowDeptDetails DeptId
               | UploadMedia MediaType FilePath
               | DownloadMedia MediaId
               | SearchProcess (Maybe Text)
               | ListProcessInstId ProcessCode Int (Maybe UserId)
               | ShowProcessInst ProcessInstanceId
               | ProcessInstAddComment ProcessInstanceId UserId Text [Text]
               | ShowUserProcessToDo UserId
               | StartProcess ProcessCode UserId (Maybe DeptId) (NonEmpty UserId) [(Text, Text)]
               | DeleteCallback
               | PunchResult Day Day (NonEmpty UserId)
               | PunchDetails Day Day (NonEmpty UserId)
               | VisibleReportTemplates (Maybe UserId)
               | QueryReports TimestampSpec TimestampSpec (Maybe UserId) (Maybe Text)
               | ListDriveSpaces UnionId DriveSpaceType
               | ListDriveEntries UnionId DriveSpaceId DriveEntryId
               | ListDriveEntriesByPath UnionId DriveSpaceId FilePath
               | GetDriveEntry UnionId DriveSpaceId DriveEntryId
               | GetDriveEntryByPath UnionId DriveSpaceId FilePath
               | DownloadDriveEntryDownload UnionId DriveSpaceId DriveEntryId
               deriving (Show)

data Options = Options
  { optVerbose    :: Int
  , optAppKey     :: AppKey
  , optAppSecret  :: AppSecret
  , optApiVersion :: Int
  , optCommand    :: ManageCmd
  }

optionsParse :: Parser Options
-- {{{1
optionsParse = Options
                <$> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")
                <*> fmap (AppKey . fromString) (strOption (long "app-key" <> short 'a' <> metavar "APP_KEY"))
                <*> fmap (AppSecret . fromString) (strOption (long "app-secret" <> short 's' <> metavar "APP_SECRET"))
                <*> (option auto
                        $ long "api-version" <> short 'V' <> value 1
                        <> metavar "VERSION"
                        <> help "API Version, 0 for old, 1 for current")
                <*> manageCmdParser
-- }}}1


manageCmdParser :: Parser ManageCmd
-- {{{1
manageCmdParser = subparser $
  command "scopes"
    (info (helper <*> pure Scopes)
          (progDesc "获取 AccessToken 并看权限范围")
    )
  <> command "search-user"
    (info (helper <*> (pure SearchUser <*> fmap fromString (argument str (metavar "USER_NAME"))))
          (progDesc "按用户名搜索钉钉用户")
    )
  <> command "show-user-details-by-id"
    (info (helper <*> (pure ShowUserDetailsById <*> fmap (UserId . fromString) (argument str (metavar "USER_NAME"))))
          (progDesc "显示指定UserId用户的详情")
    )
  <> command "search-dept"
    (info (helper <*> (pure SearchDept <*> fmap fromString (argument str (metavar "DEPT_NAME"))))
          (progDesc "按名搜索部门")
    )
  <> command "dept-tree"
    (info (helper <*> (pure DeptTree <*> optional (fmap DeptId (argument auto (metavar "DEPT_ID")))))
          (progDesc "显示部门的节点树")
    )
  <> command "dept-tree-with-user"
    (info (helper <*> (pure DeptTreeWithUser <*> optional (fmap DeptId (argument auto (metavar "DEPT_ID")))))
          (progDesc "显示部门的带用户列表的节点树")
    )
  <> command "show-dept"
    (info (helper <*> (pure ShowDeptDetails <*> fmap DeptId (argument auto (metavar "DEPT_ID"))))
          (progDesc "显示部门信息")
    )
  <> command "upload-media"
    (info (helper <*> ( UploadMedia
                          <$> (argument (enumStringReader "media type") (metavar "MEDIA_TYPE"))
                          <*> (argument str (metavar "FILE"))
                      )
          )
          (progDesc "上传文件，并取得 media id")
    )
  <> command "download-media"
    (info (helper <*> ( DownloadMedia
                          <$> (MediaId . fromString <$> argument str (metavar "MEDIA_ID"))
                      )
          )
          (progDesc "下载media_id对应的文件, 内容直接从 stdout 输出")
    )
  <> command "search-process"
    (info (helper <*> ( SearchProcess
                          <$> optional (fromString <$> argument str (metavar "TITLE"))
                      )
          )
          (progDesc "查找审批流程")
    )
  <> command "list-process-inst-id"
    (info (helper <*> ( ListProcessInstId
                          <$> (ProcessCode . fromString <$> argument str (metavar "PROCESS_CODE"))
                          <*> argument auto (metavar "SECONDS")
                          <*> optional (UserId . fromString <$> argument str (metavar "USER_ID"))
                      )
          )
          (progDesc "列出审批实例id")
    )
  <> command "show-process-inst"
    (info (helper <*> ( ShowProcessInst
                          <$> (ProcessInstanceId . fromString <$> argument str (metavar "PROCESS_INSTANCE_ID"))
                      )
          )
          (progDesc "显示审批实例信息")
    )
  <> command "process-inst-add-comment"
    (info (helper <*> ( ProcessInstAddComment
                          <$> (ProcessInstanceId . fromString <$> argument str (metavar "PROCESS_INSTANCE_ID"))
                          <*> (UserId . fromString <$> argument str (metavar "USER_ID"))
                          <*> argument str (metavar "TEXT")
                          <*> many (argument str (metavar "PHOTO_URL"))
                      )
          )
          (progDesc "显示审批实例信息")
    )
  <> command "show-process-todo"
    (info (helper <*> (pure ShowUserProcessToDo <*> fmap UserId (argument str (metavar "USER_ID"))))
          (progDesc "显示用户待审批数量")
    )
  <> command "start-process"
    (info (helper <*> (pure StartProcess <*> fmap ProcessCode (argument str (metavar "PROCESS_CODE"))
                                         <*> fmap UserId (argument str (metavar "USER_ID"))
                                         <*> optional (DeptId <$> option auto (long "dept" <> short 'd' <> metavar "DEPT_ID"))
                                         <*> some1 (UserId . fromString <$> strOption (long "approver" <> short 'A' <> metavar "APPROVER_USER"))
                                         <*> some (argument nameValueParser (metavar "NAME=VALUE"))
                      ))
          (progDesc "发起一个审批，仅用于测试")
    )
  <> command "delete-callback"
      (info (helper <*> pure DeleteCallback)
        (progDesc "删除已注册回调")
      )
  <> command "punch-result"
      (info (helper <*> pure PunchResult <*> argument auto (metavar "BEGIN_DATE")
                                         <*> argument auto (metavar "END_DATE")
                                         <*> some1 (fmap UserId (argument str (metavar "USER_ID")))
            )
        (progDesc "打卡结果")
      )
  <> command "punch-details"
      (info (helper <*> pure PunchDetails <*> argument auto (metavar "BEGIN_DATE")
                                          <*> argument auto (metavar "END_DATE")
                                          <*> some1 (fmap UserId (argument str (metavar "USER_ID")))
            )
        (progDesc "打卡详情")
      )
  <> command "visible-report-templates"
      (info (helper <*> pure VisibleReportTemplates <*> optional (UserId <$> option auto (long "user" <> metavar "USER_ID"))
            )
            (progDesc "可见的日志模板")
      )
  <> command "query-reports"
      (info (helper <*> pure QueryReports
                    <*> argument readTimestampSpec (metavar "START_TIME")
                    <*> argument readTimestampSpec (metavar "END_TIME")
                    <*> optional (UserId <$> option auto (long "user" <> metavar "USER_ID"))
                    <*> optional (fromString <$> strOption (long "name" <> metavar "TEMPLATE_NAME"))
            )
            (progDesc "取得日志")
      )

  <> command "list-drive-spaces"
      (info (helper <*> (pure ListDriveSpaces <*> fmap UnionId (argument str (metavar "USER_UNION_ID"))
                                           <*> (argument (enumStringReader "space type") (metavar "SPACE_TYPE"))
                        ))
            (progDesc "列出钉盘空间")
      )
  <> command "list-drive-entries"
      (info (helper <*> (pure ListDriveEntries
                                <*> fmap UnionId (argument str (metavar "USER_UNION_ID"))
                                <*> fmap DriveSpaceId (argument str (metavar "SPACE_ID"))
                                <*> fmap DriveEntryId (argument str (metavar "PARENT_ENTRY_ID"))
                        ))
            (progDesc "列出钉盘目录下的文件和文件夹")
      )

  <> command "list-drive-entries-by-path"
      (info (helper <*> (pure ListDriveEntriesByPath
                                <*> fmap UnionId (argument str (metavar "USER_UNION_ID"))
                                <*> fmap DriveSpaceId (argument str (metavar "SPACE_ID"))
                                <*> argument str (metavar "PATH")
                        ))
            (progDesc "列出钉盘目录下的文件和文件夹")
      )
  <> command "get-drive-entry"
      (info (helper <*> (pure GetDriveEntry
                                <*> fmap UnionId (argument str (metavar "USER_UNION_ID"))
                                <*> fmap DriveSpaceId (argument str (metavar "SPACE_ID"))
                                <*> fmap DriveEntryId (argument str (metavar "ENTRY_ID"))
                        ))
            (progDesc "取钉盘空间文件或文件夹的信息")
      )
  <> command "get-drive-entry-by-path"
      (info (helper <*> (pure GetDriveEntryByPath
                                <*> fmap UnionId (argument str (metavar "USER_UNION_ID"))
                                <*> fmap DriveSpaceId (argument str (metavar "SPACE_ID"))
                                <*> argument str (metavar "PATH")
                        ))
            (progDesc "取钉盘空间文件或文件夹的信息")
      )
  <> command "download-drive-entry"
      (info (helper <*> (pure DownloadDriveEntryDownload
                                <*> fmap UnionId (argument str (metavar "USER_UNION_ID"))
                                <*> fmap DriveSpaceId (argument str (metavar "SPACE_ID"))
                                <*> fmap DriveEntryId (argument str (metavar "ENTRY_ID"))
                        ))
            (progDesc "下载钉盘空间文件")
      )
-- }}}1


-- | parse 'name=value'
nameValueParser :: ReadM (Text, Text)
-- {{{1
nameValueParser = do
  t <- str
  let (name, value1) = T.breakOn "=" t
  value <- maybe (readerError $ "no '=' found in " <> unpack t) return $ T.stripPrefix "=" value1
  when (null name) $ readerError $ "name part is empty in " <> unpack t
  return (name, value)
-- }}}1


enumStringReader :: (Enum a, Bounded a, ParamValue a)
                 => String
                 -> ReadM a
-- {{{1
enumStringReader type_prompt = do
  s <- str
  maybe (fail $ "cannot parse as " <> type_prompt <> ": " <> s)
        return
        (parseEnumParamValueText $ fromString s)
-- }}}1

start :: (MonadLogger m, MonadIO m, MonadUnliftIO m, RemoteCallThrottle t)
      => Options
      -> HttpApiRunEnv t
      -> m ()
-- {{{1
start opts api_env = flip runReaderT api_env $ do
  atk <- api_call_or_abort2 "GetAccessToken" (oapiGetAccessToken corp_id corp_secret) (apiVxGetAccessToken corp_id corp_secret)

  case optCommand opts of
    Scopes -> do
      err_or_res <- flip runReaderT atk $ oapiGetAccessTokenScopes
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetAccessTokenScopes failed: " <> utshow err
          liftIO exitFailure

        Right resp -> do
          putStrLn $ "AccessToken is: " <> unAccessToken atk
          putStrLn $ utshow resp

    SearchUser name -> do
      err_or_res <- flip runReaderT atk $ filterUserByNameInDept rootDeptId name

      case err_or_res of
        Left err -> do
          $logError $ "some api failed: " <> utshow err
          liftIO exitFailure

        Right user_details_list -> do
          forM_ user_details_list $ \ user_details -> do
            putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty user_details
              {-
            putStrLn $ "User Id: " <> unUserId (userDetailsUserId user_details)
            putStrLn $ "User Name: " <> userDetailsName user_details
            putStrLn $ "User Roles: " <> utshow (userDetailsRoles user_details)
            putStrLn $ "User Hire Date: " <> utshow (userDetailsHiredTime user_details)
            putStrLn $ "Dept Ids: " <> utshow (userDetailsDepartments user_details)
            --}

    SearchDept name -> do
      err_or_res <- flip runReaderT atk $ oapiGetSubDeptList True rootDeptId

      case err_or_res of
        Left err -> do
          $logError $ "some api failed: " <> utshow err
          liftIO exitFailure

        Right Nothing -> do
          putStrLn "oapiGetSubDeptList failed for rootDeptId: NOT FOUND"

        Right (Just all_dept_info_list) -> do
          let dept_info_list = filter (isInfixOf name . deptInfoName) all_dept_info_list
          if null dept_info_list
             then putStrLn "NOT FOUND"
             else forM_ dept_info_list $ \ dept_info -> do
                    putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty dept_info

    ShowUserDetailsById user_id -> do
      err_or_res <- flip runReaderT atk $ runExceptT $ do
                        ExceptT $ oapiGetUserDetails user_id

      case err_or_res of
        Left err -> do
          $logError $ "some api failed: " <> utshow err
          liftIO exitFailure

        Right Nothing -> do
          putStrLn "NOT FOUND"

        Right (Just user_details) -> do
          putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty user_details

    ShowDeptDetails dept_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetDeptDetails dept_id
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetDeptDetails failed: " <> utshow err
        Right Nothing -> putStrLn "NOT FOUND"
        Right (Just details) -> do
          putStrLn $ "Name: " <> deptDetailsName details
          putStrLn $ "Manager User Ids" <> utshow (deptDetailsManagerUserIds details)
          putStrLn $ "Source Identifier: " <> fromMaybe "" (deptDetailsSourceIdentifier details)

    DeptTree m_dept_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetDeptInfoTree (fromMaybe rootDeptId m_dept_id)
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetDeptInfoSubForest failed: " <> utshow err
        Right Nothing -> putStrLn "NOT FOUND"
        Right (Just tree) -> do
          let str_tree = fmap (unpack . decodeUtf8 . AP.encodePretty) tree
          putStrLn $ fromString $ drawTree str_tree

    DeptTreeWithUser m_dept_id -> do
      let dept_id = fromMaybe rootDeptId m_dept_id
      err_or_res <- flip runReaderT atk $ oapiGetDeptInfoWithUserTree Nothing dept_id
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetDeptInfoWithUserForest failed: " <> utshow err
        Right Nothing -> putStrLn "NOT FOUND"
        Right (Just tree) -> do
          let str_tree = fmap (unpack . decodeUtf8 . AP.encodePretty) tree
          putStrLn $ fromString $ drawTree str_tree

    UploadMedia media_type file_path -> do
      file_body <- liftIO $ streamFile file_path
      err_or_res <- flip runReaderT atk $
                      oapiUploadMedia media_type file_body file_path Nothing
      case err_or_res of
        Left err -> do
          $logError $ "oapiUploadMedia failed: " <> utshow err
        Right (UploadMediaResp media_id media_type _created_time) -> do
          putStrLn $ "MediaId is: " <> unMediaId media_id
          putStrLn $ "Types is: " <> toParamValue media_type

    DownloadMedia media_id -> do
      err_or_res <- flip runReaderT atk $ oapiDownloadMedia media_id
      case err_or_res of
        Left err -> do
          $logError $ "oapiDownloadMedia failed: " <> utshow err

        Right resp -> do
          liftIO $ hPutStrLn stderr $ "Content-Type: " <> unpack (decodeUtf8 (resp ^. responseHeader "Content-Type"))
          liftIO $ LB.putStr $ resp ^. responseBody

    SearchProcess m_txt -> do
      if optApiVersion opts == 0
         then do
              let filter_conduit = case m_txt of
                                     Nothing -> awaitForever yield
                                     Just txt -> CL.filter (isInfixOf txt . processInfoName)
              let run_call f = flip runReaderT atk $ runExceptT $ runConduit $ f .| filter_conduit .| CL.consume
              proc_infos <-
               ( flip runReaderT atk $ runExceptT $ runConduit $
                    oapiSourceProcessListByUser 0.5 Nothing .| filter_conduit .| CL.consume
                 ) >>= api_from_right "oapiSourceProcessListByUser"

              forM_ proc_infos $ \ proc_info -> do
                putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty proc_info
         else do
              let filter_conduit = case m_txt of
                                     Nothing -> awaitForever yield
                                     Just txt -> CL.filter (isInfixOf txt . vxProcessInfoName)
              proc_infos <-
                 ( flip runReaderT atk $ runExceptT $ runConduit $
                      apiVxSourceProcessListByUser 0.5 Nothing .| filter_conduit .| CL.consume
                   ) >>= api_from_right "apiVxSourceProcessListByUser"

              forM_ proc_infos $ \ proc_info -> do
                putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty proc_info

    ListProcessInstId proc_code seconds m_user_id -> do
      now <- liftIO getPOSIXTime
      let start_time = timestampFromPOSIXTime $ now - fromIntegral seconds

      proc_ids <-
        if optApiVersion opts == 0
           then do
                (flip runReaderT atk $ runExceptT $ runConduit $ do
                        oapiSourceProcessInstId proc_code start_time Nothing (fmap pure m_user_id)
                                  .| CL.consume
                  ) >>= api_from_right "oapiSourceProcessInstId"
           else do
                (flip runReaderT atk $ runExceptT $ runConduit $ do
                        apiVxSourceProcessInstId maxApiVxGetProcessInstIdListTimeSpanSeconds proc_code start_time Nothing (fmap pure m_user_id) Nothing
                                  .| CL.consume
                  ) >>= api_from_right "apiVxSourceProcessInstId"

      forM_ proc_ids $ \ proc_id -> do
        putStrLn $ unProcessInstanceId proc_id


    ShowProcessInst proc_inst_id -> do
      if optApiVersion opts == 0
         then do
              proc_info <- api_call_or_abort "oapiGetProcessInstanceInfo" (flip runReaderT atk $ oapiGetProcessInstanceInfo proc_inst_id)
              putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty proc_info
              putStrLn $ "processInstInfoId=" <> toParamValue (processInstInfoId proc_info)
         else do
              proc_info <- api_call_or_abort "apiVxGetProcessInstanceInfo" (flip runReaderT atk $ apiVxGetProcessInstanceInfo proc_inst_id)
              putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty proc_info
              putStrLn $ "vxProcessInstInfoId=" <> toParamValue (vxProcessInstInfoId proc_info)


    ProcessInstAddComment proc_inst_id user_id text photo_urls -> do
      err_or_res <- flip runReaderT atk $ oapiProcessInstanceAddComment proc_inst_id user_id text photo_urls

      case err_or_res of
        Left err -> do
          $logError $ "oapiProcessInstanceAddComment failed: " <> utshow err

        Right res -> do
          putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty res

    ShowUserProcessToDo user_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetUserProcessInstanceToDo user_id

      case err_or_res of
        Left err -> do
          $logError $ "oapiGetUserProcessInstanceToDo failed: " <> utshow err

        Right cnt -> putStrLn $ utshow cnt


    StartProcess proc_code user_id m_dept_id approvers nv_list -> do
      let inputs_map = mapFromList $ map (uncurry (@=)) nv_list
      err_or_res <- flip runReaderT atk $ runExceptT $ do
                      dept_id <- case m_dept_id of
                                   Just x -> return x
                                   Nothing -> do
                                     -- 随便选一个部门
                                     fmap (fmap userDetailsDepartments) (ExceptT $ oapiGetUserDetails user_id)
                                        >>= maybe (throwIO $ userError "user not found") return
                                        >>= maybe (throwIO $ userError "user does not belong to any dept") return . listToMaybe

                      ExceptT $ oapiCreateProcessInstance Nothing proc_code user_id dept_id (Just $ pure $ toApprovers ConcensusAnd (NES.fromList approvers) ) Nothing inputs_map

      case err_or_res of
        Left err -> do
          $logError $ "Some API failed: " <> utshow err

        Right proc_inst_id -> putStrLn $ "Process Instance Id: " <> toParamValue proc_inst_id


    DeleteCallback -> do
      err_or_res <- flip runReaderT atk $ oapiDeleteCallback

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right () -> putStrLn "OK"


    PunchResult begin_day end_day user_ids -> do
      err_or_res <- flip runReaderT atk $ runExceptT $ runConduit $ do
        oapiSourceAttendPunchResults Nothing user_ids (begin_day, end_day) .| CL.consume

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right results -> do
          mapM_ ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) results


    PunchDetails begin_day end_day user_ids -> do
      err_or_res <- flip runReaderT atk $ do
        oapiGetAttendPunchDetails Nothing user_ids (begin_day, end_day)

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right results -> do
          mapM_ ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) results


    VisibleReportTemplates m_user_id -> do
      err_or_res <- flip runReaderT atk $ do
        runExceptT $ runConduit $ oapiSourceVisibleReportTemplates m_user_id .| CL.consume

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right results -> do
          mapM_ ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) results

    QueryReports start_time end_time m_user_id m_template_name -> do
      start_time' <- timestampFromSpec start_time
      end_time' <- timestampFromSpec end_time
      err_or_res <- flip runReaderT atk $ do
        runExceptT $ runConduit $ oapiSourceReports (start_time', end_time') m_user_id m_template_name .| CL.consume

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right results -> do
          mapM_ ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) results

    ListDriveSpaces union_id space_type -> do
      err_or_res <- flip runReaderT atk $ do
        runExceptT $ runConduit $ apiVxSourceDriveSpaces 0.1 union_id space_type .| CL.consume

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right results -> do
          mapM_ ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) results

    ListDriveEntries union_id space_id parent_id -> do
      err_or_res <- flip runReaderT atk $ do
        runExceptT $ runConduit $ apiVxSourceDriveEntriesUnder 0.1 union_id space_id parent_id def .| CL.consume

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right results -> do
          mapM_ ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) results

    ListDriveEntriesByPath union_id space_id path -> do
      err_or_res <- flip runReaderT atk $ do
        runExceptT $ runExceptT $ runConduit $ apiVxSourceDriveEntriesUnderPath 0.1 union_id space_id path def .| CL.consume

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right (Left (stop_entry, stop_path) ) -> $logError $ "Not found: " <> fromString stop_path
        Right (Right results) -> do
          mapM_ ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) results

    GetDriveEntry union_id space_id entry_id -> do
      err_or_res <- flip runReaderT atk $ do
        runExceptT $ apiVxGetDriveEntryMaybe union_id space_id entry_id

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right result -> do
          ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) result

    GetDriveEntryByPath union_id space_id path -> do
      err_or_res <- flip runReaderT atk $ do
        runExceptT $ apiVxGetDriveEntryByPath union_id space_id path

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err
        Right result -> do
          ( putStrLn . toStrict . decodeUtf8 . AP.encodePretty ) result

    DownloadDriveEntryDownload union_id space_id entry_id -> do
      err_or_res <- flip runReaderT atk $ runExceptT $ do
        apiVxFetchDriveEntry union_id space_id entry_id

      case err_or_res of
        Left err -> $logError $ "Some API failed: " <> utshow err

        Right (Left err) -> do
          $logError $ "apiVxFetchDriveEntry failed: " <> err

        Right (Right resp) -> do
          case resp ^. responseStatus . statusCode of
            200 -> do
              let lbs = resp ^. responseBody
              $logInfo $ "Response body length: " <> utshow (length lbs)
            code -> do
              $logError $ "Response status code: " <> utshow code

  where
    corp_id = optAppKey opts
    corp_secret = optAppSecret opts

    api_call_or_abort2 :: (MonadIO n, MonadLogger n, Show e1, Show e2)
                       => Text
                       -> n (Either e1 r)
                       -> n (Either e2 r)
                       -> n r
    api_call_or_abort2 api_name callf0 callf1 = do
      if optApiVersion opts == 0
         then callf0 >>= api_from_right api_name
         else callf1 >>= api_from_right api_name


    api_call_or_abort :: (MonadIO n, MonadLogger n, Show e)
                      => Text
                      -> n (Either e r)
                      -> n r
    api_call_or_abort api_name callf = callf >>= api_from_right api_name


    api_call_or_abort_then_print_json :: (MonadIO n, MonadLogger n, Show e, ToJSON r)
                                      => Text
                                      -> n (Either e r)
                                      -> n ()
    api_call_or_abort_then_print_json api_name callf = do
      res <- api_call_or_abort api_name callf
      putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty res


    api_from_right :: (MonadIO n, MonadLogger n, Show e) => Text -> Either e a -> n a
    api_from_right api_name err_or_res =
      case err_or_res of
        Right r -> return r
        Left err -> do
          $logError $ api_name <> " failed: " <> utshow err
          liftIO exitFailure
-- }}}1


start' :: Options -> IO ()
-- {{{1
start' opts = do
  sess <- WS.newAPISession
  logger_set <- newStderrLoggerSet 0
  throttle <- overrideMinimalIntervalThrottle 0.01 <$> newMinimalIntervalThrottle
  let api_env = HttpApiRunEnv throttle sess
  runLoggingT
      (start opts api_env)
      (appLogger logger_set (optVerbose opts))
-- }}}1


appLogger :: LoggerSet -> Int -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
-- {{{1
appLogger logger_set verbose loc src level ls = do
    let should_log = case level of
                        LevelOther {}   -> True
                        _               -> level `elem` lv_by_v verbose

    if should_log
        then pushLogStr logger_set $ defaultLogStr loc src level ls
        else return ()
    where
        lv_by_v lv
            | lv <= 0   = [ LevelError]
            | lv == 1   = [ LevelError, LevelWarn ]
            | lv == 2   = [ LevelError, LevelWarn, LevelInfo ]
            | otherwise = [ LevelError, LevelWarn, LevelInfo, LevelDebug ]
-- }}}1


main :: IO ()
-- {{{1
main = execParser opts >>= start'
  where
    opts = info (helper <*> optionsParse)
            ( fullDesc
                <> progDesc (unlines
                    [ "执行一些钉钉开发平台管理查询操作"
                    ])
                <> header "dingtalk-manage - 钉钉开发平台管理查询小工具"
                )
-- }}}1


utshow :: Show a => a -> Text
utshow = fromString . ushow


-- vim: set foldmethod=marker:
