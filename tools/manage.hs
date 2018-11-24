module Main where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=), argument)
import           Control.Monad.Trans.Except
import           Control.Monad.Logger
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.List.NonEmpty (nonEmpty)
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client   (streamFile)
import           Network.Wreq         (responseBody, responseHeader)
import qualified Network.Wreq.Session  as WS
import           Options.Applicative
import           System.IO             (hPutStrLn)
import           System.Log.FastLogger (LoggerSet, newStderrLoggerSet,
                                        pushLogStr)
import           System.Exit
import           Text.Show.Unicode (ushow)

import DingTalk
import DingTalk.Helpers
-- }}}1

data ManageCmd = Scopes
               | SearchUser Text
               | DeptSubForest (Maybe DeptId)
               | ShowDeptDetails DeptId
               | UploadMedia MediaType FilePath
               | DownloadMedia MediaId
               | SearchProcess (Maybe Text)
               | ListProcessInstId ProcessCode Int (Maybe UserId)
               | ShowProcessInst ProcessInstanceId
               | ShowUserProcessToDo UserId
               | StartProcess ProcessCode UserId (Maybe DeptId) [UserId]
               deriving (Show)

data Options = Options
  { optVerbose    :: Int
  , optCorpId     :: CorpId
  , optCorpSecret :: CorpSecret
  , optCommand    :: ManageCmd
  }

optionsParse :: Parser Options
-- {{{1
optionsParse = Options
                <$> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")
                <*> fmap (CorpId . fromString) (strOption (long "corp-id" <> short 'i' <> metavar "CORP_ID"))
                <*> fmap (CorpSecret . fromString) (strOption (long "corp-secret" <> short 's' <> metavar "CORP_SECRET"))
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
  <> command "dept-sub-forest"
    (info (helper <*> (pure DeptSubForest <*> optional (fmap DeptId (argument auto (metavar "DEPT_ID")))))
          (progDesc "显示部门树")
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
  <> command "show-process-todo"
    (info (helper <*> (pure ShowUserProcessToDo <*> fmap UserId (argument str (metavar "USER_ID"))))
          (progDesc "显示用户待审批数量")
    )
  <> command "start-process"
    (info (helper <*> (pure StartProcess <*> fmap ProcessCode (argument str (metavar "PROCESS_CODE"))
                                         <*> fmap UserId (argument str (metavar "USER_ID"))
                                         <*> optional (fmap DeptId (argument auto (metavar "DEPT_ID")))
                                         <*> some (UserId . fromString <$> strOption (long "approver" <> short 'A' <> metavar "APPROVER_USER"))
                      ))
          (progDesc "发起一个审批，仅用于测试")
    )
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

start :: (MonadLogger m, MonadCatch m, MonadIO m)
      => Options
      -> WS.Session
      -> m ()
-- {{{1
start opts api_env = flip runReaderT api_env $ do
  err_or_atk <- oapiGetAccessToken corp_id corp_secret
  atk <- case err_or_atk of
    Right atk -> return atk
    Left err -> do
      $logError $ "oapiGetAccessToken failed: " <> utshow err
      liftIO exitFailure

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
      err_or_res <- flip runReaderT atk $ runExceptT $ do
        oapiSourceDeptUserSimpleInfoRecursive rootDeptId
                        =$= CL.filter ((== name) . userSimpleInfoName)
                        =$= CL.mapM (ExceptT . oapiGetUserDetails . userSimpleInfoId)
                        =$= CL.catMaybes
                        $$ CL.consume

      case err_or_res of
        Left err -> do
          $logError $ "some api failed: " <> utshow err
          liftIO exitFailure

        Right user_details_list -> do
          forM_ user_details_list $ \ user_details -> do
            putStrLn $ "User Id: " <> unUserId (userDetailsUserId user_details)
            putStrLn $ "User Name: " <> userDetailsName user_details
            putStrLn $ "User Roles: " <> utshow (userDetailsRoles user_details)
            putStrLn $ "User Hire Date: " <> utshow (userDetailsHiredTime user_details)
            putStrLn $ "Dept Ids: " <> utshow (userDetailsDepartments user_details)

    ShowDeptDetails dept_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetDeptDetails dept_id
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetDeptDetails failed: " <> utshow err
        Right details -> do
          putStrLn $ "Name: " <> deptDetailsName details
          putStrLn $ "Manager User Ids" <> utshow (deptDetailsManagerUserIds details)
          putStrLn $ "Source Identifier: " <> fromMaybe "" (deptDetailsSourceIdentifier details)

    DeptSubForest m_dept_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetDeptSubForest (fromMaybe rootDeptId m_dept_id)
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetDeptSubForest failed: " <> utshow err
        Right sub_forest -> do
          putStrLn $ utshow sub_forest

    UploadMedia media_type file_path -> do
      file_body <- liftIO $ streamFile file_path
      err_or_res <- flip runReaderT atk $
                      oapiUploadMedia media_type file_body (Just file_path) Nothing
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
      let filter_conduit = case m_txt of
                             Nothing -> awaitForever yield
                             Just txt -> CL.filter (isInfixOf txt . processInfoName)

      err_or_res <- flip runReaderT atk $ runExceptT $ do
                      oapiSourceProcessListByUser Nothing
                        =$= filter_conduit
                        $$ CL.consume

      case err_or_res of
        Left err -> do
          $logError $ "oapiSourceProcessListByUser failed: " <> utshow err

        Right proc_infos -> do
          forM_ proc_infos $ \ proc_info -> do
            putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty proc_info

    ListProcessInstId proc_code seconds m_user_id -> do
      now <- liftIO getPOSIXTime
      let start_time = timestampFromPOSIXTime $ now - fromIntegral seconds

      err_or_res <- flip runReaderT atk $ runExceptT $ do
                      oapiSourceProcessInstId proc_code start_time Nothing (fmap pure m_user_id)
                        $$ CL.consume

      case err_or_res of
        Left err -> do
          $logError $ "oapiSourceProcessListByUser failed: " <> utshow err

        Right proc_ids -> do
          forM_ proc_ids $ \ proc_id -> do
            putStrLn $ unProcessInstanceId proc_id

    ShowProcessInst proc_inst_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetProcessInstanceInfo proc_inst_id

      case err_or_res of
        Left err -> do
          $logError $ "oapiSourceProcessListByUser failed: " <> utshow err

        Right (ProcessInstInfo {..}) -> do
          putStrLn $ "Process Instance Info"
          putStrLn $ "====================="
          putStrLn $ "Title: " <> processInstInfoTitle
          putStrLn $ "Create Time: " <> utshow processInstInfoCreateTime
          putStrLn $ "Finish Time: " <> utshow processInstInfoFinishTime

          putStr "Operation Records:"
          if null processInstInfoOpRecords
             then putStrLn " (empty)"
             else do
                  putStrLn ""
                  putStrLn "------------------"

                  forM_ processInstInfoOpRecords $ \ (ProcessOpRecord {..}) -> do
                    putStrLn $ "Time: " <> utshow processOpTime
                    putStrLn $ "User Id: " <> toParamValue processOpUserId
                    putStrLn $ "Type: " <> toParamValue processOpType
                    putStrLn $ "Result: " <> toParamValue processOpResult
                    putStrLn $ "Remarks: " <> fromMaybe "" processOpRemark
                    putStrLn ""


          putStr "Tasks:"
          if null processInstInfoTasks
             then putStrLn " (empty)"
             else do
                  putStrLn ""
                  putStrLn "------"

                  forM_ processInstInfoTasks $ \ (ProcessTaskInfo {..}) -> do
                    putStrLn $ "Id: " <> toParamValue processTaskInfoId
                    putStrLn $ "Create Time: " <> utshow processTaskInfoCreateTime
                    putStrLn $ "Finish Time: " <> utshow processTaskInfoFinishTime
                    putStrLn $ "User Id: " <> toParamValue processTaskInfoUserId
                    putStrLn $ "Status: " <> toParamValue processTaskInfoStatus
                    putStrLn $ "Result: " <> toParamValue processTaskInfoResult
                    putStrLn ""


    ShowUserProcessToDo user_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetUserProcessInstanceToDo user_id

      case err_or_res of
        Left err -> do
          $logError $ "oapiGetUserProcessInstanceToDo failed: " <> utshow err

        Right cnt -> putStrLn $ utshow cnt


    StartProcess proc_code user_id m_dept_id approvers0 -> do
      approvers <- maybe (fail "审批人不能为空") return (nonEmpty approvers0)
      let inputs = [ ("输入一", FormCompValueText "这是文字")
                   , ("多行输入", FormCompValueText "这是文字\n又多一行")
                   , ("图片", FormCompValueImages (pure (Left "https://img.alicdn.com/tfs/TB1vxZHoGmWBuNjy1XaXXXCbXXa-180-32.png")))
                   , ("明细", FormCompValueDetails
                                (pure
                                  (
                                    mapFromList [ ("明细输入一", FormCompValueText "啦啦")
                                                , ("明细输入二", FormCompValueText "呵呵")
                                                ]
                                  )
                                )
                      )
                   ]

      err_or_res <- flip runReaderT atk $ runExceptT $ do
                      dept_id <- case m_dept_id of
                                   Just x -> return x
                                   Nothing -> do
                                     -- 随便选一个部门
                                     fmap (fmap userDetailsDepartments) (ExceptT $ oapiGetUserDetails user_id)
                                        >>= maybe (fail "user not found") return
                                        >>= maybe (fail "user does not belong to any dept") return . listToMaybe

                      ExceptT $ oapiCreateProcessInstance Nothing proc_code user_id dept_id approvers Nothing (mapFromList inputs)

      case err_or_res of
        Left err -> do
          $logError $ "Some API failed: " <> utshow err

        Right proc_inst_id -> putStrLn $ "Process Instance Id: " <> toParamValue proc_inst_id

  where
    corp_id = optCorpId opts
    corp_secret = optCorpSecret opts
-- }}}1


start' :: Options -> IO ()
-- {{{1
start' opts = do
  sess <- WS.newAPISession
  logger_set <- newStderrLoggerSet 0
  let api_env = sess
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
