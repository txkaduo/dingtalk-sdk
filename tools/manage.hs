module Main where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=), argument)
import           Control.Monad.Trans.Except
import           Control.Monad.Logger
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit (($$), (=$=))
import qualified Data.Conduit.List as CL
import           Network.HTTP.Client   (streamFile)
import           Network.Wreq         (responseBody, responseHeader)
import qualified Network.Wreq.Session  as WS
import           Options.Applicative
import           System.IO             (hPutStrLn)
import           System.Log.FastLogger (LoggerSet, newStderrLoggerSet,
                                        pushLogStr)
import           System.Exit

import           Text.Parsec.TX.Utils  (SimpleStringRep,
                                        parseMaybeSimpleEncoded, simpleEncode)

import DingTalk
-- }}}1

data ManageCmd = Scopes
               | SearchUser Text
               | DeptSubForest (Maybe DeptId)
               | ShowDeptDetails DeptId
               | UploadMedia MediaType FilePath
               | DownloadMedia MediaId
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
                          <$> (argument (simpleEncodedStringReader "media type") (metavar "MEDIA_TYPE"))
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
-- }}}1


simpleEncodedStringReader :: SimpleStringRep a
                          => String
                          -> ReadM a
-- {{{1
simpleEncodedStringReader type_prompt = do
  s <- str
  maybe (fail $ "cannot parse as " <> type_prompt <> ": " <> s)
        return
        (parseMaybeSimpleEncoded s)
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
      $logError $ "oapiGetAccessToken failed: " <> tshow err
      liftIO exitFailure

  case optCommand opts of
    Scopes -> do
      err_or_res <- flip runReaderT atk $ oapiGetAccessTokenScopes
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetAccessTokenScopes failed: " <> tshow err
          liftIO exitFailure

        Right resp -> do
          putStrLn $ "AccessToken is: " <> unAccessToken atk
          putStrLn $ tshow resp

    SearchUser name -> do
      err_or_res <- flip runReaderT atk $ runExceptT $ do
        oapiSourceDeptUserSimpleInfoRecursive rootDeptId
                        =$= CL.filter ((== name) . userSimpleInfoName)
                        =$= CL.mapM (ExceptT . oapiGetUserDetails . userSimpleInfoId)
                        =$= CL.catMaybes
                        $$ CL.consume

      case err_or_res of
        Left err -> do
          $logError $ "some api failed: " <> tshow err
          liftIO exitFailure

        Right user_details_list -> do
          forM_ user_details_list $ \ user_details -> do
            putStrLn $ "User Id: " <> unUserId (userDetailsUserId user_details)
            putStrLn $ "User Name: " <> userDetailsName user_details
            putStrLn $ "User Roles: " <> tshow (userDetailsRoles user_details)

    ShowDeptDetails dept_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetDeptDetails dept_id
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetDeptDetails failed: " <> tshow err
        Right details -> do
          putStrLn $ "Name: " <> deptDetailsName details
          putStrLn $ "Manager User Ids" <> tshow (deptDetailsManagerUserIds details)
          putStrLn $ "Source Identifier: " <> fromMaybe "" (deptDetailsSourceIdentifier details)

    DeptSubForest m_dept_id -> do
      err_or_res <- flip runReaderT atk $ oapiGetDeptSubForest (fromMaybe rootDeptId m_dept_id)
      case err_or_res of
        Left err -> do
          $logError $ "oapiGetDeptSubForest failed: " <> tshow err
        Right sub_forest -> do
          putStrLn $ tshow sub_forest

    UploadMedia media_type file_path -> do
      file_body <- liftIO $ streamFile file_path
      err_or_res <- flip runReaderT atk $
                      oapiUploadMedia media_type file_body (Just file_path) Nothing
      case err_or_res of
        Left err -> do
          $logError $ "oapiUploadMedia failed: " <> tshow err
        Right (UploadMediaResp media_id media_type _created_time) -> do
          putStrLn $ "MediaId is: " <> unMediaId media_id
          putStrLn $ "Types is: " <> fromString (simpleEncode media_type)

    DownloadMedia media_id -> do
      err_or_res <- flip runReaderT atk $ oapiDownloadMedia media_id
      case err_or_res of
        Left err -> do
          $logError $ "oapiDownloadMedia failed: " <> tshow err

        Right resp -> do
          liftIO $ hPutStrLn stderr $ "Content-Type: " <> unpack (decodeUtf8 (resp ^. responseHeader "Content-Type"))
          liftIO $ LB.putStr $ resp ^. responseBody

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
