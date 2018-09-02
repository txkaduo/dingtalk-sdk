module Main where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens         hiding ((.=), argument)
import           Control.Monad.Logger
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Client   (streamFile)
import           Network.Wreq         (responseBody, responseHeader)
import qualified Network.Wreq.Session  as WS
import           Options.Applicative
import           System.IO             (hPutStrLn)
import           System.Log.FastLogger (LoggerSet, newStderrLoggerSet,
                                        pushLogStr)

import           Text.Parsec.TX.Utils  (SimpleStringRep,
                                        parseMaybeSimpleEncoded, simpleEncode)

import DingTalk
-- }}}1

data ManageCmd = UpdateAccessToken CorpId CorpSecret
               | UploadMedia AccessToken MediaType FilePath
               | DownloadMedia AccessToken MediaID
               deriving (Show)

data Options = Options
  { optVerbose :: Int
  , optCommand :: ManageCmd
  }

optionsParse :: Parser Options
-- {{{1
optionsParse = Options
                <$> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")
                <*> manageCmdParser
-- }}}1


manageCmdParser :: Parser ManageCmd
-- {{{1
manageCmdParser = subparser $
  command "get-access-token"
    (info (helper <*> ( UpdateAccessToken
                          <$> (CorpId . fromString <$> argument str (metavar "CORP_ID"))
                          <*> (CorpSecret . fromString <$> argument str (metavar "CORP_SECRET"))
                      )
          )
          (progDesc "获取 AccessToken")
    )
  <> command "upload-media"
    (info (helper <*> ( UploadMedia
                          <$> (AccessToken . fromString <$> argument str (metavar "ACCESS_TOKEN"))
                          <*> (argument (simpleEncodedStringReader "media type") (metavar "MEDIA_TYPE"))
                          <*> (argument str (metavar "FILE"))
                      )
          )
          (progDesc "上传文件，并取得 media id")
    )
  <> command "download-media"
    (info (helper <*> ( DownloadMedia
                          <$> (AccessToken . fromString <$> argument str (metavar "ACCESS_TOKEN"))
                          <*> (MediaID . fromString <$> argument str (metavar "MEDIA_ID"))
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
start opts api_env = do
  case optCommand opts of
    UpdateAccessToken corp_id corp_secret -> do
      err_or_atk <- flip runReaderT api_env (oapiGetAccessToken' corp_id corp_secret)
      case err_or_atk of
        Left err -> do
          $logError $ "oapiGetAccessToken failed: " <> tshow err
        Right atk -> do
          putStrLn $ "AccessToken is: " <> unAccessToken atk

    UploadMedia atk media_type file_path -> do
      file_body <- liftIO $ streamFile file_path
      err_or_res <- flip runReaderT api_env $ flip runReaderT atk $
                      oapiUploadMedia media_type file_body (Just file_path) Nothing
      case err_or_res of
        Left err -> do
          $logError $ "oapiUploadMedia failed: " <> tshow err
        Right (UploadMediaResp media_id media_type _created_time) -> do
          putStrLn $ "MediaID is: " <> unMediaID media_id
          putStrLn $ "Types is: " <> fromString (simpleEncode media_type)

    DownloadMedia atk media_id -> do
      err_or_res <- flip runReaderT api_env $ flip runReaderT atk $
                          oapiDownloadMedia media_id
      case err_or_res of
        Left err -> do
          $logError $ "oapiDownloadMedia failed: " <> tshow err

        Right resp -> do
          liftIO $ hPutStrLn stderr $ "Content-Type: " <> unpack (decodeUtf8 (resp ^. responseHeader "Content-Type"))
          liftIO $ LB.putStr $ resp ^. responseBody

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
