{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleTexService.Run where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Digest.Pure.MD5 as MD5
import           Data.String
import qualified Data.UUID            as UUID
import qualified Network.AWS          as AWS
import qualified Network.AWS.S3       as AWS
import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process       (CreateProcess (..), StdStream (..),
                                       createProcess, proc, waitForProcess)
import           System.Random        (randomIO)

tex :: [String] -> CreateProcess
tex args = (proc "context" args) { std_out = NoStream
                                 , std_err = NoStream
                                 , std_in = NoStream
                                 }

runTex :: FilePath -> IO (FilePath, FilePath)
runTex target = do
  tmp <- getTemporaryDirectory
  dir <- createTempDirectory tmp "simple-tex-service"
  let tmpTarget = dir ++ "/target.tex"
      logPath = dir ++ "/target.log"
      pdfPath = dir ++ "/target.pdf"
  copyFileWithMetadata target tmpTarget
  (Nothing, Nothing, Nothing, ph) <-
    createProcess ((tex ["--noconsole", tmpTarget]) {cwd = Just dir})
  ec <- waitForProcess ph
  case ec of
    ExitSuccess   -> return ()
    ExitFailure e -> error ("Context failed with: " ++ show e)
  return (pdfPath, logPath)

run :: AWS.BucketName -> FilePath -> IO (String, String, String)
run bucketName targetFile = do
  lgr <- AWS.newLogger AWS.Debug stdout
  awsEnv <- AWS.newEnv AWS.Discover
  uuid <- UUID.toString <$> randomIO
  putStrLn $ "Got request " ++ uuid
  f <- show . MD5.md5 <$> ByteStringL.readFile targetFile
  putStrLn $ "Hash " ++ show f
  let pdfS3Key = f ++ ".pdf"
      logS3Key = f ++ ".log"
      texS3Key = f ++ ".tex"
  exists <-
    AWS.runResourceT $
    AWS.runAWS (awsEnv & AWS.envLogger .~ lgr) $
    (do res <- AWS.send (AWS.headObject bucketName (fromString pdfS3Key))
        return (res ^. AWS.horsResponseStatus == 200)) `catch`
    (\(AWS.ServiceError _) -> return False)
  if exists
    then return (pdfS3Key, logS3Key, texS3Key)
    else do
      (pdfFile, logFile) <- runTex targetFile
      putStrLn $ "Generated files for " ++ uuid
      putStrLn $ "Terminating response for " ++ uuid
      pdfBody <- ByteStringL.readFile pdfFile
      texBody <- ByteStringL.readFile targetFile
      logBody <- ByteStringL.readFile logFile
      uploadAllToS3
        awsEnv
        lgr
        [ (pdfS3Key, pdfBody, "application/pdf")
        , (texS3Key, texBody, "text/plain")
        , (logS3Key, logBody, "text/plain")
        ]
      return (pdfS3Key, logS3Key, texS3Key)
  where
    uploadAllToS3 awsEnv lgr pairs =
      AWS.runResourceT $
      AWS.runAWS (awsEnv & AWS.envLogger .~ lgr) $
      forM_ pairs $ \(k, b, ct) -> uploadToS3 k b ct
    uploadToS3 key content contentType = do
      let req = AWS.putObject bucketName (fromString key) (AWS.toBody content)
          req' = req & AWS.poContentType .~ Just contentType
      AWS.send req'
