{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy          as ByteStringL
import qualified Data.Digest.Pure.MD5          as MD5
import qualified Data.HashMap.Lazy             as HashMap
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.UUID                     as UUID
import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as AWS
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process
import           System.Random
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Hamlet
import           Web.Spock
import           Web.Spock.Config

tex :: [String] -> CreateProcess
tex args = (proc "context" args) { std_out = NoStream
                                 , std_err = NoStream
                                 , std_in = NoStream
                                 }

curlExample :: Text -> Text
curlExample host = "curl " <> host <> " -F \"file=@./input.tex\" > output.pdf"

getHome host = preferredFormat >>= \case
    PrefHTML -> lazyBytes $ renderHtml $ [shamlet|
<html>
  <head>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <body>
    <div class="container">
      <h1> Hpack Convert
      <h4> Convert with <code>curl</code>
      <pre><code>#{curlExample host}</code></pre>
      <h4> Paste your cabal file bellow
      <.row>
        <.col-md-6>
          <form action="/" method="POST">
            <.form-group>
              <input name="file" .form-control name="cabalfile" type="file">
            <.form-group>
              <button class="btn btn-primary" type="submit">
                Convert
        <.col-md-6>
          <pre><code class="result"># Result</code></pre>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.1.0/jquery.min.js" />
        |]
    _ -> text $ Text.unlines [ "Available methods:"
                             , "POST /"
                             , "  Converts tex files sent through the `file` field"
                             , "  to a PDF"
                             , "  Example:"
                             , "  " <> curlExample host
                             ]

runTex :: FilePath -> IO (FilePath, FilePath)
runTex target = do
    tmp <- getTemporaryDirectory
    dir <- createTempDirectory tmp "simple-tex-service"

    let tmpTarget = dir ++ "/target.tex"
        logPath = dir ++ "/target.log"
        pdfPath = dir ++ "/target.pdf"
    copyFileWithMetadata target tmpTarget
    (Nothing, Nothing, Nothing, ph) <- createProcess ((tex ["--noconsole", tmpTarget]) { cwd = Just dir })
    ec <- waitForProcess ph
    case ec of
        ExitSuccess -> return ()
        ExitFailure e -> error ("Context failed with: " ++ show e)
    return ( pdfPath
           , logPath
           )

run targetFile = do
    lgr <- AWS.newLogger AWS.Debug stdout
    env <- AWS.newEnv AWS.Discover
    uuid <- UUID.toString <$> randomIO
    putStrLn $ "Got request " ++ uuid

    f <- show . MD5.md5 <$> ByteStringL.readFile targetFile
    putStrLn $ "Hash " ++ (show f)

    let pdfS3Key = f ++ ".pdf"
        logS3Key = f ++ ".log"
        texS3Key = f ++ ".tex"

    exists <- AWS.runResourceT $ AWS.runAWS (env & AWS.envLogger .~ lgr) $ do
        AWS.send (AWS.headObject "simple-tex-service" (fromString pdfS3Key))

    if (exists ^. AWS.horsResponseStatus == 200)
        then return (pdfS3Key, logS3Key, texS3Key)
        else do
            (pdfFile, logFile) <- runTex targetFile
            putStrLn $ "Generated files for " ++ uuid
            putStrLn $ "Terminating response for " ++ uuid

            pdfBody <- ByteStringL.readFile pdfFile
            texBody <- ByteStringL.readFile targetFile
            logBody <- ByteStringL.readFile logFile

            AWS.runResourceT $ AWS.runAWS (env & AWS.envLogger .~ lgr) $ do
                AWS.send (AWS.putObject "simple-tex-service" (fromString pdfS3Key) (AWS.toBody pdfBody))
                AWS.send (AWS.putObject "simple-tex-service" (fromString logS3Key) (AWS.toBody logBody))
                AWS.send (AWS.putObject "simple-tex-service" (fromString texS3Key) (AWS.toBody texBody))

            return (pdfS3Key, logS3Key, texS3Key)

main :: IO ()
main = do
    port <- read . fromMaybe "3003" <$> lookupEnv "PORT"
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock port $ spock spockCfg $ do
        get "/" (getHome "localhost:3003")
        post "/" $ do
            fs <- files
            let Just targetFile = uf_tempLocation <$> (HashMap.lookup "file" fs)
            liftIO $ print targetFile
            (pdfS3Key, logS3Key, texS3Key) <- liftIO $ run targetFile
            let setHeader' h = setHeader h . fromString
            setHeader' "x-simple-tex-service-pdf" (s3Url pdfS3Key)
            setHeader' "x-simple-tex-service-log" (s3Url logS3Key)
            setHeader' "x-simple-tex-service-tex" (s3Url texS3Key)
            redirect $ fromString (s3Url pdfS3Key)
  where
    s3Url u = "https://simple-tex-service.s3.amazonaws.com/" <> u

{-
(Nothing, Just inp, Just err, ph) <- createProcess (tex ["./examples/test.tex"])
let runner label outputHandle inputHandle = do
        isEOF <- hIsEOF inputHandle
        unless isEOF $ do
            ln <- hGetLine inputHandle
            hPutStrLn outputHandle ("[" ++ label ++ "] " ++ ln)
            runner label outputHandle inputHandle
            hFlush stdout
_ <- forkIO $ runner "stdout" stdout inp
_ <- forkIO $ runner "stderr" stderr err
ec <- waitForProcess ph
print ec
-}
