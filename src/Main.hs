{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.UUID            as UUID
import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process
import           System.Random
import           Text.Hamlet
import           Web.Spock

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
          <form action="/form" method="POST">
            <.form-group>
              <textarea .form-control name="cabalfile" id="cabalfile" rows="30" style="resize: none; min-height: 50vh;" />
            <.form-group>
              <button class="btn btn-primary" type="submit">
                Convert
        <.col-md-6>
          <pre><code class="result"># Result</code></pre>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.1.0/jquery.min.js" />
    <script>
      \$('form').submit(function(e) {
        e.preventDefault();
        \$.post('/form', {cabalfile: $('textarea').val()}, function(res) {
          \$('.result').text(res);
        }).fail(function(err) {
          \$('.result').text(JSON.stringify(err, null, 2));
        });
      });
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
    uuid <- UUID.toString <$> randomIO
    putStrLn $ "Got request " ++ uuid
    f <- MD5.md5 <$> ByteStringL.readFile targetFile
    putStrLn $ "Hash " ++ (show f)
    (pdfFile, logFile) <- runTex targetFile
    putStrLn $ "Generated files for " ++ uuid
    putStrLn $ "Terminating response for " ++ uuid
    return ( "PDF: " ++ pdfFile
           , "LOG: " ++ logFile
           )

main :: IO ()
main = do
    runSpock 3003 $ spockT id $ do
        post "/" $ do
            targetFile <- param "file"
            print targetFile
            run targetFile
            text "ok"

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
