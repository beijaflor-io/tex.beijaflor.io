{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module SimpleTexService.View where

import           Control.Monad.IO.Class
import qualified Data.HashMap.Lazy             as HashMap
import           Data.Monoid
import           Data.Monoid
import           Data.String
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Network.AWS.S3                as AWS
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Hamlet
import           Web.Spock

import           SimpleTexService.Run
import           SimpleTexService.Types

curlExample :: Text -> Text
curlExample host = "curl " <> host <> " -F \"file=@./input.tex\" > output.pdf"

getHome :: Action ()
getHome = do
    host <- _optionsHost . _appStateOptions <$> getState
    preferredFormat >>= \case
        PrefHTML -> lazyBytes $ renderHtml $ [shamlet|
<html>
  <head>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <body>
    <div class="container">
      <h1> TeX Service
      <h4> Convert with <code>curl</code>
      <pre><code>#{curlExample host}</code></pre>
      <h4> Upload a ConTeXt file
      <.row>
        <.col-md-6>
          <form action="/" method="POST" enctype="multipart/form-data">
            <.form-group>
              <input name="file" .form-control name="cabalfile" type="file">
            <.form-group>
              <button class="btn btn-primary" type="submit">
                Convert
        <.col-md-6>
           <p>
             The file will be uploaded the AWS S3 and you'll be redirected to
             the generated PDF permanent location.
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.1.0/jquery.min.js" />
        |]
        _ -> text $ Text.unlines [ "Available methods:"
                             , "POST /"
                             , "  Converts tex files sent through the `file` field"
                             , "  to a PDF"
                             , "  Example:"
                             , "  " <> curlExample host
                             ]

postHome :: Action ()
postHome = do
  appState <- getState
  let bucketName = _optionsBucketName . _appStateOptions $ appState
  fs <- files
  let Just targetFile = uf_tempLocation <$> HashMap.lookup "file" fs
  liftIO $ print targetFile
  (pdfS3Key, logS3Key, texS3Key) <- liftIO $ run bucketName targetFile
  let setHeader' h = setHeader h . fromString
  setHeader' "x-sts-pdf" (s3Url bucketName pdfS3Key)
  setHeader' "x-sts-log" (s3Url bucketName logS3Key)
  setHeader' "x-sts-tex" (s3Url bucketName texS3Key)
  redirect $ fromString (s3Url bucketName pdfS3Key)
  where
    s3Url (AWS.BucketName bucketName) u =
      "https://" <> Text.unpack bucketName <> ".s3.amazonaws.com/" <> u
