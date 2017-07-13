{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module SimpleTexService.View where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.HashMap.Lazy             as HashMap
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Network.AWS.S3                as AWS
import           System.IO.Temp
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
      <h4> Convert a TeX file to PDF
      <.row>
        <.col-md-6>
          <form action="/" method="POST" enctype="multipart/form-data">
            <.form-group>
              <label for="textype">
                TeX Family
              <select value="lualatex" .form-control name="textype">
                <option value="lualatex">
                  LuaLaTeX
                <option value="latex">
                  LaTeX
                <option value="context">
                  ConTeXt
            <.form-group>
              <label for="file">
                Upload a file
              <input name="file" .form-control type="file">
            <.form-group>
              <button class="btn btn-primary" type="submit">
                Convert
          <h3> Or
          <form action="/" method="POST">
            <.form-group>
              <label for="textype">
                TeX Family
              <select value="lualatex" .form-control name="textype">
                <option value="lualatex">
                  LuaLaTeX
                <option value="latex">
                  LaTeX
                <option value="context">
                  ConTeXt
            <.form-group>
              <label for="file">
                Write some TeX
              <textarea name="text" .form-control>
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

optionsHome :: Action ()
optionsHome = do
  setHeader "access-control-allow-origin" "*"
  setHeader "access-control-allow-headers" "content-type"
  return ()

postHome :: Action ()
postHome = do
  appState <- getState
  let bucketName = _optionsBucketName . _appStateOptions $ appState
      s3PublicUrl = _optionsS3PublicUrl . _appStateOptions $ appState
  (targetFile, texType) <- header "content-type" >>= \case
      Just "application/json" -> parseJson
      _ -> parseForm
  (pdfS3Key, logS3Key, texS3Key) <- liftIO $ run texType bucketName targetFile
  let setHeader' h = setHeader h . fromString
  setHeader' "access-control-allow-origin" "*"
  setHeader' "access-control-allow-headers" "content-type"
  setHeader' "x-sts-pdf" (s3PublicUrl <> pdfS3Key)
  setHeader' "x-sts-log" (s3PublicUrl <> logS3Key)
  setHeader' "x-sts-tex" (s3PublicUrl <> texS3Key)
  redirect $ fromString (s3PublicUrl <> pdfS3Key)
  where
    parseJson = do
      Object o <- jsonBody' :: Action Value
      let Just (String t) = HashMap.lookup "text" o
          (String texType) = fromMaybe "lualatex" $ HashMap.lookup "textype" o
      fp <- liftIO $ do
          print ("Flushing input", t)
          fp <- emptySystemTempFile "input"
          Text.writeFile fp t
          return fp
      return (fp, Text.unpack texType)
    parseForm = do
      fs <- files
      let mFileUpload = HashMap.lookup "file" fs
      fp <- case uf_tempLocation <$> mFileUpload of
        Just f
          | (uf_name <$> mFileUpload) /= Just "" -> return f
        _ -> do
          t <- fromMaybe "" <$> param "text" :: Action Text
          liftIO $ do
            print ("Flushing input", t)
            fp <- emptySystemTempFile "input"
            Text.writeFile fp t
            return fp
      texType <- fromMaybe "latex" <$> param "textype" :: Action String
      return (fp, texType)
