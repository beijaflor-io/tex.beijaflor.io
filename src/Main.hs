{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock

import           GHC.IO.Encoding        (setLocaleEncoding, utf8)
import           SimpleTexService.Types
import           SimpleTexService.View

main :: IO ()
main = do
  setLocaleEncoding utf8
  appState <- initialize
  spockCfg <- spockConfiguration appState
  let port = _optionsPort . _appStateOptions $ appState
  runSpock port $
    spock spockCfg $ do
      get "/" getHome
      post "/" postHome
      hookRoute OPTIONS "/" optionsHome
