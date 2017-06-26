{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock

import           SimpleTexService.Types
import           SimpleTexService.View

main :: IO ()
main = do
  appState <- initialize
  spockCfg <- spockConfiguration appState
  let port = _optionsPort . _appStateOptions $ appState
  runSpock port $
    spock spockCfg $ do
      get "/" getHome
      post "/" postHome
      hookRoute OPTIONS "/" optionsHome
