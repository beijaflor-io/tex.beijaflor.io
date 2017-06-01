{-# LANGUAGE TemplateHaskell #-}
module SimpleTexService.Types
  where

import           Control.Lens.TH

-- Types
import           Data.Text          (Text)
import           Network.AWS.S3     (BucketName (..))

-- Options
import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as Text
import           System.Environment

-- Web Service
import           Web.Spock
import           Web.Spock.Config

data Options = Options { _optionsPort       :: Int
                       , _optionsBucketName :: BucketName
                       , _optionsHost       :: Text
                       }

data ApplicationState = ApplicationState { _appStateOptions :: Options
                                         }

$(makeLenses ''ApplicationState)

type Context = ()
type DatabaseConnection = ()
type Session = ()
type Action = SpockActionCtx Context DatabaseConnection Session ApplicationState

initialize :: IO ApplicationState
initialize = ApplicationState <$> parseOptions

parseOptions :: IO Options
parseOptions = do
  host <- Text.pack <$> getEnv "STS_HOST"
  bucketName <- BucketName . Text.pack <$> getEnv "STS_BUCKET_NAME"
  port <- read . fromMaybe "3003" <$> lookupEnv "PORT"
  return Options { _optionsPort = port
                 , _optionsBucketName = bucketName
                 , _optionsHost = host
                 }

spockConfiguration :: st -> IO (SpockCfg () () st)
spockConfiguration = defaultSpockCfg () PCNoDatabase
