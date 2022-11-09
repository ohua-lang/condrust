module Ohua.Integration.Config where

import Data.Yaml (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml as Y
import Ohua.Integration.Architecture
import Ohua.Integration.Options
import Ohua.Prelude

data Config = Config
  { arch :: Arch,
    options :: Options
  }
  deriving (Show, Eq)

instance FromJSON Config where
  parseJSON = Y.withObject "Config" $ \v ->
    Config
      <$> v .: "arch"
      <*> v .:? "options" .!= Options Nothing Nothing

defaultConfig :: Config
defaultConfig = Config SharedMemory $ Options Nothing Nothing
