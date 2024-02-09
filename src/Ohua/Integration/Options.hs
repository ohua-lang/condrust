module Ohua.Integration.Options where

import Data.Yaml (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml as Y
import Ohua.Prelude

data Options = Options
  { dataPar :: Maybe Integer,
    amorphous :: Maybe Integer
  }
  deriving (Show, Eq)

instance FromJSON Options where
  parseJSON = Y.withObject "Options" $ \v ->
    Options
      <$> v .:? "data-parallelism"
      <*> v .:? "amorphous"

