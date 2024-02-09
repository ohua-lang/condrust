{-# LANGUAGE DeriveAnyClass #-}
module Ohua.Backend.Config where

import Data.Yaml (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml as Y
import Ohua.Commons.Prelude


data Options = Options
  { stateInitFusion :: Bool }
  deriving (Show, Eq)

instance FromJSON Options where
  parseJSON = Y.withObject "Options" $ \v ->
    Options
      <$> v .:? "state-init-fusion" .!= False

defaultOptions :: Options
defaultOptions = Options False

