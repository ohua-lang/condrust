module Ohua.Integration.Config where

import Ohua.Prelude ( Maybe (Just) , Integer, ($) )

import Ohua.Integration.Architecture

data Config = Config
  { arch :: Arch
  , options :: Options
  }

data Options = Options
  { dataPar :: Maybe Integer
  , amorphous :: Maybe Integer
  }

defaultConfig :: Config
defaultConfig = Config SharedMemory $ Options (Just 420) (Just 421)
