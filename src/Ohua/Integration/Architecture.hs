module Ohua.Integration.Architecture where

import Data.Yaml (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml as Y
import Ohua.Prelude

data Arch = SharedMemory | M3 | MultiProcessing
  deriving (Show, Eq)

instance FromJSON Arch where
  parseJSON = Y.withText "Arch" $ \a ->
    case a of
      "SharedMemory" -> return SharedMemory
      "M3" -> return M3
      "MultiProcessing" -> return MultiProcessing
      _ -> fail $ "Unsupported Architecture type " <> show a

data Architectures :: Arch -> * where
  SSharedMemory :: Architectures 'SharedMemory
  SM3 :: Architectures 'M3
  SMultiProc :: Architectures 'MultiProcessing
