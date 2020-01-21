{-|

Module      : $Header$
Description : Configuration for the ohuac compiler.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

A configuration has the following YAML form:

output-format: JsonGraph
compilation-scope:
- some/ns/module.go
- some/other/ns/module.go
debug:
    - log-level: verbose
    - core-stages: 
        - stage: "before-normalization"
          dump: yes
          abort-after: no
        - stage: "after-normalization"
          dump: yes
          abort-after: yes
-}

module Ohua.Compile.Config where

import Ohua.Prelude

import Ohua.Compile.Types

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Text.RawString.QQ
import Data.ByteString (ByteString)
import Control.Applicative

data LogLevel = None | Verbose | Debug
type Feature = Text

data CodeGenSelection
    = JsonGraph
    deriving (Read, Show, Bounded, Enum)

selectionToGen :: CodeGenSelection -> CodeGen
selectionToGen JsonGraph = JSONGen.generate

data DebugOptions = DebugOptions 
    { logLevel :: LogLevel
    , stageHandlingOpt :: StageHandling
    }

passStage = (Don'tDump, False)
defaultStageHandling = \_ -> passStage

defaultDebug :: DebugOptions
defaultDebug = DebugOptions 
    { loglevel = None
    , stageHandlingOpt = defaultStageHandling
    }

type Feature = Text

data CompilerOptions = CompilerOptions 
    { outputFormat :: CodeGenSelection
    , compilationScope :: CompilationScope
    , extraFeatures :: [Feature]
    , debug :: DebugOptions
    } deriving (Eq, Show)

data Files = Files [Text]

data Stage = Stage
    { stage :: Text
    , dump :: Bool
    , abortAfter :: Bool
    }

data Stages = Stages 
    { stages :: [Stage] }

instance FromJSON Stages where
    parseJSON (Y.Object v) = 
        Stages <$>
        v .:  "stage" <*>
        v .:? "dump" .!= True <*>
        v .:? "abort-after" .!= False
    parseJSON _ = fail "Expected Object for stage description."

intoStageHandling :: Stages -> StageHandling
intoStageHandling stages = 
    let registry = HM.fromList 
        $ flip map stages 
        $ \s -> ( stage s
                , ( if dump s then DumpPretty else Don'tDump
                  , abortAfter s
                  )
                )
    in $ \stage -> HM.lookupDefault passStage stage registry

instance FromJSON DebugOptions where
    parseJSON (Y.Object v) = 
        DebugOptions <$>
        v .:? "log-level" .!= None <*>
        intoStageHandling . (v .:? "core-stages" .! defaultStageHandling)
    parseJSON _ = fail "Expected Object for DebugOptions description"

intoCompilationScope :: [Text] -> CompilationScope
intoCompilationScope = 
    HM.fromList
    $ flip map 
    $ \t -> 
        let (path, suffix) = splitOn "." t
        in (fromList $ splitOn "/" path, suffix)

instance FromJSON CompilerOptions where
  parseJSON (Y.Object v) =
    CompilerOptions <$>
    v .:  "output-format" <*>
    intoCompilationScope . (v .:  "compilation-scope") <*>
    v .:? "debug" .!= defaultDebug
  parseJSON _ = fail "Expected Object for Config value"