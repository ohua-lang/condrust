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
extra-features:
- tail-rec
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
import Ohua.Frontend.NS (Feature)
import qualified Ohua.Compile.CodeGen.JSONObject as JSONGen
import Ohua.Compile.CodeGen.Iface (CodeGen)
import Ohua.Compile.Util (toFilePath)

import qualified Data.Text as T (Text, unpack, pack, intercalate)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=), decodeFileThrow)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import System.FilePath.Posix (splitDirectories, splitExtension)
import System.Directory (doesFileExist)

data CodeGenSelection
    = JsonGraph
    deriving (Read, Show, Bounded, Enum, Eq)

selectionToGen :: CodeGenSelection -> CodeGen
selectionToGen JsonGraph = JSONGen.generate

intoCodeGenSelection :: Text -> CodeGenSelection
intoCodeGenSelection "json-graph" = JsonGraph
intoCodeGenSelection t            = error $ "Unknown code gen: " <> t

data DebugOptions = DebugOptions 
    { logLevel :: LogLevel
    , stageHandlingOpt :: StageHandling
    }

passStage = (Don'tDump, False)
defaultStageHandling = \_ -> passStage

intoLogLevel :: Text -> LogLevel
intoLogLevel "debug" = LevelDebug
intoLogLevel "info"  = LevelInfo
intoLogLevel "warn"  = LevelWarn
intoLogLevel "error" = LevelError
intoLogLevel  t      = LevelOther t

defaultDebug :: DebugOptions
defaultDebug = DebugOptions 
    { logLevel = LevelWarn
    , stageHandlingOpt = defaultStageHandling
    }

data CompilerOptions = CompilerOptions 
    { outputFormat :: CodeGenSelection
    , compilationScope :: CompilationScope
    , extraFeatures :: [Feature]
    , debug :: DebugOptions
    }

data Files = Files [Text]

data Stage = Stage
    { stage :: Text
    , dump :: Bool
    , abortAfter :: Bool
    } deriving (Eq, Show)

instance FromJSON Stage where
    parseJSON (Y.Object v) = 
        Stage <$>
        v .:  "stage" <*>
        (v .:? "dump" .!= True) <*>
        (v .:? "abort-after" .!= False)
    parseJSON _ = fail "Expected Object for stage description."

intoStageHandling :: Maybe [Stage] -> StageHandling
intoStageHandling Nothing   = defaultStageHandling
intoStageHandling (Just []) = defaultStageHandling
intoStageHandling (Just stages)  = 
    let registry = 
            HM.fromList 
                $ map
                (\s -> ( stage s
                      , ( if dump s then DumpPretty else Don'tDump, abortAfter s)
                      ))
                $ stages
    in \stage -> HM.lookupDefault passStage stage registry

instance FromJSON DebugOptions where
    parseJSON (Y.Object v) = 
        DebugOptions <$>
        (intoLogLevel <$> v .:? "log-level" .!= "warn") <*>
        (intoStageHandling <$> v .:? "core-stages")
    parseJSON _ = fail "Expected Object for DebugOptions description"

intoCompilationScope :: [Text] -> CompilationScope
intoCompilationScope filePaths = 
    HM.fromList 
        $ map 
            (\t -> let (path, suffix) = splitExtension $ T.unpack t
                    in (convert path, T.pack suffix))
            filePaths
            
    where
        convert :: FilePath -> NSRef
        convert = toNSRef . toBnds . splitDirectories
        toNSRef :: [Binding] -> NSRef
        toNSRef = makeThrow
        toBnds :: [FilePath] -> [Binding]
        toBnds = map $ makeThrow . T.pack

instance FromJSON CompilerOptions where
    parseJSON (Y.Object v) =
        CompilerOptions <$>
        (intoCodeGenSelection <$> v .:  "output-format") <*>
        (intoCompilationScope <$> v .:  "compilation-scope") <*>
        v .:? "extra-features" .!= [] <*>
        v .:? "debug" .!= defaultDebug
    parseJSON _ = fail "Expected Object for Config value"

defaultCompilerOptions = CompilerOptions JsonGraph HM.empty [] defaultDebug

loadConfig :: (MonadIO m, MonadError Error m) => Maybe String -> m CompilerOptions
loadConfig Nothing    = return defaultCompilerOptions
loadConfig (Just ref) = decodeFileThrow ref

validateConfig :: (MonadIO m, MonadError Error m) => CompilerOptions -> m ()
validateConfig conf = do
    mapM
        (\pathAndSuffix -> do
            let file = toFilePath pathAndSuffix
            fileExists <- liftIO $ doesFileExist file
            if fileExists
            then return ()
            else throwError $ "Configuration error: Module '" <> show file <> "' does not exist.")
        $ HM.toList $ compilationScope conf
    return ()
