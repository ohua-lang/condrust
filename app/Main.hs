{-# LANGUAGE TemplateHaskell, CPP #-}
module Main where

import Ohua.Prelude

import Control.Lens (Index, IxValue, Ixed, (^?), ix, view)
import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import qualified Data.Char as C (toLower)
import qualified Data.HashSet as HS (HashSet, fromList, member)
import Data.List (intercalate, lookup)
import qualified Data.String as Str
import Data.Time (getCurrentTime)
import Language.Haskell.TH
import Options.Applicative as O
import Options.Applicative.Help.Pretty as O
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath as FP ((-<.>), takeDirectory)

import Ohua.ALang.Lang
import Ohua.Frontend.NS
import Ohua.CodeGen.Iface
import qualified Ohua.CodeGen.JSONObject as JSONGen
import Ohua.Compile
import Ohua.Compile.Configuration
import Ohua.Standalone
import Ohua.Stage (knownStages)
import Ohua.Unit

data BuildOpts = BuildOpts
    { outputFormat :: CodeGenSelection
    , stageHandlingOpt :: StageHandling
    , extraFeatures :: HS.HashSet Feature
    }

data Command
    = Build CommonCmdOpts BuildOpts
    | ShowVersion

data CommonCmdOpts = CommonCmdOpts
    { inputModuleFile :: Text
    , outputPath :: Maybe Text
    , logLevel :: LogLevel
    }


data CodeGenSelection
    = JsonGraph
    deriving (Read, Show, Bounded, Enum)

selectionToGen :: CodeGenSelection -> CodeGen
selectionToGen JsonGraph = JSONGen.generate

-- (-<.>) :: Text -> Text -> Text
-- p1 -<.> p2 = toText $ toString p1 FP.-<.> toString p2

runCompM :: LogLevel -> ExceptT Text (LoggingT IO) a -> IO a
runCompM targetLevel c =
    runStderrLoggingT $
    filterLogger (\_ level -> level >= targetLevel) $
    runExceptT c >>= either exitError pure
  where
    exitError message = do
        logErrorN message
        exitFailure

-- TODO the main arguments of the compiler are:
-- - the file to be compiled
-- -I the set of "include paths". in our case: these are file references to "Ohua" source files.

main :: IO ()
main = do
    opts <- execParser odef
    case opts of
        ShowVersion -> do
            putStrLn ("ohuac v" <> CURRENT_PACKAGE_VERSION :: Text)
            putStrLn
                ("Compiled at " <>
                 $(LitE . StringL . show <$> liftIO getCurrentTime) :: Text)
        Build common@CommonCmdOpts {..} BuildOpts { outputFormat
                                                  , stageHandlingOpt
                                                  , extraFeatures
                                                  } ->
            runCompM loglevel 
                $ runReaderT 
                    ( stageHandlingOpt
                    , ("tail-recursion" `elem` extraFeatures)
                    , outputFormat
                    ) 
                    $ compile 
  where
    odef =
        info
            (helper <*> optsParser)
            (fullDesc <> header "ohuac ~ the ohua standalone compiler" <>
             progDescDoc
                 (Just $
                  softStr
                      "Compiles algorithm source files into a dataflow graph, which can be read and executed by a runtime." <$$>
                  "Supported module file extensions are:" </>
                  fillSep
                      (punctuate comma $
                       map (squotes . text . toString . view _1) definedLangs)))
    buildOpts =
        BuildOpts <$>
        O.option
            (eitherReader readCodeGen)
            (O.value JsonGraph <>
             helpDoc
                 (Just $
                  softStr "Format to emit the generated code in." <//>
                  "Accepted choices:" <+>
                  fillSep
                      (punctuate
                           comma
                           (map (text . showCodeGen)
                                [(minBound :: CodeGenSelection) ..])) </>
                  "(default: json-graph)") <>
             long "code-gen" <>
             short 'g') <*>
        ((\stopOn dumpStages sname ->
              ( if sname `HS.member` HS.fromList dumpStages
                    then DumpPretty
                    else Don'tDump
              , stopOn == Just sname)) <$>
         optional
             (O.strOption $
              long "stop-on" <> help "Stop execution after this stage." <>
              metavar "STAGE") <*>
         many
             (O.strOption
                  (long "dump-stage" <>
                   help
                       "Dump the code at this stage. (can be supplied multiple times) The code is dumped to a file called <stage-name>.dump" <>
                   metavar "STAGE"))) <*>
        (HS.fromList <$>
         many
             (strOption
                  (short 'f' <> long "feature" <>
                   help "Enable extra (experimental) features")))
    softStr = fillSep . map text . Str.words
    optsParser =
        hsubparser
            (command
                 "build"
                 (info
                      (Build <$> commonOptsParser <*> buildOpts)
                      (progDescDoc $
                       Just $
                       "Build the ohua graph. " <$$> "" <$$>
                       softStr
                           "The options --dump-stage and --stop-on pertain to stages. See https://ohua.rtfd.io/en/latest/debugging.html#stages." <$$>
                       fillSep
                           ("I know about the following stages:" :
                            punctuate comma (map (text . toString) knownStages)))) <>
            --  command
            --      "dump-main-type"
            --      (info
            --           (flip DumpType <$> dumpOpts <*> commonOptsParser)
            --           (progDesc "Dump the type of the main function")) <>
             command
                 "version"
                 (info
                      (pure ShowVersion)
                      (progDesc
                           "Show information, such as the version, about this binary.")))
    commonOptsParser =
        CommonCmdOpts <$>
        argument str (metavar "SOURCE" <> help "Source file to compile") <*>
        argument
            (eitherReader $ mapLeft toString . make . toText)
            (metavar "MAIN" <> help "Algorithm that serves as entry point" <>
             O.value "main") <*>
        optional
            (strOption $
             long "output" <> metavar "PATH" <> short 'o' <>
             help
             -- FIXME fix the description accordingly
                 "Path to write the output to (default: input filename with '.ohuao' extension for 'build' with the JSON format and '.java' with the java format and '.type-dump' for 'dump-main-type')") <*>
        ((\verbose debug ->
              if debug
                  then LevelDebug
                  else if verbose
                           then LevelInfo
                           else LevelWarn) <$>
         switch
             (short 'v' <> long "verbose" <>
              help "Print more detailed logging messages") <*>
         switch
             (long "debug" <>
              help "Activate all logging messages for debugging purposes."))
