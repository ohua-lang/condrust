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
import Ohua.Compile
import Ohua.Compile.Config
import Ohua.Integration.Langs
import Ohua.Stage (knownStages)
import Ohua.Unit

data Command
    = Build CommonCmdOpts
    | ShowVersion

data CommonCmdOpts = CommonCmdOpts
    { inputModuleFile :: FileRef
    , outputPath :: FileRef
    , config :: Maybe FileRef
    }

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
        Build common@CommonCmdOpts {..} ->
            CompilerOptions {..} <- loadConfig config
            runCompM (logLevel debug)
                $ runReaderT 
                    -- TODO just pass in the configuration
                    ( (stageHandlingOpt debug)
                    , ("tail-recursion" `elem` extraFeatures)
                    , outputFormat
                    ) 
                    $ compile inputModuleFile compilationScope outputPath
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
    softStr = fillSep . map text . Str.words
    optsParser =
        hsubparser
            (command
                 "build"
                 (info
                      (Build <$> commonOptsParser)
                      (progDescDoc $
                       Just $
                       "Build the ohua graph. " <$$> ""))) <>
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
            (strOption $
             long "output" <> metavar "PATH" <> short 'o' <>
             help "Path to write the output to.") <*>
        optional
            (strOption $
             long "config" <> metavar "PATH" <> short 'c' <>
             help "Path to the configuration file for the compilation.")
