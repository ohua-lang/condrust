module Integrations.Rust.RustM3.RustM3Setup
  ( renderRustCode,
    showCode,
    showCodeWithDiff,
    compileCode,
    compileCodeWithDebug,
    compileCodeWithRec,
    compileCodeWithRecWithDebug,
    module Test.Hspec,
    module Language.Rust.Quote,
  )
where

import Ohua.Prelude
import Ohua.Compile.Compiler (compile)
import qualified Ohua.Integration.Architecture as Arch
import qualified Ohua.Integration.Config as IC
import Ohua.Core.Types (Options)


import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Language.Rust.Parser (Span, parse', readInputStream)
import Language.Rust.Pretty ( pretty')
import Language.Rust.Quote
import Language.Rust.Syntax

import System.Process.Extra (readProcessWithExitCode)
import System.Directory (copyFile, createDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO.Temp

import Test.Hspec
import TestOptions 

import Integrations.TestSetup (Testable(..))
import Integrations.Rust.RustM3.HelperFiles




-- ISSUE: I should two instances of testable for the rust code format. 
--        Instead I should move the Instance declaration one level up to the rust folder and
--        find a way (proably again instances) to only define the compileModule (and maybe runCompiler) here 
instance Testable (SourceFile Span) where
  -- Todo: replace by Lang' when I'm done messing with the types :-)
  type CodeFormat (SourceFile Span) = (SourceFile Span)

  compileFormat = compileModule
  renderFormat = renderRust

renderRust :: SourceFile Span -> Text
renderRust code =  renderStrict $ layoutSmart defaultLayoutOptions $ pretty' code 

renderRustCode :: SourceFile Span -> L.ByteString
renderRustCode =
  encodeUtf8
    . (<> "\n")
    . renderLazy
    . layoutSmart defaultLayoutOptions
    . pretty'


integrationOptions :: IC.Config
integrationOptions = IC.Config Arch.M3 $ IC.Options Nothing Nothing


compileModule :: SourceFile Span -> Options -> CompilationType -> ReaderT DebugOptions IO (SourceFile Span)
compileModule inCode opts cty = do
  debug <- asks printIRs
  lift $ withSystemTempDirectory
    "testDir"
    $ \testDir -> do
      setCurrentDirectory testDir
      -- if neccessary, write library files to compile directory/scope
      -- writeFile (testDir </> "funs.rs") funs
      let inFile = testDir </> "test.rs"
      L.writeFile inFile $ renderRustCode inCode
      withSystemTempDirectory "output" $
        \outDir -> do
          let compScope = HM.empty
          let options = if debug then withDebug opts else opts
          runCompM
            LevelWarn
            $ compile inFile compScope options integrationOptions outDir
          let outFile = outDir </> takeFileName inFile

          -- producedCode <- readFile outFile
          --putStr ("\n PRODUCED MODULE: \n"::String)
          -- putStr producedCode
          placeholderFile <-readFile (outDir </> "placeholderlib.rs")
          putStr placeholderFile

          -- run the target compiler (i.e., rustc) on the input
          case cty of
            OhuaOnly -> pure ()
            BuildTarget -> runTargetCompiler testDir outDir outFile
            RunTarget -> error "Error: Running target code not implemented yet"
          -- parse & return the generated output file
          outCode :: SourceFile Span <-
            parse' <$> readInputStream outFile
          return outCode


runTargetCompiler :: FilePath -> FilePath -> FilePath -> IO ()
runTargetCompiler inDir outDir outFile = do
  let srcDir = outDir </> "src"
  createDirectory srcDir
  setCurrentDirectory outDir
  writeFile (outDir </> "Cargo.toml") cargoFile
  writeFile (srcDir </> "main.rs") libFile
  writeFile (srcDir </> "funs.rs") funs
  copyFile outFile (srcDir </> "test.rs")

  -- actually run the compiler
  compilationResult <- readProcessWithExitCode "cargo" ["check"] ""
  case compilationResult of
    (ExitSuccess, _, _) -> return ()
    (ExitFailure exitCode, stdOut, stdErr) -> error $ toText $ "Target Compiler Compilation Failed: " <> stdErr
