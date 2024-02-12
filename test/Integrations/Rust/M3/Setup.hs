module Integrations.Rust.M3.Setup
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

import Ohua.Commons.Prelude hiding (Text)
import Ohua.Compile.Compiler (compile)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM
import Data.Text.Lazy as T (Text)
import Data.Text.Prettyprint.Doc


import Language.Rust.Parser (Span, parse', readInputStream)
import Language.Rust.Pretty ( pretty')
import Language.Rust.Quote
import Language.Rust.Syntax

import qualified Ohua.Integration.Architecture as Arch
import qualified Ohua.Integration.Config as IC
import qualified Ohua.Backend.Config as BC
import qualified Ohua.Integration.Options as O
import qualified Ohua.Integration.Rust.Util as Util
import Integrations.Rust.CommonSetup

import Ohua.Core.Types (Options)

import System.Directory
    ( copyFile,
      createDirectory,
      setCurrentDirectory,)
import System.Process.Extra (readProcessWithExitCode)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO.Temp

import Test.Hspec
import TestOptions
import Integrations.TestSetup (Testable(..))

import Integrations.Rust.M3.TestCode.KVAppCode (driverLib, loopLib, m3Lib, localSmoltcp)



instance Testable (SourceFile Span) where
  -- Todo: replace by Lang' when I'm done messing with the types :-)
  type CodeFormat (SourceFile Span) = (SourceFile Span)

  compileFormat = compileModule
  renderFormat = toStrict . renderRust

renderRust :: SourceFile Span -> T.Text
renderRust = fromString . Util.renderStr

renderRustCode :: SourceFile Span -> L.ByteString
renderRustCode = Util.render

backendOptions :: BC.Options
backendOptions = BC.Options False

integrationOptions :: IC.Config
integrationOptions = IC.Config Arch.M3 $ O.Options Nothing Nothing


compileModule :: SourceFile Span -> Options -> CompilationType -> ReaderT DebugOptions IO (SourceFile Span)
compileModule inCode opts cty = do
  debug <- asks printIRs
  lift $ withSystemTempDirectory
    "testDir"
    $ \testDir -> do
      setCurrentDirectory testDir
      --  write library files to compile directory/scope
      let inFile = testDir </> "test.rs"
      mapM_
        (\(f,c) -> L.writeFile (testDir </> f) $ renderRustCode c)
        [ ("funs.rs"  , funs)
        , ("driver.rs", driverLib)
        , ("m3.rs"       , m3Lib)
        , ("local_smoltcp.rs",localSmoltcp)
        , ("loop_lib.rs" , loopLib)
        , ("test.rs"  , inCode)
        ]
      withSystemTempDirectory "output" $
        \outDir -> do
          let compScope = HM.empty
          let cOptions = if debug then withDebug opts else opts
          runErrAndLogM
            LevelWarn
            $ compile inFile compScope cOptions backendOptions integrationOptions outDir
          let outFile = outDir </> takeFileName inFile

          producedCode <- readFile outFile
          -- putStr ("\n PRODUCED MODULE: \n"::String)
          -- putStr producedCode
          -- putStr ("\n \n \n"::String)
          -- placeholderFile <-readFile (outDir </> "placeholderlib.rs")
          -- putStr placeholderFile

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
runTargetCompiler _inDir outDir outFile = do
  let srcDir = outDir </> "src"
  createDirectory srcDir
  setCurrentDirectory outDir
  _ <- mapM
    (\(f,c) -> L.writeFile (srcDir </> f) $ renderRustCode c)
    [ ("funs.rs"  , funs)
    , ("benchs.rs", benchs)
    , ("main.rs"  , libFile)
    ]
  writeFile (outDir </> "Cargo.toml") $ toStrict cargoFile
  copyFile outFile (srcDir </> "test.rs")

  -- actually run the compiler
  compilationResult <- readProcessWithExitCode "cargo" ["check"] ""
  case compilationResult of
    (ExitSuccess, _, _) -> return ()
    (ExitFailure _exitCode, _stdOut, stdErr) -> error $ toText $ "Target Compiler Compilation Failed: " <> stdErr
