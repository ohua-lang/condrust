module Integrations.Python.PythonSetup (
    renderPython, showCode, compileCode, compileCodeWithRec, compileAndRun,
    module Test.Hspec,
) where

import Ohua.Prelude

import Test.Hspec
import TestOptions
import Ohua.Core.Types.Environment (Options)

import Ohua.Compile.Compiler (compile)

import qualified Ohua.Integration.Config as IC
import qualified Ohua.Integration.Architecture as Arch


import Language.Python.Common.AST
import Language.Python.Version3 as V3
import Language.Python.Common (prettyText, Token)
import Language.Python.Common.ParserMonad (ParseError)
import Language.Python.Common.SrcLocation (SrcSpan)
import qualified Language.Python.Common.Pretty as PyPretty

import Integrations.TestSetup (Testable(..))
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Output

import System.FilePath
import System.IO.Temp
import System.Exit (ExitCode (..))
import System.Process.Extra (readProcessWithExitCode)
import System.Directory (setCurrentDirectory, listDirectory, getCurrentDirectory)

import Data.Text as T (Text, pack, unpack)
import qualified Data.HashMap.Lazy as HM


type ParseResult = Either ParseError (ModuleSpan, [Token])

instance Testable (Module SrcSpan) where
    type CodeFormat (Module SrcSpan) = (Module SrcSpan)

    compileFormat = compileModule
    renderFormat = renderPython

renderPython:: (PyPretty.Pretty a) => a -> Text
renderPython = T.pack . prettyText

integrationOptions :: IC.Config
integrationOptions = IC.Config Arch.MultiProcessing $ IC.Options Nothing Nothing


compileModule ::  Module SrcSpan -> Options -> CompilationType -> ReaderT DebugOptions IO (Module SrcSpan)
compileModule inCode opts compType = do 
    debug <- asks printIRs
    lift $ 
     withSystemTempDirectory "testDir"
        $ \testDir -> do
            setCurrentDirectory testDir
            writeFile
                (testDir </> "testLib.py") 
                (renderPython Input.testLib)
            let inFile = testDir </> "test.py"
            writeFile
                inFile
                (renderPython inCode)
            withSystemTempDirectory "output"
                $ \outDir -> do
                    let compScope = HM.empty
                    let options = if debug then withDebug opts else opts
                    runCompM
                        LevelWarn
                        $ compile inFile compScope options integrationOptions outDir
                    (caller:modules) <- listDirectory outDir
                    -- putStr filesHint
                    -- mapM_ putStr (caller:modules)
                    -- files <- mapM (\name -> readFile (outDir </> name)) (caller:modules)
                    producedFile <-readFile (outDir </> "algo.py")
                    newMain <-readFile (outDir </> "test.py")
                    --putStr newMainStr
                    -- putStr $ newMain  <> "\n"
                    -- putStr algoModStr 
                    -- putStr producedFile
                    -- test the produced code by compiling it, runnning it or just compare it
                    case compType of
                        OhuaOnly -> pure ()
                        BuildTarget -> error "Error: Running target code not implemented yet"
                        RunTarget -> runCode testDir outDir
                    return $ wrappedParsing  (T.unpack producedFile) (takeFileName inFile)



wrappedParsing:: String -> String -> Module SrcSpan
wrappedParsing pyCode filename = do
    let parseresult = V3.parseModule pyCode filename
    case parseresult of
        Left parse_error -> error $ T.pack $ prettyText parse_error
        Right (mod_span, _comments) -> mod_span


runCode:: FilePath -> FilePath -> IO()
-- TODO: This currently doesn't work -> file not founf error on "python"
-- Reasons tested : files not in directories, python not callable from tempdir -> both false
-- Assumend Cause : some nix-depency needs to be added to a) have python b) have multiprocessing 
runCode testDir outDir = do
    (_ , seqStdOut, seqErr) <- readProcessWithExitCode "python" [testDir </> "test.py"] ""
    putStrLn $ seqStdOut <> seqErr

    writeFile
        ( outDir </> "testLib.py") 
        (renderPython Input.testLib)

    compiledResult <- readProcessWithExitCode "timeout" ["5s", "python", outDir </> "test.py"] ""

    case compiledResult of
        (ExitFailure exitCode, stdOut, stdErr) -> error $ toText $ "Running compiled code failed with" <> stdErr
        (ExitSuccess , stdOut, _ ) -> 
            if stdOut == seqStdOut 
                then putStr successMsg
                    else do
                        putStr $ "unequal results sequential: " <> seqStdOut <> " compiled: " <> stdOut
                        return ()




newMainStr :: String
newMainStr = "\nNew__MainModule_____________\n"

algoModStr :: String
algoModStr = "\nAlgo__Module_____________\n"

filesHint :: String
filesHint = "\n\nPRODUCED THE FOLLOWING FILES _______________:\n\n"

successMsg::String
successMsg = "running works, results are equal"

cd::String
cd = "current Dir\n"

