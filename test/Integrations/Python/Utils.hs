module Integrations.Python.Utils (
    renderPython, showCode, compileCode, compileCodeWithRec,
    module Test.Hspec,
) where

import Ohua.Prelude

import Test.Hspec

import TestOptions
import Ohua.Core.Types.Environment (stageHandling, Options, transformRecursiveFunctions)
import Ohua.Core.Types.Stage (DumpCode(..), StageHandling)
import Ohua.Core.Stage

import Ohua.Compile.Compiler
import Ohua.Compile.Config (intoStageHandling, Stage(..))

import qualified Ohua.Integration.Config as IC
import qualified Ohua.Integration.Architecture as Arch


import Language.Python.Common.AST
import Language.Python.Version3 as V3
import Language.Python.Common (prettyText, Token)
import Language.Python.Common.ParserMonad (ParseError)
import Language.Python.Common.SrcLocation (SrcSpan)
import qualified Language.Python.Common.Pretty as PyPretty

import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Output

import System.FilePath
import System.IO.Temp
import System.Directory (setCurrentDirectory, listDirectory, getCurrentDirectory)

import Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text as PP
import Data.Text as T (concat, Text, span, pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM


type ParseResult = Either ParseError (ModuleSpan, [Token])

renderPython:: (PyPretty.Pretty a)=> a -> Text
renderPython = T.pack . prettyText

integrationOptions :: IC.Config
integrationOptions = IC.Config Arch.MultiProcessing $ IC.Options Nothing Nothing

compileCodeWithRec :: Module SrcSpan -> IO (Module SrcSpan)
compileCodeWithRec inCode = runReaderT (compileCode' inCode $ withRec def) def

compileCode ::  Module SrcSpan -> IO (Module SrcSpan)
compileCode inCode = runReaderT (compileCode' inCode def) def

wrappedParsing:: String -> String -> Module SrcSpan
wrappedParsing pyCode filename = do
    let parseresult = V3.parseModule pyCode filename
    case parseresult of
        Left parse_error -> error $ T.pack $ prettyText parse_error
        Right (mod_span, _comments) -> mod_span


--TODO: Change return types once compiling is implemented 
compileCode' ::  Module SrcSpan -> Options -> ReaderT DebugOptions IO (Module SrcSpan)
compileCode' inCode opts = do 
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
                    -- newMain <-readFile (outDir </> "test.py")
                    -- putStr newMainStr
                    -- putStr $ newMain  <> "\n"
                    -- putStr algoModStr 
                    -- putStr $ producedFile  <> "\n"
                    return $ wrappedParsing  (T.unpack producedFile) (takeFileName inFile)
                   
showCode :: T.Text -> Module SrcSpan -> IO T.Text
showCode msg code = runReaderT (showCode' msg code) def

showCodeWithDiff :: T.Text -> Module SrcSpan -> IO T.Text
showCodeWithDiff msg code = runReaderT (showCode' msg code) $ DebugOptions False True

showCode' :: T.Text -> Module SrcSpan -> ReaderT DebugOptions IO T.Text
showCode' msg ast =
    let
        c = renderPython ast
    in do
        showDiff <- asks showCodeDiff
        lift $ when showDiff $ printCode c
        return c
    where
        printCode c = putStr $ boundary <> header <> c <> boundary
        boundary = "\n" <> T.concat (replicate 20 ("-"::T.Text)) <> "\n"
        header = msg <> "\n\n"

newMainStr :: String
newMainStr = "\nNew__MainModule_____________\n"

algoModStr :: String
algoModStr = "\nAlgo__Module_____________\n"

filesHint :: String
filesHint = "\n\nPRODUCED THE FOLLOWING FILES _______________:\n\n"