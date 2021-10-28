module Integrations.Python.Utils (
    renderPython, showCode, compileCode, compileCodeWithRec,
    module Test.Hspec,
) where

import Ohua.Prelude

import Test.Hspec

import Ohua.Core.Types.Environment (stageHandling, Options, transformRecursiveFunctions)
import Ohua.Core.Types.Stage (DumpCode(..), StageHandling)
import Ohua.Core.Stage

import Ohua.Compile.Compiler
import Ohua.Compile.Config (intoStageHandling, Stage(..))
import qualified Data.HashMap.Lazy as HM

import Language.Python.Common.AST
import Language.Python.Version3 as V3
import Language.Python.Common (prettyText, Token)
import Language.Python.Common.ParserMonad (ParseError)
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
import Language.Python.Common.SrcLocation (SrcSpan)
import Test.Hspec.Discover (String)


type ParseResult = Either ParseError (ModuleSpan, [Token])

-- TODO turn this into a parameter for a particular test
-- TODO: remove redundancy among rust.utils and python.utils
debug :: Bool
debug = False

withDebug :: Options -> Options
withDebug d = d & stageHandling .~ debugStageHandling

withRec :: Options -> Options
withRec d = d & transformRecursiveFunctions .~ True

debugStageHandling :: StageHandling
debugStageHandling =
    intoStageHandling DumpStdOut
        $ Just
            [ Stage resolvedAlang True False
            , Stage normalizedAlang True False
            , Stage coreDflang True False
            , Stage coreAlang True False
            , Stage initialDflang True False
            , Stage preControlSTCLangALang True False
            , Stage smapTransformationALang True False
            , Stage conditionalsTransformationALang True False
            , Stage seqTransformationALang True False
            , Stage postControlSTCLangALang True False
            , Stage normalizeAfterCorePasses True False
            , Stage customDflang True False
            , Stage finalDflang True False
            ]

renderPython:: (PyPretty.Pretty a)=> a -> Text
renderPython = T.pack . prettyText

compileCodeWithRec :: Module SrcSpan -> IO (Module SrcSpan)
compileCodeWithRec inCode = compileCode' inCode $ withRec def

compileCode ::  Module SrcSpan -> IO (Module SrcSpan)
compileCode inCode = compileCode' inCode def

wrappedParsing:: String -> String -> Module SrcSpan
wrappedParsing pyCode filename = do
    let parseresult = V3.parseModule pyCode filename
    case parseresult of
        Left parse_error -> error $ T.pack $ prettyText parse_error
        Right (mod_span, _comments) -> mod_span

--TODO: Change return types once compiling is implemented 
compileCode' ::  Module SrcSpan  -> Options -> IO (Module SrcSpan)
compileCode' inCode opts =
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
                        $ compile inFile compScope options outDir
                    (caller:modules) <- listDirectory outDir
                    mapM_ putStr (caller:modules)
                    files <- mapM (\name -> readFile (outDir </> name)) (caller:modules)
                    producedFile <-readFile (outDir </> "algo.py")
                    newMain <-readFile (outDir </> "test.py")
                    putStr newMainStr
                    putStr $ newMain  <> "\n"
                    putStr algoModStr 
                    putStr $ producedFile  <> "\n"
                    return $ wrappedParsing  (T.unpack producedFile) (takeFileName inFile)
                    {-- Dummy replacement for 'id compilation'
                    writeFile
                        (outDir </> takeFileName inFile)
                        (renderPython inCode)
                    contents <- readFile (outDir </> takeFileName inFile)
                    let outCode = wrappedParsing (T.unpack contents) "test.py"--}

showCode :: T.Text -> Module SrcSpan -> IO T.Text
showCode msg ast =
    let
        c = renderPython ast
    in do
        when debug $ printCode c
        return c
    where
        printCode c = putStr $ boundary <> header <> c <> boundary
        boundary = "\n" <> T.concat (replicate 20 ("-"::T.Text)) <> "\n"
        header = msg <> "\n\n"

newMainStr :: String
newMainStr = "\nNew__MainModule_____________\n"

algoModStr :: String
algoModStr = "\nAlgo__Module_____________\n"