module Integrations.Rust.Utils (
    renderRustCode, showCode, compileCode,
    module Test.Hspec,
    module Language.Rust.Quote
) where

import Ohua.Prelude

import Test.Hspec

import Ohua.Core.Types.Environment (stageHandling)
import Ohua.Core.Types.Stage (DumpCode(..))
import Ohua.Core.Stage

import Ohua.Compile.Compiler
import Ohua.Compile.Config (intoStageHandling, Stage(..))
import qualified Data.HashMap.Lazy as HM

import System.FilePath
import System.IO.Temp
import System.Directory (setCurrentDirectory)

import Language.Rust.Pretty ( pretty' )
import Language.Rust.Quote
import Language.Rust.Syntax
import Language.Rust.Parser (parse', readInputStream, Span)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text as T (concat, Text)
import qualified Data.ByteString.Lazy.Char8 as L


debug :: Bool
debug = False

renderRustCode :: SourceFile Span -> L.ByteString
renderRustCode = 
    encodeUtf8 . 
    (<> "\n") . 
    renderLazy . 
    layoutSmart defaultLayoutOptions . 
    pretty'

debugOptions = def & stageHandling .~ debugStageHandling 

debugStageHandling = 
    intoStageHandling DumpStdOut
        $ Just [Stage coreDflang True False
                , Stage coreAlang True False
                , Stage initialDflang True False]

compileCode :: SourceFile Span -> IO (SourceFile Span)
compileCode inCode = 
    withSystemTempDirectory "testDir" 
        $ \testDir -> do
            setCurrentDirectory testDir
            writeFile 
                (testDir </> "funs.rs")
                " \
                \ fn hello_world() -> String { unimplemented!{} } \
                \ fn f() -> i32 { unimplemented!{} } \
                \ fn g(i:i32) -> String { unimplemented!{} } \
                \ fn h(i:i32) -> i32 { unimplemented!{} } \
                \ fn f0(i:i32) -> i32 { unimplemented!{} } \
                \ fn f1(i:i32) -> i32 { unimplemented!{} } \
                \ fn f2(i:i32) -> i32 { unimplemented!{} } \
                \ fn g0(i:i32) -> i32 { unimplemented!{} } \
                \ fn g1(i:i32) -> i32 { unimplemented!{} } \
                \ struct S{} \
                \ impl S { \
                \   fn new(i:i32) -> S { unimplemented!{} } \
                \   fn gs(self, i:i32) -> i32 { unimplemented!{} } \
                \ } \
                \ "
            let inFile = testDir </> "test.rs"
            L.writeFile inFile $ renderRustCode inCode
            withSystemTempDirectory "output" 
                $ \outDir -> do
                    let compScope = HM.empty
                    let options = if debug then debugOptions else def 
                    runCompM
                        LevelWarn
                        $ compile inFile compScope options outDir
                    outCode :: SourceFile Span 
                        <- parse' <$> readInputStream (outDir </> takeFileName inFile)
                    return outCode

showCode :: T.Text -> SourceFile Span -> IO T.Text
showCode msg code = 
    let 
        c = renderStrict $ layoutSmart defaultLayoutOptions $ pretty' code
    in do
        when debug $ print c
        return c
    where
        print code = putStr $ boundary <> header <> code <> boundary
        boundary = "\n" <> T.concat (replicate 20 ("-"::T.Text)) <> "\n"
        header = msg <> "\n\n"

