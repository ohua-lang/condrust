module Integrations.Rust.Utils (
    renderRustCode, showCode, compileCode, compileCodeWithRec,
    module Test.Hspec,
    module Language.Rust.Quote
) where

import Ohua.Prelude

import Test.Hspec

import Ohua.Core.Types.Environment (stageHandling, Options, transformRecursiveFunctions)
import Ohua.Core.Types.Stage (DumpCode(..), StageHandling)
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


-- TODO turn this into a parameter for a particular test
debug :: Bool
debug = False

renderRustCode :: SourceFile Span -> L.ByteString
renderRustCode = 
    encodeUtf8 . 
    (<> "\n") . 
    renderLazy . 
    layoutSmart defaultLayoutOptions . 
    pretty'

withDebug :: Options -> Options
withDebug d = d & stageHandling .~ debugStageHandling

withRec :: Options -> Options
withRec d = d & transformRecursiveFunctions .~ True

debugStageHandling :: StageHandling
debugStageHandling = 
    intoStageHandling DumpStdOut
        $ Just 
            [ Stage resolvedAlang True False
            , Stage coreDflang True False
            , Stage coreAlang True False
            , Stage initialDflang True False
            , Stage preControlSTCLangALang True False
            , Stage smapTransformationALang True False
            , Stage postControlSTCLangALang True False
            , Stage normalizeAfterCorePasses True False
            , Stage customDflang True False
            ]

compileCodeWithRec :: SourceFile Span -> IO (SourceFile Span)
compileCodeWithRec inCode = compileCode' inCode $ withRec def

compileCode :: SourceFile Span -> IO (SourceFile Span)
compileCode inCode = compileCode' inCode def

compileCode' :: SourceFile Span -> Options -> IO (SourceFile Span)
compileCode' inCode opts = 
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
                \ fn check(i:i32) -> bool { unimplemented!{} } \
                \\
                \ struct S {} \
                \ impl S { \
                \   fn new(i:i32) -> S { unimplemented!{} } \
                \   fn gs(self, i:i32) -> i32 { unimplemented!{} } \
                \ } \
                \ fn k(s:S) -> () { unimplemented!{} } \
                \\
                \ fn iter() -> Iterator<S> { unimplemented!{} } \
                \ impl Iterator for S { \
                \   type Item=S; \
                \   fn next(&mut self) -> Option<S> { unimplemented!{} } \
                \   fn size_hint(&self) -> (usize, Option<usize>) { unimplemented!{} } \
                \ } \
                \ "
            let inFile = testDir </> "test.rs"
            L.writeFile inFile $ renderRustCode inCode
            withSystemTempDirectory "output" 
                $ \outDir -> do
                    let compScope = HM.empty
                    let options = if debug then withDebug opts else opts
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
        when debug $ printCode c
        return c
    where
        printCode c = putStr $ boundary <> header <> c <> boundary
        boundary = "\n" <> T.concat (replicate 20 ("-"::T.Text)) <> "\n"
        header = msg <> "\n\n"

