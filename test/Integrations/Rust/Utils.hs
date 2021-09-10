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

showCodeDiff :: Bool
showCodeDiff = False

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

compileCodeWithRec :: SourceFile Span -> IO (SourceFile Span)
compileCodeWithRec inCode = compileCode' inCode $ withRec def

compileCode :: SourceFile Span -> IO (SourceFile Span)
compileCode inCode = compileCode' inCode def

compileCode' :: SourceFile Span -> Options -> IO (SourceFile Span)
compileCode' inCode opts =
    withSystemTempDirectory "testDir"
        $ \testDir -> do
            setCurrentDirectory testDir
            writeFile (testDir </> "funs.rs") funs
            writeFile (testDir </> "benchs.rs") benchs
            writeFile (testDir </> "std.rs") std
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
        when showCodeDiff $ printCode c
        return c
    where
        printCode c = putStr $ boundary <> header <> c <> boundary
        boundary = "\n" <> T.concat (replicate 20 ("-"::T.Text)) <> "\n"
        header = msg <> "\n\n"

funs :: Text
funs =
  " \
  \ fn hello_world() -> String { unimplemented!{} } \
  \ fn f() -> i32 { unimplemented!{} } \
  \ fn f_tup() -> (i32,i32) { unimplemented!{} } \
  \ fn fi_tup(i:i32) -> (i32,i32) { unimplemented!{} } \
  \ fn f_arc() -> Arc<i32> { unimplemented!{} } \
  \ fn g(i:i32) -> String { unimplemented!{} } \
  \ fn h(i:i32) -> i32 { unimplemented!{} } \
  \ fn h2(i:i32, j:i32) -> i32 { unimplemented!{} }\
  \ fn f0(i:i32) -> i32 { unimplemented!{} } \
  \ fn f1(i:i32) -> i32 { unimplemented!{} } \
  \ fn f2(i:i32) -> i32 { unimplemented!{} } \
  \ fn g0(i:i32) -> i32 { unimplemented!{} } \
  \ fn g1(i:i32) -> i32 { unimplemented!{} } \
  \ fn check(i:i32) -> bool { unimplemented!{} } \
  \\
  \ struct S {} \
  \ impl S { \
  \   fn new_state(i:i32) -> S { unimplemented!{} } \
  \   fn gs(self, i:i32) -> i32 { unimplemented!{} } \
  \   fn modify(&mut self, i:i32) { unimplemented!{} } \
  \   fn gs1(self, i:i32) -> String { unimplemented!{} } \
  \ } \
  \ fn k(s:S) -> () { unimplemented!{} } \
  \\
  \ fn iter() -> Iterator<S> { unimplemented!{} } \
  \ impl Iterator for S { \
  \   type Item=S; \
  \   fn next(&mut self) -> Option<S> { unimplemented!{} } \
  \   fn size_hint(&self) -> (usize, Option<usize>) { unimplemented!{} } \
  \ } \
  \ impl Clone for S { \
  \   fn clone(&self) -> Self { unimplemented!() } \
  \ } \
  \ fn iter_i32() -> Iterator<i32> { unimplemented!{} } \
  \ fn f_s(s:&S, i:i32) -> i32 { unimplemented!() } \
  \ "

benchs :: Text
benchs =
  " \
  \ struct Maze {} \
  \ impl Maze { \
  \   fn init(dimensions: Point) -> Self \
  \   { unimplemented!() } \
  \ \
  \   fn update(&mut self, path: Option<Path>) -> Option<(Point,Point)> \
  \   { unimplemented!() } \
  \ } \
  \ \
  \ struct Path {} \
  \ \
  \ struct Arc<T> {} \
  \ impl<T> Arc<T> { \
  \   fn new(i: T) -> Self { unimplemented!() } \
  \ } \
  \ impl<T> Clone for Arc<T> { \
  \   fn clone(&self) -> Self { unimplemented!() } \
  \ } \
  \ \
  \ struct Point {} \
  \ \
  \ fn find_path(m: Arc<Maze>, pair: (Point, Point)) -> Option<Path> \
  \ { unimplemented!() } \
  \ \
  \ fn get_unmapped(results: Vec<Option<(Point,Point)>>, its_left: u32) -> (Vec<(Point,Point)>,bool, u32)\
  \ { unimplemented!() } \
  \ \
  \ fn filter_mapped(results: Vec<Option<(Point,Point)>>) -> Vec<Option<(Point,Point)>>\
  \ { unimplemented!() } \
  \ \
  \ fn calculate_done(results: Vec<Option<(Point,Point)>>, its_left: u32) -> (u32, bool)\
  \ { unimplemented!() } \
  \ \
  \ fn decrement(u: u32) -> u32\
  \ { unimplemented!() } \
  \ \
  \ fn fill1(m: Maze, p:Vec<(Point,Point)>, ma:u32) -> Maze \
  \ { unimplemented!() } \
  \ \
  \ \
  \ \
  \ struct OptionData{} \
  \ pub fn calculate_black_scholes(op: OptionData) -> f32 { unimplemented!() } \
  \ pub fn batch_calculate_black_scholes(op: Vec<OptionData>) -> Vec<f32> { unimplemented!() } \
  \ pub fn unpack(v: Vec<Vec<f32>>) -> Vec<f32> { unimplemented!() } \
  \ \
  \ \
  \ \
  \ struct Value {} \
  \ \
  \ pub fn reassign_value(v: Value, centroids: Arc<Vec<Centroid>>) -> (Value, u32) \
  \ { unimplemented!() } \
  \ \
  \ pub fn create_centroids(values: Vec<Value>, centroids: Arc<Vec<Centroid>>) -> (Vec<Value>, Arc<Vec<Centroid>>) \
  \ { unimplemented!() } \
  \ pub fn evaluate_results(new_results: Vec<(Value, u32)>) -> (Vec<Value>, f32) \
  \ { unimplemented!() } \
  \ pub fn should_continue(delta: f32, threshold: f32, iterations: u32) -> bool \
  \ { unimplemented!() } \
  \ pub fn inc(it: u32) -> u32 \
  \ { unimplemented!() } \
  \ \
  \ pub fn id<T>(item: T) -> T { unimplemented!() } \
  \ "

std :: Text
std =
  " \
  \ struct Vec<T> {} \
  \ impl<T> Vec<T> { \
  \   pub fn default() -> Self { unimplemented!() } \
  \   pub fn push(&mut self, value: T) { unimplemented!() } \
  \   pub fn evict_mapped(&mut self) { unimplemented!() } \
  \   pub fn calculate_done1(&mut self, its_left: u32) -> bool { unimplemented!() } \
  \ } \
  \ \
  \ enum Option<T> {} \
  \ "

  -- We'd normally have this in the impl block: pub const fn new() -> Self { unimplemented!() }
