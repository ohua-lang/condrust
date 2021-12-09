module Integrations.Rust.Utils
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

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM
import Data.Text as T (Text, concat)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Rust.Parser (Span, parse', readInputStream)
import Language.Rust.Pretty (pretty')
import Language.Rust.Quote
import Language.Rust.Syntax
import Ohua.Compile.Compiler
import qualified Ohua.Integration.Architecture as Arch
import qualified Ohua.Integration.Config as IC
import Ohua.Prelude
import System.Directory (copyFile, createDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO.Temp
import Test.Hspec
import TestOptions
import Ohua.Core.Types (Options)
import System.Process.Extra (readProcessWithExitCode)


renderRustCode :: SourceFile Span -> L.ByteString
renderRustCode =
  encodeUtf8
    . (<> "\n")
    . renderLazy
    . layoutSmart defaultLayoutOptions
    . pretty'


integrationOptions :: IC.Config
integrationOptions = IC.Config Arch.SharedMemory $ IC.Options Nothing Nothing

compileCodeWithRec :: SourceFile Span -> IO (SourceFile Span)
compileCodeWithRec inCode = runReaderT (compileCode' inCode $ withRec def) def

compileCodeWithRecWithDebug :: SourceFile Span -> IO (SourceFile Span)
compileCodeWithRecWithDebug inCode = runReaderT (compileCode' inCode $ withRec def) $ DebugOptions True False

compileCode :: SourceFile Span -> IO (SourceFile Span)
compileCode inCode = runReaderT (compileCode' inCode def) def

compileCodeWithDebug :: SourceFile Span -> IO (SourceFile Span)
compileCodeWithDebug inCode = runReaderT (compileCode' inCode def) $ DebugOptions True False

compileCode' :: SourceFile Span -> Options -> ReaderT DebugOptions IO (SourceFile Span)
compileCode' inCode opts = do
  debug <- asks printIRs
  lift $ withSystemTempDirectory
    "testDir"
    $ \testDir -> do
      setCurrentDirectory testDir
      writeFile (testDir </> "funs.rs") funs
      writeFile (testDir </> "benchs.rs") benchs
      writeFile (testDir </> "std.rs") std
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
          -- run the target compiler (i.e., rustc) on the input
          runTargetCompiler testDir outDir outFile
          -- parse & return the generated output file
          outCode :: SourceFile Span <-
            parse' <$> readInputStream outFile
          return outCode

showCode :: T.Text -> SourceFile Span -> IO T.Text
showCode msg code = runReaderT (showCode' msg code) def

showCodeWithDiff :: T.Text -> SourceFile Span -> IO T.Text
showCodeWithDiff msg code = runReaderT (showCode' msg code) $ DebugOptions False True

showCode' :: T.Text -> SourceFile Span -> ReaderT DebugOptions IO T.Text
showCode' msg code =
  let c = renderStrict $ layoutSmart defaultLayoutOptions $ pretty' code
   in do
        showDiff <- asks showCodeDiff
        lift $ when showDiff $ printCode c
        return c
  where
    printCode c = putStr $ boundary <> header <> c <> boundary
    boundary = "\n" <> T.concat (replicate 20 ("-" :: T.Text)) <> "\n"
    header = msg <> "\n\n"

runTargetCompiler :: FilePath -> FilePath -> FilePath -> IO ()
runTargetCompiler inDir outDir outFile = do
  let srcDir = outDir </> "src"
  createDirectory srcDir
  setCurrentDirectory outDir
  writeFile (outDir </> "Cargo.toml") cargoFile
  writeFile (srcDir </> "main.rs") libFile
  writeFile (srcDir </> "funs.rs") funs
  writeFile (srcDir </> "benchs.rs") benchs
  copyFile outFile (srcDir </> "test.rs")

  -- actually run the compiler
  compilationResult <- readProcessWithExitCode "cargo" ["check"] ""
  case compilationResult of
    (ExitSuccess, _, _) -> return ()
    (ExitFailure exitCode, stdOut, stdErr) -> error $ toText $ "Target Compiler Compilation Failed: " <> stdErr

cargoFile :: Text
cargoFile =
  " [package] \n\
  \ name = \"ohua-test\" \n\
  \ version = \"0.1.0\" \n\
  \ edition = \"2021\" \n\
  \ \n\
  \ [dependencies] \n\
  \ "

libFile :: Text
libFile =
  " \
  \ mod funs; \
  \ mod benchs; \
  \ mod test; \
  \ \
  \ fn main() { \
  \     test::test(); \
  \ } \
  \ "

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
  \   fn has_next(&self) -> bool { unimplemented!{} } \
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
  \ pub fn reassign_values(v: Vec<Value>, centroids: Arc<Vec<Centroid>>) -> (Vec<Value>, u32) \
  \ { unimplemented!() } \
  \ pub fn reassign_value(v: Value, centroids: Arc<Vec<Centroid>>) -> (Value, u32) \
  \ { unimplemented!() } \
  \ \
  \ pub fn create_centroids(values: Vec<Vec<Value>>, centroids: Arc<Vec<Centroid>>) -> (Vec<Vec<Value>>, Arc<Vec<Centroid>>) \
  \ { unimplemented!() } \
  \ pub fn evaluate_results(new_results: Vec<(Vec<Value>, u32)>) -> (Vec<Value>, f32) \
  \ { unimplemented!() } \
  \ pub fn should_continue(delta: f32, threshold: f32, iterations: u32) -> bool \
  \ { unimplemented!() } \
  \ pub fn inc(it: u32) -> u32 \
  \ { unimplemented!() } \
  \ \
  \ \
  \ \
  \ struct Netlist {} \
  \ enum MoveDecision { Good, Bad, Rejected } \
  \ impl Netlist { \
  \     pub fn update(&mut self, switch_info: (MoveDecision, (usize, usize))) -> Result<MoveDecision, (usize, usize)> \
  \     { unimplemented!() } \
  \ \
  \     pub fn clear_changes(&mut self) { unimplemented!() } \
  \ } \
  \ \
  \ struct Location {} \
  \ impl ChaCha12Rng { \
  \ } \
  \ \
  \ pub fn increment(completed_steps: &i32) -> i32 { \
  \     completed_steps + 1 \
  \ } \
  \  \
  \ pub fn reduce_temp(temperature: f64) -> f64 { \
  \     temperature / 1.5 \
  \ } \
  \ \
  \ fn filter_work(work: Vec<Result<MoveDecision, (usize, usize)>>) -> Vec<Result<MoveDecision, (usize, usize)>> { \
  \     unimplemented!() \
  \ } \
  \ \
  \ pub fn process_move(item: Result<MoveDecision, (usize, usize)>, netlist: Arc<Netlist>, temperature: f64) -> (MoveDecision, (usize, usize)) { \
  \     unimplemented!() \
  \ } \
  \ \
  \ struct InternalState {} \
  \ impl InternalState { \
  \     pub fn initialize(total_elements: usize, max_steps: Option<i32>, swaps_per_temp: usize) -> Self { unimplemented!() } \
  \     pub fn generate_worklist(&mut self) -> Vec<Result<MoveDecision, (usize, usize)>> { \
  \         unimplemented!() \
  \     } \
  \     pub fn assess_updates(&mut self, results: Vec<MoveDecision>, length: usize) -> (Vec<Result<MoveDecision, (usize, usize)>>, bool) { \
  \         unimplemented!() \
  \     } \
  \ } \
  \ \
  \ struct NetlistAndInternal {} \
  \ impl NetlistAndInternal { \
  \     pub fn update(&mut self, switch_info: (MoveDecision, (usize, usize))) -> Result<MoveDecision, (usize, usize)> \
  \     { unimplemented!() } \
  \\
  \     pub fn get_keep_going(&self) -> bool { unimplemented!() } \
  \\
  \ } \
  \ \
  \ pub fn id<T>(item: T) -> T { unimplemented!() } \
  \ pub fn dup<T>(item: T) -> (T, T) { unimplemented!() } \
  \ pub fn extract(item: Arc<Netlist>) -> Netlist { unimplemented!() } \
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
  \   pub fn len(&self) -> usize { unimplemented!() } \
  \   pub fn exp(&mut self, other: Self) { unimplemented!() } \
  \ } \
  \ \
  \ enum Option<T> {} \
  \ "

-- We'd normally have this in the impl block: pub const fn new() -> Self { unimplemented!() }
