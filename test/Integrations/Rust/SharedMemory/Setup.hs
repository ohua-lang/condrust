{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.Setup
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
import Data.Text.Lazy as T (Text)
import Data.Text.Prettyprint.Doc
--import Data.Text.Prettyprint.Doc.Render.Text
import Language.Rust.Parser (Span, parse', readInputStream)
import Language.Rust.Pretty ( pretty')
import Language.Rust.Quote
import Language.Rust.Syntax
import Ohua.Compile.Compiler (compile)
import qualified Ohua.Integration.Architecture as Arch
import qualified Ohua.Integration.Config as IC
import qualified Ohua.Backend.Config as BC
import qualified Ohua.Integration.Options as O
import qualified Ohua.Integration.Rust.Util as Util
import Ohua.Prelude hiding (Text)
import System.Directory (copyFile, createDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO.Temp
import Test.Hspec
import TestOptions
import Integrations.TestSetup (Testable(..))
import Ohua.Core.Types (Options)
import System.Process.Extra (readProcessWithExitCode)



instance Testable (SourceFile Span) where
  -- Todo: replace by Lang' when I'm done messing with the types :-)
  type CodeFormat (SourceFile Span) = (SourceFile Span)

  compileFormat = compileModule
  renderFormat = toStrict . renderRust

renderRust :: SourceFile Span -> T.Text
-- FIXME: There should be a renderText in Util, however in the lates version I have there's only 'render' and 'renderStr'
renderRust = fromString . Util.renderStr

renderRustCode :: SourceFile Span -> L.ByteString
renderRustCode = Util.render

backendOptions :: BC.Options
backendOptions = BC.Options False

integrationOptions :: IC.Config
integrationOptions = IC.Config Arch.SharedMemory $ O.Options Nothing Nothing


compileModule :: SourceFile Span -> Options -> CompilationType -> ReaderT DebugOptions IO (SourceFile Span)
compileModule inCode opts cty = do
  debug <- asks printIRs
  lift $ withSystemTempDirectory
    "testDir"
    $ \testDir -> do
      setCurrentDirectory testDir
      let inFile = testDir </> "test.rs"
      _ <- mapM
        (\(f,c) -> L.writeFile (testDir </> f) $ renderRustCode c)
        [ ("funs.rs"  , funs)
        , ("benchs.rs", benchs)
        -- TODO with the new extern_spec approach, the creation of these file is obsolete now.
        , ("std.rs"   , std)
        , ("ptdr.rs"  , ptdr)
        , ("test.rs"  , inCode)
        ]
      withSystemTempDirectory "output" $
        \outDir -> do
          let compScope = HM.empty
          let options = if debug then withDebug opts else opts
          runErrAndLogM
            LevelWarn
            $ compile inFile compScope options backendOptions integrationOptions outDir
          let outFile = outDir </> takeFileName inFile

          -- producedCode <- readFile outFile
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

cargoFile :: Text
cargoFile =
  " [package] \n\
  \ name = \"ohua-test\" \n\
  \ version = \"0.1.0\" \n\
  \ edition = \"2021\" \n\
  \ \n\
  \ [dependencies] \n\
  \ "

libFile :: SourceFile Span
libFile = [sourceFile|
mod funs;
mod benchs;
mod test;

fn main() {
    test::test();
}
|]

funs :: SourceFile Span
funs = [sourceFile|
use std::ops::Range;

#[derive(Copy, Clone, Debug)]
pub struct State{
    val:i32,
}


impl State {
    pub fn new(val:i32) -> Self {
        State{val}
    }

    pub fn new_state(val:i32) -> Self {
        State{val}
    }

    pub fn do_stuff(&self, i:i32) -> i32 {
        i
    }

    pub fn io(&self) {
        println!("LOOOP gnihihi\n")
    }

    pub fn gs(&mut self, num:i32) -> i32 {
        self.val += num;
        return self.val
    }

    pub fn do_it(&mut self, input:String) -> () {
        println!("Got {:}", input);
        self.val += 1;
    }

   pub fn get_num(&self) -> i32 {
       self.val
   }

   pub fn clone(&self) -> State { }

   pub fn use_arc(&self, arc:Arc<i32>) -> String {
        "works for me".into()
   }

}

pub fn hello_world() -> String {
   String::from("Hello, world!")
}
pub fn h(i:i32) -> i32 {
    let the_answer = if i==23 {i} else {i+1};
    the_answer
}
pub fn h_Arc(i:Arc<i32>) -> i32 {
    let the_answer = if *i==23 {23} else {24};
    the_answer
}

pub fn f() -> i32 { 1 }

#[extern_spec(std::sync)]
impl Arc {
  pub fn new(i:i32) -> Arc{ }
  pub fn clone(&self) -> Arc{ }
}

pub fn g() -> String {
    let the_answer = String::from("Hello, world!");
    the_answer
}

pub fn h2(i:i32) -> i32 {
    let the_answer = if i==23 {i+19} else {42};
    println!("Calls h2 with {}", i);
    the_answer
}

pub fn h3(i:i32, j:i32) -> () {
    let the_answer = if i==23 {j} else {42};
    println!("Calls h2 with {}", i)
}

pub fn h4(i:i32, j:i32) -> i32 {
    i+j
}


pub fn take_triple(i:i32, j:i32, k:usize) -> i32 {
    i+j
}


pub fn check(i:i32) -> bool {
    i < 23
}

pub fn host_id(i:bool) -> (bool, bool) {
    (i,i)
}

pub fn iter_i32() -> Vec<i32> {
    (1..11).collect()
}

pub type ThisIsActuallyUnit = ();

pub fn make_unit() -> ThisIsActuallyUnit {
    ()
}

pub fn some_int() -> i32 {
    11
}

pub fn take_char_i(c:Char, i:i32) -> String {
    format!("Got {} and {}", c, i)
}

pub fn take_string(s:String) -> usize {
    s.len()
}

pub fn int_and_string(i:i32, s:String) -> i32 {
    i+j
}
pub fn f_s(state:State, i:i32) -> i32 {
    println!("State: {:?}, i: {:?}", state.val, i);
    state.val - i
}

pub fn random_bool() -> bool {
   true
}

pub fn somefun() -> i32 {
   42
}

pub fn otherfun() -> i32 {
   23
}

pub fn from_int(i:i32) -> String {
   String::new("Actually I ignore the input")
}
|]

benchs :: SourceFile Span
benchs = [sourceFile|

struct Maze {}
impl Maze {
  fn init(dimensions: Point) -> Self
  { unimplemented!() }

  fn update(&mut self, path: Option<Path>) -> Option<(Point,Point)>
  { unimplemented!() }
}

struct Path {}

struct Point {}

fn find_path(m: Arc<Maze>, pair: (Point, Point)) -> Option<Path>
{ unimplemented!() }

fn get_unmapped(results: Vec<Option<(Point,Point)>>, its_left: u32) -> (Vec<(Point,Point)>,bool, u32)
{ unimplemented!() }

fn filter_mapped(results: Vec<Option<(Point,Point)>>) -> Vec<Option<(Point,Point)>>
{ unimplemented!() }

fn calculate_done(results: Vec<Option<(Point,Point)>>, its_left: u32) -> (u32, bool)
{ unimplemented!() }

fn decrement(u: u32) -> u32
{ unimplemented!() }

fn fill1(m: Maze, p:Vec<(Point,Point)>, ma:u32) -> Maze
{ unimplemented!() }


struct OptionData{}
pub fn calculate_black_scholes(op: OptionData) -> f32 { unimplemented!() }
pub fn batch_calculate_black_scholes(op: Vec<OptionData>) -> Vec<f32> { unimplemented!() }
pub fn unpack(v: Vec<Vec<f32>>) -> Vec<f32> { unimplemented!() }



struct Value {}

pub fn reassign_values(v: Vec<Value>, centroids: Arc<Vec<Centroid>>) -> (Vec<Value>, u32)
{ unimplemented!() }
pub fn reassign_value(v: Value, centroids: Arc<Vec<Centroid>>) -> (Value, u32)
{ unimplemented!() }

pub fn create_centroids(values: Vec<Vec<Value>>, centroids: Arc<Vec<Centroid>>) -> (Vec<Vec<Value>>, Arc<Vec<Centroid>>)
{ unimplemented!() }
pub fn evaluate_results(new_results: Vec<(Vec<Value>, u32)>) -> (Vec<Value>, f32)
{ unimplemented!() }
pub fn should_continue(delta: f32, threshold: f32, iterations: u32) -> bool
{ unimplemented!() }
pub fn inc(it: u32) -> u32
{ unimplemented!() }

struct Netlist {}
enum MoveDecision { Good, Bad, Rejected }
impl Netlist {
    pub fn update(&mut self, switch_info: (MoveDecision, (usize, usize))) -> Result<MoveDecision, (usize, usize)>
    { unimplemented!() }

    pub fn clear_changes(&mut self) { unimplemented!() }
}

struct Location {}
impl ChaCha12Rng {
}

pub fn increment(completed_steps: &i32) -> i32 {
    completed_steps + 1
}

pub fn reduce_temp(temperature: f64) -> f64 {
    temperature / 1.5
}

fn filter_work(work: Vec<Result<MoveDecision, (usize, usize)>>) -> Vec<Result<MoveDecision, (usize, usize)>> {
    unimplemented!()
}

pub fn process_move(item: Result<MoveDecision, (usize, usize)>, netlist: Arc<Netlist>, temperature: f64) -> (MoveDecision, (usize, usize)) {
    unimplemented!()
}

struct InternalState {}
impl InternalState {
    pub fn initialize(total_elements: usize, max_steps: Option<i32>, swaps_per_temp: usize) -> Self { unimplemented!() }
    pub fn generate_worklist(&mut self) -> Vec<Result<MoveDecision, (usize, usize)>> {
        unimplemented!()
    }
    pub fn assess_updates(&mut self, results: Vec<MoveDecision>, length: usize) -> (Vec<Result<MoveDecision, (usize, usize)>>, bool) {
        unimplemented!()
    }
}

struct NetlistAndInternal {}
impl NetlistAndInternal {
    pub fn update(&mut self, switch_info: (MoveDecision, (usize, usize))) -> Result<MoveDecision, (usize, usize)>
    { unimplemented!() }

    pub fn get_keep_going(&self) -> bool { unimplemented!() }
}

pub fn dup<T>(item: T) -> (T, T) { unimplemented!() }
pub fn extract(item: Arc<Netlist>) -> Netlist { unimplemented!() }

|]

std :: SourceFile Span
std = [sourceFile|
struct Vec<T> {}
impl<T> Vec<T> {
  pub fn default() -> Self { unimplemented!() }
  pub fn push(&mut self, value: T) { unimplemented!() }
  pub fn evict_mapped(&mut self) { unimplemented!() }
  pub fn calculate_done1(&mut self, its_left: u32) -> bool { unimplemented!() }
  pub fn len(&self) -> usize { unimplemented!() }
  pub fn exp(&mut self, other: Self) { unimplemented!() }
}

enum Option<T> {}

struct Arc<T> {}
impl<T> Arc<T> {
  fn new(i: T) -> Self { unimplemented!() }
}
impl<T> Clone for Arc<T> {
  fn clone(&self) -> Self { unimplemented!() }
}

pub fn id<T>(item: T) -> T { unimplemented!() }
|]
-- We'd normally have this in the impl block: pub const fn new() -> Self { unimplemented!() }

ptdr :: SourceFile Span
ptdr = [sourceFile|
pub fn sub(a: Duration, b: Duration) -> Duration { unimplemented!() }
struct TimeRange {}

impl TimeRange {
    pub fn new1(start: DateTime<Utc>, duration: Duration) -> TimeRange { unimplemented!() }
    pub fn step_by(&self, duration: Duration) -> TimeRangeStepIterator { unimplemented!() }
}

struct TimeRangeStepIterator {}

struct DateTime<T> {}

struct Duration {}

struct Utc {}

struct Simulation {}

impl Simulation {
    pub fn new2(p: &str) -> Simulation { unimplemented!() }
    pub fn add(&mut self, simulation: SingleSimulation) -> Simulation { unimplemented!() }
}

struct SingleSimulation {}

impl SingleSimulation {
    pub fn new3(departure_time: DateTime<Utc>, samples: usize) -> SingleSimulation { unimplemented!() }
    pub fn set_samples(&mut self, samples: Vec<Duration>) { unimplemented!() }
}

struct Route {}

impl Route {
    pub fn drive(&self, departure_time: DateTime<Utc>, prob_profiles: Arc<P>) -> Duration { unimplemented!() }
}

struct Quartiles {}

struct NoLimitProbProfile {}

impl NoLimitProbProfile {
    pub fn new4() -> NoLimitProbProfile { unimplemented!() }
}

struct SegmentsHistoryProbProfile {}

pub fn sample_range(upper: usize) -> Range<usize> { unimplemented!() }

struct Range<T> {}

impl String {
    fn as_str(&self) -> &str { unimplemented!() }
}
|]
