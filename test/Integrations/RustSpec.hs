{-# LANGUAGE QuasiQuotes #-}
module Integrations.RustSpec
    ( spec
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

import Language.Rust.Pretty ( pretty' )
import Language.Rust.Quote
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (parse', readInputStream, Span)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc.Render.String (renderShowS)
import Data.Text as T (concat, Text)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy.IO as LT

debug :: Bool
debug = True

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
        $ Just [Stage coreDflang True False]

compileCode :: SourceFile Span -> IO (SourceFile Span)
compileCode inCode = 
    withSystemTempFile 
        "test.rs" 
        $ \inFile _inHandle -> do
            hClose _inHandle
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

spec :: Spec
spec = 
    describe "Rust integration (compilation) " $ do
        it "simple" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                fn test() -> String {
                    let x = f();
                    g(x)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        fn test() -> String {
                        let x_0_0 = ohua::arcs::Channel::new(1);
                        let a_0 = ohua::arcs::Channel::new(1);
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop { let var_0 = x_0_0.recv(0); let result = g(var_0); a_0.send(result) }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                              let result = f(); x_0_0.send(result)
                            }));
                        run(tasks);
                        a_0.recv(0)
                        }
                    |]
                compiled `shouldBe` expected)
        it "env vars" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                fn test(i: i32) -> String {
                    let x = f(i);
                    g(x)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        fn test(i: i32) -> String {
                        let x_0_0 = ohua::arcs::Channel::new(1);
                        let a_0 = ohua::arcs::Channel::new(1);
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop { let var_0 = x_0_0.recv(0); let result = g(var_0); a_0.send(result) }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                              let var_0 = i; let result = f(var_0); x_0_0.send(result)
                            }));
                        run(tasks);
                        a_0.recv(0)
                        }
                    |]
                compiled `shouldBe` expected)
        it "simple condition" $ -- in most languages, a condition is not a function!
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                fn test(i: i32) -> String {
                    let a = f0(i);
                    let b = f1(i);
                    let c = f2(i);
                    let d = if a {
                        g0(b)
                    } else {
                        g1(c)
                    };
                    h(d)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        fn test(i: i32) -> String {
                        let a_0_0 = ohua::arcs::Channel::new(2);
                        let b_0_0 = ohua::arcs::Channel::new(1);
                        let c_0_0 = ohua::arcs::Channel::new(1);
                        let ctrlTrue_0 = ohua::arcs::Channel::new(1);
                        let ctrlFalse_0 = ohua::arcs::Channel::new(1);
                        let e_0 = ohua::arcs::Channel::new(1);
                        let f_0 = ohua::arcs::Channel::new(1);
                        let result_0 = ohua::arcs::Channel::new(1);
                        let g_0 = ohua::arcs::Channel::new(1);
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let renew = false;
                                let c_0_0_0 = c_0_0.recv(0);
                                while !renew {
                                let sig = ctrlFalse_0.recv(0);
                                let count = sig.1;
                                for _ in [0; count] {
                                    let var_0 = c_0_0_0;
                                    let result = g1(var_0);
                                    f_0.send(result)
                                };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                                };
                                ()
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let renew = false;
                                let b_0_0_0 = b_0_0.recv(0);
                                while !renew {
                                let sig = ctrlTrue_0.recv(0);
                                let count = sig.1;
                                for _ in [0; count] {
                                    let var_0 = b_0_0_0;
                                    let result = g0(var_0);
                                    e_0.send(result)
                                };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                                };
                                ()
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let var_0 = result_0.recv(0);
                                let result = h(var_0);
                                g_0.send(result)
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let branchSelection = a_0_0.recv(0);
                                if branchSelection {
                                let result = e_0.recv(0);
                                result_0.send(result)
                                } else { let result = f_0.recv(0); result_0.send(result) }
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let branchSelection = a_0_0.recv(0);
                                if branchSelection {
                                let ctrlTrue = (true, 1);
                                let ctrlFalse = (true, 0);
                                ctrlTrue_0.send(ctrlTrue);
                                ctrlFalse_0.send(ctrlFalse)
                                } else {
                                let ctrlTrue = (true, 0);
                                let ctrlFalse = (true, 1);
                                ctrlTrue_0.send(ctrlTrue);
                                ctrlFalse_0.send(ctrlFalse)
                                }
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let result = f2(var_0);
                            c_0_0.send(result)
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let result = f1(var_0);
                            b_0_0.send(result)
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let result = f0(var_0);
                            a_0_0.send(result)
                            }));
                        run(tasks);
                        g_0.recv(0)
                        }                    
                    |]
                compiled `shouldBe` expected)
