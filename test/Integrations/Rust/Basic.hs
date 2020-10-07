{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.Basic where

import Ohua.Prelude
import Integrations.Rust.Utils


spec :: Spec
spec = 
    describe "Basics" $ do
        it "a function" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                fn test() -> String {
                    hello_world()
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        fn test() -> String {
                        let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            let result = hello_world();
                            a_0_tx.send(result)
                            }));
                        run(tasks);
                        a_0_rx.recv()
                        }
                    |]
                compiled `shouldBe` expected)
        it "simple composition" $
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
                        let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                        let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop { let var_0 = x_0_0_rx.recv(); let result = g(var_0); a_0_tx.send(result) }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                              let result = f(); x_0_0_tx.send(result)
                            }));
                        run(tasks);
                        a_0_rx.recv()
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
                        let (x_0_0_tx, x_0_0_rx)  = std::sync::mpsc::channel();
                        let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop { let var_0 = x_0_0_rx.recv(); let result = g(var_0); a_0_tx.send(result) }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                              let var_0 = i; let result = f(var_0); x_0_0_tx.send(result)
                            }));
                        run(tasks);
                        a_0_rx.recv()
                        }
                    |]
                compiled `shouldBe` expected)
