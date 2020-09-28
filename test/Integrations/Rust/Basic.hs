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
                        let a_0 = ohua::arcs::Channel::new(1);
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            let result = hello_world();
                            a_0.send(result)
                            }));
                        run(tasks);
                        a_0.recv(0)
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
