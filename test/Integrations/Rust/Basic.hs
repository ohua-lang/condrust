{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.Basic where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils


spec :: Spec
spec = 
    describe "Basics" $ do
        it "a function" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                use funs::hello_world;

                fn test() -> String {
                    hello_world()
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs::hello_world;
                        
                        fn test() -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                let a_0 = hello_world();
                                a_0_tx.send(a_0)
                                }));
                            run(tasks);
                            a_0_rx.recv()
                        }
                    |]
                compiled `shouldBe` expected)
        it "simple composition" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*; 
                
                fn test() -> String {
                    let x = f();
                    g(x)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs::*;

                        fn test() -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                loop { let var_0 = x_0_0_rx.recv(); let a_0 = g(var_0); a_0_tx.send(a_0) }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let x_0_0 = f(); x_0_0_tx.send(x_0_0)
                                }));
                            run(tasks);
                            a_0_rx.recv()
                        }
                    |]
                compiled `shouldBe` expected)
        it "env vars" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                use funs;

                fn test(i: i32) -> String {
                    let x = funs::h(i);
                    funs::g(x)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs;

                        fn test(i: i32) -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx)  = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                loop { let var_0 = x_0_0_rx.recv(); let a_0 = funs::g(var_0); a_0_tx.send(a_0) }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let var_0 = i; let x_0_0 = funs::h(var_0); x_0_0_tx.send(x_0_0)
                                }));
                            run(tasks);
                            a_0_rx.recv()
                        }
                    |]
                compiled `shouldBe` expected)
        it "algo loading" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                use funs;

                fn algo(i: i32) -> String {
                    let x = funs::h(i);
                    funs::g(x)
                }

                fn test() -> String {
                    algo(4)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs;
                        
                        fn algo(i: i32) -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                  loop {
                                    let var_0 = x_0_0_rx.recv();
                                    let a_0 = funs::g(var_0);
                                    a_0_tx.send(a_0)
                                  }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                  let var_0 = i;
                                  let x_0_0 = funs::h(var_0);
                                  x_0_0_tx.send(x_0_0)
                                }));
                            run(tasks);
                            a_0_rx.recv()
                        }
                        
                        fn test() -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                  loop {
                                    let var_0 = x_0_0_rx.recv();
                                    let a_0 = funs::g(var_0);
                                    a_0_tx.send(a_0)
                                  }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                  let var_0 = 4;
                                  let x_0_0 = funs::h(var_0);
                                  x_0_0_tx.send(x_0_0)
                                }));
                            run(tasks);  
                            a_0_rx.recv()
                        }
                    |]
                compiled `shouldBe` expected)
