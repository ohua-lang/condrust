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
                                a_0_tx.send(a_0)?
                                }));
                            run(tasks);
                            a_0_rx.recv()?
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
                                loop { let var_0 = x_0_0_rx.recv()?; let a_0 = g(var_0); a_0_tx.send(a_0)? }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let x_0_0 = f(); x_0_0_tx.send(x_0_0)?
                                }));
                            run(tasks);
                            a_0_rx.recv()?
                        }
                    |]
                compiled `shouldBe` expected)
        it "var multi fail" $
            -- enforce Arc construction via type-check
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x = f();
                    let y = h(x);
                    h2(x,y)
                }
                |]) >>=
            (\compiled -> do
                -- expect type error!
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        fn test() -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (y_0_0_tx, y_0_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                    loop {
                                        let var_0 = x_0_0_rx.recv()?;
                                        let var_1 = y_0_0_rx.recv()?;
                                        let a_0 = h2(var_0, var_1);
                                        a_0_tx.send(a_0)?
                                    }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                    loop {
                                        let var_0 = x_0_0_rx.recv()?;
                                        let y_0_0 = h(var_0);
                                        y_0_0_tx.send(y_0_0)?
                                    }
                                }));
                            tasks.push(Box::new(move || -> _ { let x_0_0 = f(); x_0_0_tx.send(x_0_0)? }));
                            run(tasks);
                            a_0_rx.recv()?
                        }
                    |]
                compiled `shouldBe` expected)
        it "var multi 1: read-only" $
          -- due to the transformation to state threads, this transforms into:
          -- let x = f();
          -- let x1 = Arc::new();
          -- let (x2,x1') = x1.arc_clone();
          -- let y = h(x1');
          -- h2(x2,y)
          -- where no variable is used more than once!
          (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x = f();
                    let x1 = Arc::new(x);
                    let x2 = x1.clone();
                    let y = h(x1);
                    h2(x2,y)
                }
                |]) >>=
            (\compiled -> do
                -- expect type error! -> NOTE(feliix42) really? This should work
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        fn test() -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                            let (x1_0_1_tx, x1_0_1_rx) = std::sync::mpsc::channel();
                            let (x1_0_0_0_tx, x1_0_0_0_rx) = std::sync::mpsc::channel();
                            let (y_0_0_tx, y_0_0_rx) = std::sync::mpsc::channel();
                            let (x2_0_0_tx, x2_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                    loop {
                                        let var_0 = x2_0_0_rx.recv()?;
                                        let var_1 = y_0_0_rx.recv()?;
                                        let a_0 = h2(var_0, var_1);
                                        a_0_tx.send(a_0)?
                                    }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                    loop {
                                        let var_0 = x1_0_0_0_rx.recv()?;
                                        let y_0_0 = h(var_0);
                                        y_0_0_tx.send(y_0_0)?
                                    }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                    loop {
                                        let var_0 = x1_0_1_rx.recv()?;
                                        let x2_0_0 = var_0.clone();
                                        x2_0_0_tx.send(x2_0_0)?;
                                        x1_0_0_0_tx.send(var_0)?;
                                        ()
                                    }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                    loop {
                                        let var_0 = x_0_0_rx.recv()?;
                                        let x1_0_1 = Arc::new(var_0);
                                        x1_0_1_tx.send(x1_0_1)?
                                    }
                                }));
                            tasks.push(Box::new(move || -> _ { let x_0_0 = f(); x_0_0_tx.send(x_0_0)? }));
                            run(tasks);
                            a_0_rx.recv()?
                        }
                    |]
                compiled `shouldBe` expected)
        it "var multi 2: explicit clone" $
          -- due to the transformation to state threads, this transforms into:
          -- let x = f();
          -- let (x1,x') = x.clone();
          -- let y = h(x');
          -- h2(x1,y)
          -- where no variable is used more than once!
          (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x = f();
                    let x1 = x.clone();
                    let y = h(x);
                    h2(x1,y)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        fn test() -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_1_tx, x_0_1_rx) = std::sync::mpsc::channel();
                            let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel();
                            let (y_0_0_tx, y_0_0_rx) = std::sync::mpsc::channel();
                            let (x1_0_0_tx, x1_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks.push(Box::new(move || -> _ {
                                loop {
                                    let var_0 = x1_0_0_rx.recv()?;
                                    let var_1 = y_0_0_rx.recv()?;
                                    let a_0 = h2(var_0, var_1);
                                    a_0_tx.send(a_0)?
                                }
                            }));
                            tasks.push(Box::new(move || -> _ {
                                loop {
                                    let var_0 = x_0_0_0_rx.recv()?;
                                    let y_0_0 = h(var_0);
                                    y_0_0_tx.send(y_0_0)?
                                }
                            }));
                            tasks.push(Box::new(move || -> _ {
                                loop {
                                    let var_0 = x_0_1_rx.recv()?;
                                    let x1_0_0 = var_0.clone();
                                    x1_0_0_tx.send(x1_0_0)?;
                                    x_0_0_0_tx.send(var_0)?;
                                    ()
                                }
                            }));
                            tasks.push(Box::new(move || -> _ {
                                let x_0_1 = f();
                                x_0_1_tx.send(x_0_1)?
                            }));
                            run(tasks);
                            a_0_rx.recv()?
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
                                loop { let var_0 = x_0_0_rx.recv()?; let a_0 = funs::g(var_0); a_0_tx.send(a_0)? }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let var_0 = i; let x_0_0 = funs::h(var_0); x_0_0_tx.send(x_0_0)?
                                }));
                            run(tasks);
                            a_0_rx.recv()?
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
                                    let var_0 = x_0_0_rx.recv()?;
                                    let a_0 = funs::g(var_0);
                                    a_0_tx.send(a_0)?
                                  }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                  let var_0 = i;
                                  let x_0_0 = funs::h(var_0);
                                  x_0_0_tx.send(x_0_0)?
                                }));
                            run(tasks);
                            a_0_rx.recv()?
                        }
                        
                        fn test() -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                  loop {
                                    let var_0 = x_0_0_rx.recv()?;
                                    let a_0 = funs::g(var_0);
                                    a_0_tx.send(a_0)?
                                  }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                  let var_0 = 4;
                                  let x_0_0 = funs::h(var_0);
                                  x_0_0_tx.send(x_0_0)?
                                }));
                            run(tasks);  
                            a_0_rx.recv()?
                        }
                    |]
                compiled `shouldBe` expected)
        it "algo loading (globs)" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                use funs::*;

                fn algo(i: i32) -> String {
                    let x = h(i);
                    g(x)
                }

                fn test() -> String {
                    algo(4)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs::*;
                        
                        fn algo(i: i32) -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                  loop {
                                    let var_0 = x_0_0_rx.recv()?;
                                    let a_0 = g(var_0);
                                    a_0_tx.send(a_0)?
                                  }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                  let var_0 = i;
                                  let x_0_0 = h(var_0);
                                  x_0_0_tx.send(x_0_0)?
                                }));
                            run(tasks);
                            a_0_rx.recv()?
                        }
                        
                        fn test() -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                  loop {
                                    let var_0 = x_0_0_rx.recv()?;
                                    let a_0 = g(var_0);
                                    a_0_tx.send(a_0)?
                                  }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                  let var_0 = 4;
                                  let x_0_0 = h(var_0);
                                  x_0_0_tx.send(x_0_0)?
                                }));
                            run(tasks);  
                            a_0_rx.recv()?
                        }
                    |]
                compiled `shouldBe` expected)
