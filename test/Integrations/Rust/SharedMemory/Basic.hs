{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.Basic where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

spec :: Spec
spec =
    describe "Basics" $ do
{-
        it "a function" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use funs::hello_world;

                fn test() -> String {
                    hello_world()
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" aFunction
                compiled `shouldBe` expected)

        it "simple composition" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x: i32 = f();
                    from_int(x)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" simpleComposition                    
                compiled `shouldBe` expected)
-- FIXME see issue ohua-lang/ohua-frontend#8
--        it "var multi fail" $
--            -- enforce Arc construction via type-check
--            (showCode "Compiled: " =<< compileCode  [sourceFile|
--                use crate::funs::*;
--
--                fn test() -> String {
--                    let x = f();
--                    let y = h(x);
--                    h2(x,y)
--                }
--                |]) `shouldThrow` anyErrorCall
        it "binary operations" $ 
          (showCode "Compiled: " =<< compileCode[sourceFile|
                fn test() -> i32 {
                    let x:i32 = 0;
                    let y:i32 = 42;
                    let z:i32 = x + y;
                    let z1:i32 = z * 2;
                    z1 
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" binaryOperation
                compiled `shouldBe` expected)
 -}               
        it "var multi 1: read-only" $
          -- due to the transformation to state threads, this transforms into:
          -- let x = f();
          -- let x1 = Arc::new();
          -- let (x2,x1') = x1.arc_clone();
          -- let y = h(x1');
          -- h2(x2,y)
          -- where no variable is used more than once!
          -- FIXME(feliix42): At some point we'll have to adjust the data types of the functions used here in `Util.hs` because they currently do not make sense as they don't account for the `Arc`
          (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x:i32 = f();
                    let x1:Arc<i32> = std::sync::Arc::new(x);
                    let x2:Arc<i32> = x1.clone();
                    let y:i32 = h_Arc(x1);
                    notInLib(x2,y)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" var_multi_1
                compiled `shouldBe` expected)
{-
        it "var multi 2: explicit clone" $
          -- due to the transformation to state threads, this transforms into:
          -- let x = f();
          -- let (x1,x') = x.clone();
          -- let y = h(x');
          -- h2(x1,y)
          -- where no variable is used more than once!
          (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x:i32 = f();
                    let x1:i32 = x.clone();
                    let y:i32 = h(x);
                    h2(x1,y)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" var_multi_2 
                compiled `shouldBe` expected)

        it "env vars" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use funs;

                fn test(i: i32) -> String {
                    let x:i32 = funs::h(i);
                    funs::g(x)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" envVars
                compiled `shouldBe` expected)

        it "algo loading" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use funs;

                fn algo(i: i32, j:char) -> String {
                    let x:i32 = funs::h(i);
                    funs::take_char_i(j, x)
                }

                fn test() -> String {
                    algo(4)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" algoLoading 
                compiled `shouldBe` expected)

        it "algo loading (globs)" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn algo(i: i32) -> String {
                    let x:i32 = h(i);
                    g(x)
                }

                fn test() -> String {
                    algo(4)
                }
                |]) >>= 
            -- Question: I wonder why I didn't notice until now but ...Why are tasks doubled in 
                -- algo() and test()?
            (\compiled -> do
                expected <- showCode "Expected:"  algoLoadingGlobs
                compiled `shouldBe` expected)


        describe "tuples" $ do

          it "different targets" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test(i:i32) -> i32 {
                    let (x0 ,y0):(i32, i32) = fi_tup(i);
                    let x1:i32 = f0(x0);
                    let y1:i32 = f1(y0);
                    h2(x1,y1)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" differentTargets
                compiled `shouldBe` expected)

          it "unit fun" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> i32 {
                    let (x0,y0):(i32, i32) = f_tup();
                    let x1:i32 = f0(x0);
                    let y1:i32 = f1(y0);
                    h2(x1,y1)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" unit_fun
                compiled `shouldBe` expected)

          it "destruct > 2" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> i32 {
                    let (a,b, c):(i32, i32, String) = f_tup();
                    let x:i32 = f0(a);
                    let y:i32 = f1(b);
                    let z:usize = take_string(c);
                    take_triple(x, y, z)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" destruct
                compiled `shouldBe` expected)
-}
------------- Testouput -------------------------------------

aFunction :: SourceFile Span
aFunction = [sourceFile|
                        use funs::hello_world;

                        fn test() -> String {
                            #[derive(Debug)]
                            enum RunError {
                                SendFailed,
                                RecvFailed,
                            }
                            impl<T: Send> From<std::sync::mpsc::SendError<T>> for RunError {
                                fn from(_err: std::sync::mpsc::SendError<T>) -> Self {
                                    RunError::SendFailed
                                }
                            }
                            impl From<std::sync::mpsc::RecvError> for RunError {
                                fn from(_err: std::sync::mpsc::RecvError) -> Self {
                                    RunError::RecvFailed
                                }
                            }
                            let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                            let mut tasks: Vec<Box<dyn FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks.push(Box::new(move || -> _ {
                                let a_0_0 = hello_world();
                                a_0_0_tx.send(a_0_0)?;
                                Ok(())
                            }));
                            let handles: Vec<std::thread::JoinHandle<_>> = tasks
                                .into_iter()
                                .map(|t| {
                                    std::thread::spawn(move || {
                                        let _ = t();
                                    })
                                })
                                .collect();
                            for h in handles {
                                if let Err(_) = h.join() {
                                    eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                                }
                            }
                            match a_0_0_rx.recv() {
                                Ok(res) => res,
                                Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
                            }
                        }
                    |]

simpleComposition :: SourceFile Span
simpleComposition = [sourceFile|
                        use crate::funs::*;

                        fn test() -> String {
                        #[derive(Debug)]
                        enum RunError {
                            SendFailed,
                            RecvFailed,
                        }
                        impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
                            fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
                            RunError::SendFailed
                            }
                        }
                        impl From<  std::sync::mpsc::RecvError,> for RunError {
                            fn from(_err: std::sync::mpsc::RecvError) -> Self {
                            RunError::RecvFailed
                            }
                        }
                        let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                        let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
                        let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
                            Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let var_0 = x_0_0_0_rx.recv()?;
                                let a_0_0 = from_int(var_0);
                                a_0_0_tx.send(a_0_0)?;
                                ()
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let x_0_0_0 = f();
                            x_0_0_0_tx.send(x_0_0_0)?;
                            Ok(())
                            }));
                        let handles: Vec<  std::thread::JoinHandle<  _,>,> =
                            tasks
                            .into_iter()
                            .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
                            .collect();
                        for h in handles {
                            if let Err(_) = h.join() {
                            eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                            }
                        }
                        match a_0_0_rx.recv() {
                            Ok(res) => res,
                            Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
                        }
                        }
                    |]

binaryOperation:: SourceFile Span
binaryOperation= [sourceFile|
                      fn test() -> i32 {
                        #[derive(Debug)]
                        enum RunError {
                          SendFailed,
                          RecvFailed,
                        }
                        impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
                          fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
                            RunError::SendFailed
                          }
                        }
                        impl From<  std::sync::mpsc::RecvError,> for RunError {
                          fn from(_err: std::sync::mpsc::RecvError) -> Self {
                            RunError::RecvFailed
                          }
                        }
                        let (z1_0_0_0_tx, z1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
                        let (z_0_0_0_tx, z_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
                        let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
                          Vec::new();
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let var_0 = z_0_0_0_rx.recv()?;
                              let z1_0_0_0 = var_0 * 2;
                              z1_0_0_0_tx.send(z1_0_0_0)?;
                              ()
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            let z_0_0_0 = 0 + 42;
                            z_0_0_0_tx.send(z_0_0_0)?;
                            Ok(())
                          }));
                        let handles: Vec<  std::thread::JoinHandle<  _,>,> =
                          tasks
                            .into_iter()
                            .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
                            .collect();
                        for h in handles {
                          if let Err(_) = h.join() {
                            eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                          }
                        }
                        match z1_0_0_0_rx.recv() {
                          Ok(res) => res,
                          Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
                        }
                      }
                    |]

var_multi_1 :: SourceFile Span
var_multi_1 = [sourceFile|
use crate::funs::*;

fn test() -> String {
  #[derive(Debug)]
  enum RunError {
    SendFailed,
    RecvFailed,
  }
  impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
    fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
      RunError::SendFailed
    }
  }
  impl From<  std::sync::mpsc::RecvError,> for RunError {
    fn from(_err: std::sync::mpsc::RecvError) -> Self {
      RunError::RecvFailed
    }
  }
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x1_0_0_1_tx, x1_0_0_1_rx) = std::sync::mpsc::channel::<  Arc<  i32,>,>();
  let (x1_0_0_0_0_tx, x1_0_0_0_0_rx) =
    std::sync::mpsc::channel::<  Arc<  i32,>,>();
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x2_0_0_0_tx, x2_0_0_0_rx) = std::sync::mpsc::channel::<  Arc<  i32,>,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x2_0_0_0_rx.recv()?;
        let var_1 = y_0_0_0_rx.recv()?;
        let a_0_0 = h2(var_0, var_1);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x1_0_0_0_0_rx.recv()?;
        let y_0_0_0 = h(var_0);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x1_0_0_1_rx.recv()?;
        let x2_0_0_0 = var_0.clone();
        x2_0_0_0_tx.send(x2_0_0_0)?;
        x1_0_0_0_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let x1_0_0_1 = std::sync::Arc::new(var_0);
        x1_0_0_1_tx.send(x1_0_0_1)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = f();
      x_0_0_0_tx.send(x_0_0_0)?;
      Ok(())
    }));
  let handles: Vec<  std::thread::JoinHandle<  _,>,> =
    tasks
      .into_iter()
      .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
      .collect();
  for h in handles {
    if let Err(_) = h.join() {
      eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
    }
  }
  match a_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]

var_multi_2 :: SourceFile Span
var_multi_2 = [sourceFile|
 use crate::funs::*;

fn test() -> String {
  #[derive(Debug)]
  enum RunError {
    SendFailed,
    RecvFailed,
  }
  impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
    fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
      RunError::SendFailed
    }
  }
  impl From<  std::sync::mpsc::RecvError,> for RunError {
    fn from(_err: std::sync::mpsc::RecvError) -> Self {
      RunError::RecvFailed
    }
  }
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let (x_0_0_1_tx, x_0_0_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_0_tx, x_0_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x1_0_0_0_tx, x1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x1_0_0_0_rx.recv()?;
        let var_1 = y_0_0_0_rx.recv()?;
        let a_0_0 = h2(var_0, var_1);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_0_rx.recv()?;
        let y_0_0_0 = h(var_0);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_1_rx.recv()?;
        let x1_0_0_0 = var_0.clone();
        x1_0_0_0_tx.send(x1_0_0_0)?;
        x_0_0_0_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_1 = f();
      x_0_0_1_tx.send(x_0_0_1)?;
      Ok(())
    }));
  let handles: Vec<  std::thread::JoinHandle<  _,>,> =
    tasks
      .into_iter()
      .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
      .collect();
  for h in handles {
    if let Err(_) = h.join() {
      eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
    }
  }
  match a_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}|]

envVars :: SourceFile Span
envVars = [sourceFile|
                        use funs;

                        fn test(i: i32) -> String {
                            #[derive(Debug)]
                            enum RunError {
                              SendFailed,
                              RecvFailed,
                            }
                            impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
                              fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
                                RunError::SendFailed
                              }
                            }
                            impl From<  std::sync::mpsc::RecvError,> for RunError {
                              fn from(_err: std::sync::mpsc::RecvError) -> Self {
                                RunError::RecvFailed
                              }
                            }
                            let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                            let (x_0_0_0_tx, x_0_0_0_rx)  = std::sync::mpsc::channel::<i32>();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                    loop {
                                        let var_0 = x_0_0_0_rx.recv()?;
                                        let a_0_0 = funs::g(var_0);
                                        a_0_0_tx.send(a_0_0)?;
                                        ()
                                    }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                    let x_0_0_0 = funs::h(i);
                                    x_0_0_0_tx.send(x_0_0_0)?;
                                    Ok(())
                                }));
                            let handles: Vec<  std::thread::JoinHandle<  _,>,> =
                                tasks
                                    .into_iter()
                                    .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
                                    .collect();
                            for h in handles {
                                if let Err(_) = h.join() {
                                    eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                                }
                            }
                            match a_0_0_rx.recv() {
                                Ok(res) => res,
                                Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
                            }
                        }
                    |]

algoLoading :: SourceFile Span
algoLoading = [sourceFile|
                      use funs;

                    fn algo(i: i32) -> String {
                    #[derive(Debug)]
                    enum RunError {
                        SendFailed,
                        RecvFailed,
                    }
                    impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
                        fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
                        RunError::SendFailed
                        }
                    }
                    impl From<  std::sync::mpsc::RecvError,> for RunError {
                        fn from(_err: std::sync::mpsc::RecvError) -> Self {
                        RunError::RecvFailed
                        }
                    }
                    let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                    let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
                    let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
                        Vec::new();
                    tasks
                        .push(Box::new(move || -> _ {
                        loop {
                            let var_0 = x_0_0_0_rx.recv()?;
                            let a_0_0 = funs::g(var_0);
                            a_0_0_tx.send(a_0_0)?;
                            ()
                        }
                        }));
                    tasks
                        .push(Box::new(move || -> _ {
                        let x_0_0_0 = funs::h(i);
                        x_0_0_0_tx.send(x_0_0_0)?;
                        Ok(())
                        }));
                    let handles: Vec<  std::thread::JoinHandle<  _,>,> =
                        tasks
                        .into_iter()
                        .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
                        .collect();
                    for h in handles {
                        if let Err(_) = h.join() {
                        eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                        }
                    }
                    match a_0_0_rx.recv() {
                        Ok(res) => res,
                        Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
                    }
                    }

                    fn test() -> String {
                    #[derive(Debug)]
                    enum RunError {
                        SendFailed,
                        RecvFailed,
                    }
                    impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
                        fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
                        RunError::SendFailed
                        }
                    }
                    impl From<  std::sync::mpsc::RecvError,> for RunError {
                        fn from(_err: std::sync::mpsc::RecvError) -> Self {
                        RunError::RecvFailed
                        }
                    }
                    let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                    let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
                    let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
                        Vec::new();
                    tasks
                        .push(Box::new(move || -> _ {
                        loop {
                            let var_0 = x_0_0_0_rx.recv()?;
                            let a_0_0 = funs::g(var_0);
                            a_0_0_tx.send(a_0_0)?;
                            ()
                        }
                        }));
                    tasks
                        .push(Box::new(move || -> _ {
                        let x_0_0_0 = funs::h(4);
                        x_0_0_0_tx.send(x_0_0_0)?;
                        Ok(())
                        }));
                    let handles: Vec<  std::thread::JoinHandle<  _,>,> =
                        tasks
                        .into_iter()
                        .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
                        .collect();
                    for h in handles {
                        if let Err(_) = h.join() {
                        eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                        }
                    }
                    match a_0_0_rx.recv() {
                        Ok(res) => res,
                        Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
                    }
                    }
                    |]

algoLoadingGlobs :: SourceFile Span
algoLoadingGlobs = 
                    [sourceFile|
                        use crate::funs::*;

                        fn algo(i: i32) -> String {
                        #[derive(Debug)]
                        enum RunError {
                            SendFailed,
                            RecvFailed,
                        }
                        impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
                            fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
                            RunError::SendFailed
                            }
                        }
                        impl From<  std::sync::mpsc::RecvError,> for RunError {
                            fn from(_err: std::sync::mpsc::RecvError) -> Self {
                            RunError::RecvFailed
                            }
                        }
                        let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                        let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
                        let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
                            Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let var_0 = x_0_0_0_rx.recv()?;
                                let a_0_0 = g(var_0);
                                a_0_0_tx.send(a_0_0)?;
                                ()
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let x_0_0_0 = h(i);
                            x_0_0_0_tx.send(x_0_0_0)?;
                            Ok(())
                            }));
                        let handles: Vec<  std::thread::JoinHandle<  _,>,> =
                            tasks
                            .into_iter()
                            .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
                            .collect();
                        for h in handles {
                            if let Err(_) = h.join() {
                            eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                            }
                        }
                        match a_0_0_rx.recv() {
                            Ok(res) => res,
                            Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
                        }
                        }

                        fn test() -> String {
                        #[derive(Debug)]
                        enum RunError {
                            SendFailed,
                            RecvFailed,
                        }
                        impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
                            fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
                            RunError::SendFailed
                            }
                        }
                        impl From<  std::sync::mpsc::RecvError,> for RunError {
                            fn from(_err: std::sync::mpsc::RecvError) -> Self {
                            RunError::RecvFailed
                            }
                        }
                        let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                        let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
                        let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
                            Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let var_0 = x_0_0_0_rx.recv()?;
                                let a_0_0 = g(var_0);
                                a_0_0_tx.send(a_0_0)?;
                                ()
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let x_0_0_0 = h(4);
                            x_0_0_0_tx.send(x_0_0_0)?;
                            Ok(())
                            }));
                        let handles: Vec<  std::thread::JoinHandle<  _,>,> =
                            tasks
                            .into_iter()
                            .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
                            .collect();
                        for h in handles {
                            if let Err(_) = h.join() {
                            eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
                            }
                        }
                        match a_0_0_rx.recv() {
                            Ok(res) => res,
                            Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
                        }
                        }
                    |]

differentTargets :: SourceFile Span
differentTargets = [sourceFile|
use crate::funs::*;

fn test(i: i32) -> i32 {
  #[derive(Debug)]
  enum RunError {
    SendFailed,
    RecvFailed,
  }
  impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
    fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
      RunError::SendFailed
    }
  }
  impl From<  std::sync::mpsc::RecvError,> for RunError {
    fn from(_err: std::sync::mpsc::RecvError) -> Self {
      RunError::RecvFailed
    }
  }
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x0_0_0_0_tx, x0_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (y0_0_0_0_tx, y0_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (y1_0_0_0_tx, y1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x1_0_0_0_tx, x1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x1_0_0_0_rx.recv()?;
        let var_1 = y1_0_0_0_rx.recv()?;
        let b_0_0 = h2(var_0, var_1);
        b_0_0_tx.send(b_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = y0_0_0_0_rx.recv()?;
        let y1_0_0_0 = f1(var_0);
        y1_0_0_0_tx.send(y1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x0_0_0_0_rx.recv()?;
        let x1_0_0_0 = f0(var_0);
        x1_0_0_0_tx.send(x1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let res = fi_tup(i);
      let x0_0_0_0 = res.0;
      x0_0_0_0_tx.send(x0_0_0_0)?;
      let y0_0_0_0 = res.1;
      y0_0_0_0_tx.send(y0_0_0_0)?;
      Ok(())
    }));
  let handles: Vec<  std::thread::JoinHandle<  _,>,> =
    tasks
      .into_iter()
      .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
      .collect();
  for h in handles {
    if let Err(_) = h.join() {
      eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
    }
  }
  match b_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
                    |]


unit_fun :: SourceFile Span
unit_fun = [sourceFile|
use crate::funs::*;

fn test() -> i32 {
  #[derive(Debug)]
  enum RunError {
    SendFailed,
    RecvFailed,
  }
  impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
    fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
      RunError::SendFailed
    }
  }
  impl From<  std::sync::mpsc::RecvError,> for RunError {
    fn from(_err: std::sync::mpsc::RecvError) -> Self {
      RunError::RecvFailed
    }
  }
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x0_0_0_0_tx, x0_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (y0_0_0_0_tx, y0_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (y1_0_0_0_tx, y1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x1_0_0_0_tx, x1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x1_0_0_0_rx.recv()?;
        let var_1 = y1_0_0_0_rx.recv()?;
        let b_0_0 = h2(var_0, var_1);
        b_0_0_tx.send(b_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = y0_0_0_0_rx.recv()?;
        let y1_0_0_0 = f1(var_0);
        y1_0_0_0_tx.send(y1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x0_0_0_0_rx.recv()?;
        let x1_0_0_0 = f0(var_0);
        x1_0_0_0_tx.send(x1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let res = f_tup();
      let x0_0_0_0 = res.0;
      x0_0_0_0_tx.send(x0_0_0_0)?;
      let y0_0_0_0 = res.1;
      y0_0_0_0_tx.send(y0_0_0_0)?;
      Ok(())
    }));
  let handles: Vec<  std::thread::JoinHandle<  _,>,> =
    tasks
      .into_iter()
      .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
      .collect();
  for h in handles {
    if let Err(_) = h.join() {
      eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
    }
  }
  match b_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
                    |]

destruct :: SourceFile Span
destruct = [sourceFile|
use crate::funs::*;

fn test() -> i32 {
  #[derive(Debug)]
  enum RunError {
    SendFailed,
    RecvFailed,
  }
  impl<  T: Send,> From<  std::sync::mpsc::SendError<  T,>,> for RunError {
    fn from(_err: std::sync::mpsc::SendError<  T,>) -> Self {
      RunError::SendFailed
    }
  }
  impl From<  std::sync::mpsc::RecvError,> for RunError {
    fn from(_err: std::sync::mpsc::RecvError) -> Self {
      RunError::RecvFailed
    }
  }
  let (e_0_0_tx, e_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_0_tx, a_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (b_0_0_0_tx, b_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (c_0_0_0_tx, c_0_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let (z_0_0_0_tx, z_0_0_0_rx) = std::sync::mpsc::channel::<  usize,>();
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let var_1 = y_0_0_0_rx.recv()?;
        let var_2 = z_0_0_0_rx.recv()?;
        let e_0_0 = take_triple(var_0, var_1, var_2);
        e_0_0_tx.send(e_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = c_0_0_0_rx.recv()?;
        let z_0_0_0 = take_string(var_0);
        z_0_0_0_tx.send(z_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = b_0_0_0_rx.recv()?;
        let y_0_0_0 = f1(var_0);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = a_0_0_0_rx.recv()?;
        let x_0_0_0 = f0(var_0);
        x_0_0_0_tx.send(x_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let res = f_tup();
      let a_0_0_0 = res.0;
      a_0_0_0_tx.send(a_0_0_0)?;
      let b_0_0_0 = res.1;
      b_0_0_0_tx.send(b_0_0_0)?;
      let c_0_0_0 = res.2;
      c_0_0_0_tx.send(c_0_0_0)?;
      Ok(())
    }));
  let handles: Vec<  std::thread::JoinHandle<  _,>,> =
    tasks
      .into_iter()
      .map(|t| { std::thread::spawn(move || { let _ = t(); }) })
      .collect();
  for h in handles {
    if let Err(_) = h.join() {
      eprintln!("[Error] A worker thread of an Ohua algorithm has panicked!");
    }
  }
  match e_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]