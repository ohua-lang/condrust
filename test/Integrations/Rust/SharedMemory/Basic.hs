{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.Basic where

import Ohua.Commons.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

spec :: Spec
spec =
    describe "Basics" $ do

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

     {-FIXME: We cannot compile simple binary operations now, because we don't propagate 
     agrument types and they are overloaded
     
        it "binary operations" $ 
          (compileCode[sourceFile|
                fn test() -> i32 {
                    let x:i32 = 0;
                    let y:i32 = 42;
                    let z:i32 = x + y;
                    let z1:i32 = z * 2;
                    z1 
                }
                |] `shouldThrow` anyException) 
-}
        it "var multi 1: read-only" $
          (showCode "Compiled: " =<< compileCode [sourceFile|
                use crate::funs::*;

                fn test() -> i32 {
                    let x:i32 = f();
                    let x1:Arc<i32> = std::sync::Arc::new(x);
                    let x2:Arc<i32> = x1.clone();
                    let y:i32 = h_Arc(x1);
                    y
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" var_multi_1
                compiled `shouldBe` expected)

        it "Struct Namespace: distinguish clones" $
          -- to check that the type system can distiguish namespaces of the clone function
          -- and to check that use_arc is found in the impl space of State, not as a pure function from 
          -- top-level definitions 
          (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x1:Arc<i32> = std::sync::Arc::new(1);
                    let x2:State = State::new(2);
                    let y1 = x1.clone();
                    let y2 = x2.clone();
                    y2.use_arc(y1)                    
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" var_multi_2 
                compiled `shouldBe` expected)

        it "env vars" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs;

                fn test(i: i32) -> String {
                    let x:i32 = funs::h(i);
                    funs::from_int(x)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" envVars
                compiled `shouldBe` expected)

        it "algo loading" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs;

                fn algo(i: i32, j:char) -> String {
                    let x:i32 = funs::h(i);
                    funs::take_char_i(j, x)
                }

                fn test() -> String {
                    let c = funs::make_char();
                    let i = funs::somefun();
                    algo(i, c)
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
                    g()
                }

                fn test() -> String {
                    let inp = some_int();
                    algo(inp)
                }
                |]) >>= 
            -- Question: I wonder why I didn't notice until now but ...Why are tasks doubled in 
                -- algo() and test()?
            (\compiled -> do
                expected <- showCode "Expected:"  algoLoadingGlobs
                compiled `shouldBe` expected)

        it "algo loading (globs) - wrongly typed" $
            compileCode  [sourceFile|
                use crate::funs::*;

                fn algo(i: i32) -> String {
                    let x:i32 = h(i);
                    g(x)
                }

                fn test() -> String {
                    let inp = some_int();
                    algo(inp)
                }
                |] `shouldThrow` anyException

        describe "tuples" $ do

          it "different targets" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test(i:i32) -> i32 {
                    let (x0 ,y0):(i32, i32) = fi_tup(i);
                    let x1:i32 = f0(x0);
                    let y1:i32 = f1(y0);
                    h4(x1,y1)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" differentTargets
                compiled `shouldBe` expected)

          it "unit fun" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> i32 {
                    let (x0,y0):(i32, i32) = fu_tup();
                    let x1:i32 = f0(x0);
                    let y1:i32 = f1(y0);
                    h4(x1,y1)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" unit_fun
                compiled `shouldBe` expected)

          it "destruct > 2" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test(i:i32) -> i32 {
                    let (a, b, c):(i32, i32, String) = f_tup(i);
                    let x:i32 = f0(a);
                    let y:i32 = f1(b);
                    let z:usize = take_string(c);
                    take_triple(x, y, z)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" destruct
                compiled `shouldBe` expected)

          it "float literal" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> bool {
                    let x = 7.4;
                    let y = 8.25;
                    take_floats(x, y)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" floats
                compiled `shouldBe` expected)

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
                            let (result_0_0_0_tx, result_0_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                            let mut tasks: Vec<Box<dyn FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks.push(Box::new(move || -> _ {
                                let result_0_0_0 = funs::hello_world();
                                result_0_0_0_tx.send(result_0_0_0)?;
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
                            match result_0_0_0_rx.recv() {
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
                        let (result_0_0_0_tx, result_0_0_0_rx) = std::sync::mpsc::channel::<  String,>();
                        let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
                        let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
                            Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let var_0 = x_0_0_0_rx.recv()?;
                                let result_0_0_0 = crate::funs::from_int(var_0);
                                result_0_0_0_tx.send(result_0_0_0)?;
                                ()
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let x_0_0_0 = crate::funs::f();
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
                        match result_0_0_0_rx.recv() {
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
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x1_0_0_1_tx, x1_0_0_1_rx) = std::sync::mpsc::channel::<  Arc,>();
  let (x1_0_0_0_0_tx, x1_0_0_0_0_rx) = std::sync::mpsc::channel::<  Arc,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x1_0_0_0_0_rx.recv()?;
        let y_0_0_0 = crate::funs::h_Arc(var_0);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x1_0_0_1_rx.recv()?;
        var_0.clone();
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
      let x_0_0_0 = crate::funs::f();
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
  match y_0_0_0_rx.recv() {
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
  let (result_0_0_0_tx, result_0_0_0_rx) =
    std::sync::mpsc::channel::<  String,>();
  let (x1_0_0_1_tx, x1_0_0_1_rx) = std::sync::mpsc::channel::<  Arc,>();
  let (x2_0_0_1_tx, x2_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (y1_0_0_0_tx, y1_0_0_0_rx) = std::sync::mpsc::channel::<  Arc,>();
  let (y2_0_0_1_tx, y2_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = y2_0_0_1_rx.recv()?;
        let var_1 = y1_0_0_0_rx.recv()?;
        let result_0_0_0 = var_0.use_arc(var_1);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x2_0_0_1_rx.recv()?;
        let y2_0_0_1 = var_0.clone();
        y2_0_0_1_tx.send(y2_0_0_1)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x1_0_0_1_rx.recv()?;
        let y1_0_0_0 = var_0.clone();
        y1_0_0_0_tx.send(y1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x2_0_0_1 = crate::funs::State::new(2);
      x2_0_0_1_tx.send(x2_0_0_1)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x1_0_0_1 = std::sync::Arc::new(1);
      x1_0_0_1_tx.send(x1_0_0_1)?;
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
  match result_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}|]

envVars :: SourceFile Span
envVars = [sourceFile|
use crate::funs;

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
  let (result_0_0_0_tx, result_0_0_0_rx) =
    std::sync::mpsc::channel::<  String,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let result_0_0_0 = crate::funs::from_int(var_0);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = crate::funs::h(i);
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
  match result_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
                    |]

algoLoading :: SourceFile Span
algoLoading = [sourceFile|
use crate::funs;

fn algo(i: i32, j: char) -> String {
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
  let (result_0_0_0_tx, result_0_0_0_rx) =
    std::sync::mpsc::channel::<  String,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_1 = x_0_0_0_rx.recv()?;
        let result_0_0_0 = crate::funs::take_char_i(j, var_1);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = crate::funs::h(i);
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
  match result_0_0_0_rx.recv() {
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
  let (result_0_0_0_tx, result_0_0_0_rx) =
    std::sync::mpsc::channel::<  String,>();
  let (i_1_0_0_tx, i_1_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (c_0_0_0_tx, c_0_0_0_rx) = std::sync::mpsc::channel::<  char,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = c_0_0_0_rx.recv()?;
        let var_1 = x_0_0_0_rx.recv()?;
        let result_0_0_0 = crate::funs::take_char_i(var_0, var_1);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_1_0_0_rx.recv()?;
        let x_0_0_0 = crate::funs::h(var_0);
        x_0_0_0_tx.send(x_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let i_1_0_0 = crate::funs::somefun();
      i_1_0_0_tx.send(i_1_0_0)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let c_0_0_0 = crate::funs::make_char();
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
  match result_0_0_0_rx.recv() {
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
  let (result_0_0_0_tx, result_0_0_0_rx) =
    std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let result_0_0_0 = crate::funs::g();
      result_0_0_0_tx.send(result_0_0_0)?;
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
  match result_0_0_0_rx.recv() {
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
  let (result_0_0_0_tx, result_0_0_0_rx) =
    std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let result_0_0_0 = crate::funs::g();
      result_0_0_0_tx.send(result_0_0_0)?;
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
  match result_0_0_0_rx.recv() {
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
  let (result_0_0_0_tx, result_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
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
        let result_0_0_0 = crate::funs::h4(var_0, var_1);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = y0_0_0_0_rx.recv()?;
        let y1_0_0_0 = crate::funs::f1(var_0);
        y1_0_0_0_tx.send(y1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x0_0_0_0_rx.recv()?;
        let x1_0_0_0 = crate::funs::f0(var_0);
        x1_0_0_0_tx.send(x1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let res = crate::funs::fi_tup(i);
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
  match result_0_0_0_rx.recv() {
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
  let (result_0_0_0_tx, result_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
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
        let result_0_0_0 = crate::funs::h4(var_0, var_1);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = y0_0_0_0_rx.recv()?;
        let y1_0_0_0 = crate::funs::f1(var_0);
        y1_0_0_0_tx.send(y1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x0_0_0_0_rx.recv()?;
        let x1_0_0_0 = crate::funs::f0(var_0);
        x1_0_0_0_tx.send(x1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let res = crate::funs::fu_tup();
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
  match result_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
                    |]

destruct :: SourceFile Span
destruct = [sourceFile|
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
  let (result_0_0_0_tx, result_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
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
        let result_0_0_0 = crate::funs::take_triple(var_0, var_1, var_2);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = c_0_0_0_rx.recv()?;
        let z_0_0_0 = crate::funs::take_string(var_0);
        z_0_0_0_tx.send(z_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = b_0_0_0_rx.recv()?;
        let y_0_0_0 = crate::funs::f1(var_0);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = a_0_0_0_rx.recv()?;
        let x_0_0_0 = crate::funs::f0(var_0);
        x_0_0_0_tx.send(x_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let res = crate::funs::f_tup(i);
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
  match result_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]

floats :: SourceFile Span
floats = [sourceFile|
use crate::funs::*;

fn test() -> bool {
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
  let (result_0_0_0_tx, result_0_0_0_rx) =
    std::sync::mpsc::channel::<  bool,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let result_0_0_0 = crate::funs::take_floats(7.4, 8.25);
      result_0_0_0_tx.send(result_0_0_0)?;
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
  match result_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]