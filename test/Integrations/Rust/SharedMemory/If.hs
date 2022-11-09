{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.If where

import Ohua.Prelude

import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

spec :: Spec
spec =
    describe "Conditionals" $ do
        it "simple condition" $ -- in most languages, a condition is not a function!
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use funs::*;

                fn test(i: i32) -> i32 {
                    let a: i32 = f0(i);
                    let b: i32 = f1(i);
                    let c: i32 = f2(i);
                    let d: i32 = if a {
                        g0(b)
                    } else {
                        g1(c)
                    };
                    h(d)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" simple_condition
                compiled `shouldBe` expected)
        it "context functions" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use funs::*;

                fn test(i: i32) -> i32 {
                    let a: i32 = f0(i);
                    let d: i32 = if a {
                        g0(5)
                    } else {
                        f()
                    };
                    h(d)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" context_functions
                compiled `shouldBe` expected)

------------- Testoutput -----------------------------

simple_condition :: SourceFile Span
simple_condition = [sourceFile|
use funs::*;

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
  let (g_0_0_tx, g_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_0_0_tx, a_0_0_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (b_0_0_0_tx, b_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (ctrlTrue_0_tx, ctrlTrue_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (c_0_0_0_tx, c_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (ctrlFalse_0_tx, ctrlFalse_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (f_0_0_tx, f_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (e_0_0_tx, e_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_0_1_tx, a_0_0_0_1_rx) = std::sync::mpsc::channel::<  bool,>();
  let (result_0_tx, result_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let c_0_0_0_0 = c_0_0_0_rx.recv()?;
        while !renew {
          let sig = ctrlFalse_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let f_0_0 = g1(c_0_0_0_0);
            f_0_0_tx.send(f_0_0)?;
            ()
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
        let mut renew = false;
        let b_0_0_0_0 = b_0_0_0_rx.recv()?;
        while !renew {
          let sig = ctrlTrue_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let e_0_0 = g0(b_0_0_0_0);
            e_0_0_tx.send(e_0_0)?;
            ()
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
        let var_0 = result_0_rx.recv()?;
        let g_0_0 = h(var_0);
        g_0_0_tx.send(g_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = a_0_0_0_1_rx.recv()?;
        if branchSelection {
          let result = e_0_0_rx.recv()?;
          result_0_tx.send(result)?
        } else { let result = f_0_0_rx.recv()?; result_0_tx.send(result)? }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = a_0_0_0_0_rx.recv()?;
        if branchSelection {
          let ctrlTrue = (true, 1);
          let ctrlFalse = (true, 0);
          ctrlTrue_0_tx.send(ctrlTrue)?;
          ctrlFalse_0_tx.send(ctrlFalse)?
        } else {
          let ctrlTrue = (true, 0);
          let ctrlFalse = (true, 1);
          ctrlTrue_0_tx.send(ctrlTrue)?;
          ctrlFalse_0_tx.send(ctrlFalse)?
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let c_0_0_0 = f2(i);
      c_0_0_0_tx.send(c_0_0_0)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let b_0_0_0 = f1(i);
      b_0_0_0_tx.send(b_0_0_0)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let res = f0(i);
      a_0_0_0_0_tx.send(res)?;
      a_0_0_0_1_tx.send(res)?;
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
  match g_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}|]

context_functions :: SourceFile Span
context_functions = [sourceFile|
use funs::*;

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
  let (e_0_0_tx, e_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_0_0_tx, a_0_0_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (ctrlTrue_0_tx, ctrlTrue_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrlFalse_0_tx, ctrlFalse_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_0_1_tx, a_0_0_0_1_rx) = std::sync::mpsc::channel::<  bool,>();
  let (result_0_tx, result_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        while !renew {
          let sig = ctrlFalse_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count { let c_0_0 = f(); c_0_0_tx.send(c_0_0)?; () };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        while !renew {
          let sig = ctrlTrue_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count { let b_0_0 = g0(5); b_0_0_tx.send(b_0_0)?; () };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = result_0_rx.recv()?;
        let e_0_0 = h(var_0);
        e_0_0_tx.send(e_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = a_0_0_0_1_rx.recv()?;
        if branchSelection {
          let result = b_0_0_rx.recv()?;
          result_0_tx.send(result)?
        } else { let result = c_0_0_rx.recv()?; result_0_tx.send(result)? }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = a_0_0_0_0_rx.recv()?;
        if branchSelection {
          let ctrlTrue = (true, 1);
          let ctrlFalse = (true, 0);
          ctrlTrue_0_tx.send(ctrlTrue)?;
          ctrlFalse_0_tx.send(ctrlFalse)?
        } else {
          let ctrlTrue = (true, 0);
          let ctrlFalse = (true, 1);
          ctrlTrue_0_tx.send(ctrlTrue)?;
          ctrlFalse_0_tx.send(ctrlFalse)?
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let res = f0(i);
      a_0_0_0_0_tx.send(res)?;
      a_0_0_0_1_tx.send(res)?;
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
