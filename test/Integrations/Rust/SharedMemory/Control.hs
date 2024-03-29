{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.Control where

import Ohua.Commons.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

spec :: Spec
spec =
    describe "Control" $
    describe "var reuse" $ do
        -- FIXME see issue sertel/ohua-core#16
        it "simple" $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn rec(one:i32) -> i32 {
                    // FIXME(feliix42): This is very unfortunate! Normally there'd be no cloning necessary here...
                    // No it wouldn't. The Problem is, that a) we cannot distinguish if is necessary or not and 
                    // b) that Ohua for whatever reason ony produces one channel and moves it's receiver multiple times (which is invalid)
                    // when we reuse variables
                    let (o2, oneN) = fi_tup(one);
                    let i: i32 = h(o2);
                    let j: i32 = h(oneN);
                    let k: i32 = h4(i, j);
                    let (k1, k2) = fi_tup(k);
                    if check(k2) {
                        rec(k1)
                    } else {
                        k1
                    }
                }

                fn test() -> i32 {
                    rec(2)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" simple 
                compiled `shouldBe` expected)

        it "dependent" $
          -- fusion is more tricky here because `one` and `two` are used by data dependent calls!
          (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn rec(one:i32, two:i32) -> i32 {
                    let i: i32 = h(one);
                    let (i1, i2) = fi_tup(i);
                    let k: i32 = h4(two, i);
                    let (k1, k2) = fi_tup(k);
                    if check(k) {
                        rec(k1,k2)
                    } else {
                        k1
                    }
                }

                fn test() -> i32 {
                    rec(2,4)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" dependent
                compiled `shouldBe` expected)

-------------- Testoutput -------------------------------
simple :: SourceFile Span
simple = [sourceFile|
use crate::funs::*;

fn rec(one: i32) -> i32 {
  let (o2, oneN) = fi_tup(one);
  let i: i32 = h(o2);
  let j: i32 = h(oneN);
  let k: i32 = h4(i, j);
  let (k1, k2) = fi_tup(k);
  if check(k2) { rec(k1) } else { k1 }
}

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
  let (k1_0_0_0_tx, k1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (one_0_0_0_tx, one_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (o2_0_0_0_tx, o2_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (oneN_0_0_0_tx, oneN_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (j_0_0_0_tx, j_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (k2_0_0_0_tx, k2_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = k2_0_0_0_rx.recv()?;
        let c_0_0 = crate::funs::check(var_0);
        c_0_0_tx.send(c_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = k_0_0_0_rx.recv()?;
        let res = crate::funs::fi_tup(var_0);
        let k1_0_0_0 = res.0;
        k1_0_0_0_tx.send(k1_0_0_0)?;
        let k2_0_0_0 = res.1;
        k2_0_0_0_tx.send(k2_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_0_0_0_rx.recv()?;
        let var_1 = j_0_0_0_rx.recv()?;
        let k_0_0_0 = crate::funs::h4(var_0, var_1);
        k_0_0_0_tx.send(k_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = oneN_0_0_0_rx.recv()?;
        let j_0_0_0 = crate::funs::h(var_0);
        j_0_0_0_tx.send(j_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = o2_0_0_0_rx.recv()?;
        let i_0_0_0 = crate::funs::h(var_0);
        i_0_0_0_tx.send(i_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = one_0_0_0_rx.recv()?;
        let res = crate::funs::fi_tup(var_0);
        let o2_0_0_0 = res.0;
        o2_0_0_0_tx.send(o2_0_0_0)?;
        let oneN_0_0_0 = res.1;
        oneN_0_0_0_tx.send(oneN_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      one_0_0_0_tx.send(2)?;
      while c_0_0_rx.recv()? {
        let loop_res_0 = k1_0_0_0_rx.recv()?;
        one_0_0_0_tx.send(loop_res_0)?;
        ()
      };
      let finalResult = k1_0_0_0_rx.recv()?;
      Ok(result_0_0_0_tx.send(finalResult)?)
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
} |]

dependent :: SourceFile Span
dependent =  [sourceFile|
use crate::funs::*;

fn rec(one: i32, two: i32) -> i32 {
  let i: i32 = h(one);
  let (i1, i2) = fi_tup(i);
  let k: i32 = h4(two, i);
  let (k1, k2) = fi_tup(k);
  if check(k) { rec(k1, k2) } else { k1 }
}

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
  let (k1_0_0_0_tx, k1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (k2_0_0_0_tx, k2_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (one_0_0_0_tx, one_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (two_0_0_0_tx, two_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = k_0_0_0_rx.recv()?;
        let c_0_0 = crate::funs::check(var_0);
        c_0_0_tx.send(c_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = k_0_0_0_rx.recv()?;
        let res = crate::funs::fi_tup(var_0);
        let k1_0_0_0 = res.0;
        k1_0_0_0_tx.send(k1_0_0_0)?;
        let k2_0_0_0 = res.1;
        k2_0_0_0_tx.send(k2_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = two_0_0_0_rx.recv()?;
        let var_1 = i_0_0_0_rx.recv()?;
        let k_0_0_0 = crate::funs::h4(var_0, var_1);
        k_0_0_0_tx.send(k_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = one_0_0_0_rx.recv()?;
        let i_0_0_0 = crate::funs::h(var_0);
        i_0_0_0_tx.send(i_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      one_0_0_0_tx.send(2)?;
      two_0_0_0_tx.send(4)?;
      while c_0_0_rx.recv()? {
        let loop_res_0 = k1_0_0_0_rx.recv()?;
        let loop_res_1 = k2_0_0_0_rx.recv()?;
        one_0_0_0_tx.send(loop_res_0)?;
        two_0_0_0_tx.send(loop_res_1)?;
        ()
      };
      k2_0_0_0_rx.recv()?;
      let finalResult = k1_0_0_0_rx.recv()?;
      Ok(result_0_0_0_tx.send(finalResult)?)
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
} |]
