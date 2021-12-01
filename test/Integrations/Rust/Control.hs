{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.Control where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils


spec :: Spec
spec =
    describe "Control" $
    describe "var reuse" $ do
        -- FIXME see issue sertel/ohua-core#16
        it "simple" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one:i32) -> i32 {
                    // FIXME(feliix42): This is very unfortunate! Normally there'd be no cloning necessary here...
                    let o2 = one.clone();
                    let i = h(o2);
                    let j = h(one);
                    let k = h2(i, j);
                    let k2 = k.clone();
                    if check(k2) {
                        rec(k)
                    } else {
                        k
                    }
                }

                fn test() -> i32 {
                    rec(2)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
use funs::*;

fn rec(one: i32) -> i32 {
  let o2 = one.clone();
  let i = h(o2);
  let j = h(one);
  let k = h2(i, j);
  let k2 = k.clone();
  if check(k2) { rec(k) } else { k }
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
  let (k_0_0_0_0_tx, k_0_0_0_0_rx) = std::sync::mpsc::channel();
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
  let (one_0_0_1_tx, one_0_0_1_rx) = std::sync::mpsc::channel::<  S,>();
  let (o2_0_0_0_tx, o2_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (one_0_0_0_0_tx, one_0_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (j_0_0_0_tx, j_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (k_0_0_1_tx, k_0_0_1_rx) = std::sync::mpsc::channel::<  S,>();
  let (k2_0_0_0_tx, k2_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = k_0_0_1_rx.recv()?;
        let k2_0_0_0 = var_0.clone();
        k2_0_0_0_tx.send(k2_0_0_0)?;
        k_0_0_0_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = one_0_0_0_0_rx.recv()?;
        let j_0_0_0 = h(var_0);
        j_0_0_0_tx.send(j_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_0_0_0_rx.recv()?;
        let var_1 = j_0_0_0_rx.recv()?;
        let k_0_0_1 = h2(var_0, var_1);
        k_0_0_1_tx.send(k_0_0_1)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = k2_0_0_0_rx.recv()?;
        let a_0_0 = check(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = o2_0_0_0_rx.recv()?;
        let i_0_0_0 = h(var_0);
        i_0_0_0_tx.send(i_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = one_0_0_1_rx.recv()?;
        let o2_0_0_0 = var_0.clone();
        o2_0_0_0_tx.send(o2_0_0_0)?;
        one_0_0_0_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      one_0_0_1_tx.send(2)?;
      while a_0_0_rx.recv()? {
        let loop_res_0 = k_0_0_0_0_rx.recv()?;
        one_0_0_1_tx.send(loop_res_0)?;
        ()
      };
      let finalResult = k_0_0_0_0_rx.recv()?;
      Ok(c_0_0_tx.send(finalResult)?)
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
  match c_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
                    |]
                compiled `shouldBe` expected)
        it "dep" $
          -- fusion is more tricky here because `one` and `two` are used by data dependent calls!
          (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one:i32, two:i32) -> i32 {
                    let i = h(one);
                    let i1 = i.clone();
                    let k = h2(two, i);
                    let k1 = k.clone();
                    if check(k) {
                        rec(k1,i1)
                    } else {
                        k1
                    }
                }

                fn test() -> i32 {
                    rec(2,4)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
use funs::*;

fn rec(one: i32, two: i32) -> i32 {
  let i = h(one);
  let i1 = i.clone();
  let k = h2(two, i);
  let k1 = k.clone();
  if check(k) { rec(k1, i1) } else { k1 }
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
  let (k1_0_0_0_tx, k1_0_0_0_rx) = std::sync::mpsc::channel();
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
  let (i1_0_0_0_tx, i1_0_0_0_rx) = std::sync::mpsc::channel();
  let (one_0_0_0_tx, one_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i_0_0_1_tx, i_0_0_1_rx) = std::sync::mpsc::channel::<  S,>();
  let (i_0_0_0_0_tx, i_0_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (two_0_0_0_tx, two_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (k_0_0_1_tx, k_0_0_1_rx) = std::sync::mpsc::channel::<  S,>();
  let (k_0_0_0_0_tx, k_0_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = k_0_0_0_0_rx.recv()?;
        let a_0_0 = check(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = one_0_0_0_rx.recv()?;
        let i_0_0_1 = h(var_0);
        i_0_0_1_tx.send(i_0_0_1)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      one_0_0_0_tx.send(2)?;
      two_0_0_0_tx.send(4)?;
      while a_0_0_rx.recv()? {
        let loop_res_0 = k1_0_0_0_rx.recv()?;
        let loop_res_1 = i1_0_0_0_rx.recv()?;
        one_0_0_0_tx.send(loop_res_0)?;
        two_0_0_0_tx.send(loop_res_1)?;
        ()
      };
      i1_0_0_0_rx.recv()?;
      let finalResult = k1_0_0_0_rx.recv()?;
      Ok(c_0_0_tx.send(finalResult)?)
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = k_0_0_1_rx.recv()?;
        let k1_0_0_0 = var_0.clone();
        k1_0_0_0_tx.send(k1_0_0_0)?;
        k_0_0_0_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = two_0_0_0_rx.recv()?;
        let var_1 = i_0_0_0_0_rx.recv()?;
        let k_0_0_1 = h2(var_0, var_1);
        k_0_0_1_tx.send(k_0_0_1)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = i_0_0_1_rx.recv()?;
        let i1_0_0_0 = var_0.clone();
        i1_0_0_0_tx.send(i1_0_0_0)?;
        i_0_0_0_0_tx.send(var_0)?
      }
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
  match c_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
                    |]
                compiled `shouldBe` expected)
