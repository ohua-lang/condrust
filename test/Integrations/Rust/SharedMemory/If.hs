{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.If where

import Ohua.Commons.Prelude

import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

spec :: Spec
spec =
    describe "Conditionals" $ do
        it "simple condition" $ -- in most languages, a condition is not a function!
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test(i: i32) -> i32 {
                    let a: i32 = f0(i);
                    let b: i32 = f1(i);
                    let c: i32 = f2(i);
                    let d: i32 = if check(a) {
                        h(b)
                    } else {
                        h2(c)
                    };
                    h(d)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" simpleCondition
                compiled `shouldBe` expected)

      {-}
        it "context functions" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

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
                expected <- showCode "Expected:" contextFunctions
                compiled `shouldBe` expected)

        it "blocks in branches" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test(i: i32) -> i32 {
                    let a: i32 = f0(i);
                    let d: i32 = if a {
                        let x:i32 = some();
                        g0(x)
                    } else {
                        let x:i32 = other();
                        f(x)
                    };
                    h(d)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" blocks
                compiled `shouldBe` expected)

        it "IO" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use crate::funs::*;

                fn test(i: i32) -> i32 {
                    let a: i32 = f0(i);
                    let io1: IO = IO::new();
                    let io2: IO = IO::new();
                    let r :i32 = if a {
                        let x:i32 = some();
                        io1.g0(x)
                    } else {
                        let x:i32 = other();
                        io2.f(x)
                    };
                    h(r)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" io
                compiled `shouldBe` expected)

        it "loop with stateless condition" $
             (showCode "Compiled: " =<< compileCode [sourceFile|
                 fn test(i: i32) -> i32 {
                    let s:State = new(i);
                    for num in random(i) {
                      let ok: bool = random_bool();
                      let iNum:i32 = 
                        if ok {
                          somefun(ok)
                        } else {
                          otherfun(ok)
                        };
                      s.update(iNum);
                    }
                    s.get_num()
                  }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" loopWithStatelessCondition
                compiled `shouldBe` expected)

        it "ERROR: Stateful if-else in loop - We do not support IF-Else in Loops right now" $
            compileCode  [sourceFile|
                use crate::funs::*;

                fn test(i: i32) -> i32 {
                    let s:State = new();
                    for num in random() {
                      let ok: bool = random_bool();
                      let iNum:i32 = new_num();
                      if ok {
                        s.update(iNum);
                      } else {
                        s.other_update(iNum);
                      }
                      
                    }
                    s.get_num()
                }
                |]  `shouldThrow` anyException

        it "Error: only if branch in loop" $
            compileCode  [sourceFile|
                use crate::funs::*;

                fn test(i: i32) -> i32 {
                    let s:State = new();
                    for num in random() {
                      let ok: bool = random_bool();
                      if ok {
                        let iNum:i32 = new_num();
                        s.update(iNum);
                        }
                    }
                    s.get_num()
                }
                |]  `shouldThrow` anyException

-}

                
------------- Testoutput -----------------------------

simpleCondition :: SourceFile Span
simpleCondition = [sourceFile|
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
  let (e_0_0_0_tx, e_0_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (b_0_0_0_tx, b_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (ctrlTrue_0_tx, ctrlTrue_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (c_0_0_0_tx, c_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (ctrlFalse_0_tx, ctrlFalse_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (g_0_0_tx, g_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (f_0_0_tx, f_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (e_0_0_1_tx, e_0_0_1_rx) = std::sync::mpsc::channel::<  bool,>();
  let (result_1_tx, result_1_rx) = std::sync::mpsc::channel::<  i32,>();
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
            let g_0_0 = crate::funs::h2(c_0_0_0_0);
            g_0_0_tx.send(g_0_0)?;
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
            let f_0_0 = crate::funs::h(b_0_0_0_0);
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
        let var_0 = result_1_rx.recv()?;
        let result_0_0_0 = crate::funs::h(var_0);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = e_0_0_1_rx.recv()?;
        if branchSelection {
          let result = f_0_0_rx.recv()?;
          result_1_tx.send(result)?
        } else { let result = g_0_0_rx.recv()?; result_1_tx.send(result)? }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = e_0_0_0_rx.recv()?;
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
      loop {
        let var_0 = a_0_0_0_rx.recv()?;
        let res = crate::funs::check(var_0);
        e_0_0_0_tx.send(res)?;
        e_0_0_1_tx.send(res)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let c_0_0_0 = crate::funs::f2(i);
      c_0_0_0_tx.send(c_0_0_0)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let b_0_0_0 = crate::funs::f1(i);
      b_0_0_0_tx.send(b_0_0_0)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let a_0_0_0 = crate::funs::f0(i);
      a_0_0_0_tx.send(a_0_0_0)?;
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

contextFunctions :: SourceFile Span
contextFunctions = [sourceFile|
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

blocks :: SourceFile Span
blocks = [sourceFile|
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
  let (e_0_0_tx, e_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_0_0_tx, a_0_0_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (ctrlTrue_0_tx, ctrlTrue_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (ctrlFalse_0_tx, ctrlFalse_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (x_1_0_0_tx, x_1_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
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
          for _ in 0 .. count {
            let x_1_0_0 = other();
            x_1_0_0_tx.send(x_1_0_0)?;
            ()
          };
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
          for _ in 0 .. count {
            let x_0_0_0 = some();
            x_0_0_0_tx.send(x_0_0_0)?;
            ()
          };
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
        let var_0 = x_1_0_0_rx.recv()?;
        let c_0_0 = f(var_0);
        c_0_0_tx.send(c_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let b_0_0 = g0(var_0);
        b_0_0_tx.send(b_0_0)?;
        ()
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


loopWithStatelessCondition :: SourceFile Span
loopWithStatelessCondition = [sourceFile|
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
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ok_0_0_0_2_tx, ok_0_0_0_2_rx) = std::sync::mpsc::channel::<  bool,>();
  let (ok_0_0_0_3_tx, ok_0_0_0_3_rx) = std::sync::mpsc::channel::<  bool,>();
  let (ctrlTrue_0_tx, ctrlTrue_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ok_0_0_0_4_tx, ok_0_0_0_4_rx) = std::sync::mpsc::channel::<  bool,>();
  let (ctrlFalse_0_tx, ctrlFalse_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (ok_0_0_0_5_tx, ok_0_0_0_5_rx) = std::sync::mpsc::channel::<  bool,>();
  let (result_1_tx, result_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (s_0_1_1_tx, s_0_1_1_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        while !renew {
          let sig = ctrl_0_1_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let res = random_bool();
            ok_0_0_0_2_tx.send(res)?;
            ok_0_0_0_3_tx.send(res)?;
            ok_0_0_0_4_tx.send(res)?;
            ok_0_0_0_5_tx.send(res)?;
            ()
          };
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
        let ok_0_0_0_1 = ok_0_0_0_4_rx.recv()?;
        while !renew {
          let sig = ctrlFalse_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let c_0_0 = otherfun(ok_0_0_0_1);
            c_0_0_tx.send(c_0_0)?;
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
        let ok_0_0_0_0 = ok_0_0_0_3_rx.recv()?;
        while !renew {
          let sig = ctrlTrue_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let b_0_0 = somefun(ok_0_0_0_0);
            b_0_0_tx.send(b_0_0)?;
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
        let mut var_0 = s_0_1_1_rx.recv()?;
        let e_0_0 = var_0.get_num();
        e_0_0_tx.send(e_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = ok_0_0_0_5_rx.recv()?;
        if branchSelection {
          let result = b_0_0_rx.recv()?;
          result_1_tx.send(result)?
        } else { let result = c_0_0_rx.recv()?; result_1_tx.send(result)? }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = ok_0_0_0_2_rx.recv()?;
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
      let mut a_0_0 = random(i);
      let hasSize =
        {
          let tmp_has_size = a_0_0.iter().size_hint();
          tmp_has_size.1.is_some()
        };
      Ok(if hasSize {
        let size = a_0_0.len();
        let ctrl = (true, size);
        ctrl_0_0_tx.send(ctrl)?;
        let ctrl = (true, size);
        ctrl_0_1_tx.send(ctrl)?;
        ()
      } else {
        let mut size = 0;
        for d in a_0_0 {
          let ctrl = (false, 1);
          ctrl_0_0_tx.send(ctrl)?;
          let ctrl = (false, 1);
          ctrl_0_1_tx.send(ctrl)?;
          size = size + 1;
          ()
        };
        let ctrl = (true, 0);
        ctrl_0_0_tx.send(ctrl)?;
        let ctrl = (true, 0);
        ctrl_0_1_tx.send(ctrl)?;
        ()
      })
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_1 = new(i);
      s_0_0_1_tx.send(s_0_0_1)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let mut s_0_0_1_0 = s_0_0_1_rx.recv()?;
        while !renew {
          let sig = ctrl_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = result_1_rx.recv()?;
            s_0_0_1_0.update(var_1);
            ()
          };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        };
        s_0_1_1_tx.send(s_0_0_1_0)?;
        ()
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
  match e_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]

io :: SourceFile Span
io = [sourceFile|
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
  let (d_0_0_tx, d_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_0_0_tx, a_0_0_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (io1_0_0_1_tx, io1_0_0_1_rx) = std::sync::mpsc::channel::<  IO,>();
  let (ctrlTrue_0_0_tx, ctrlTrue_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrlTrue_0_1_tx, ctrlTrue_0_1_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (io2_0_0_1_tx, io2_0_0_1_rx) = std::sync::mpsc::channel::<  IO,>();
  let (ctrlFalse_0_0_tx, ctrlFalse_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrlFalse_0_1_tx, ctrlFalse_0_1_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (x_1_0_0_tx, x_1_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
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
          let sig = ctrlFalse_0_1_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let x_1_0_0 = other();
            x_1_0_0_tx.send(x_1_0_0)?;
            ()
          };
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
          let sig = ctrlTrue_0_1_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let x_0_0_0 = some();
            x_0_0_0_tx.send(x_0_0_0)?;
            ()
          };
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
        let mut io2_0_0_1_0 = io2_0_0_1_rx.recv()?;
        while !renew {
          let sig = ctrlFalse_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = x_1_0_0_rx.recv()?;
            let c_0_0 = io2_0_0_1_0.f(var_1);
            c_0_0_tx.send(c_0_0)?;
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
        let mut io1_0_0_1_0 = io1_0_0_1_rx.recv()?;
        while !renew {
          let sig = ctrlTrue_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = x_0_0_0_rx.recv()?;
            let b_0_0 = io1_0_0_1_0.g0(var_1);
            b_0_0_tx.send(b_0_0)?;
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
        let d_0_0 = h(var_0);
        d_0_0_tx.send(d_0_0)?;
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
          ctrlTrue_0_0_tx.send(ctrlTrue)?;
          ctrlTrue_0_0_tx.send(ctrlFalse)?;
          ctrlTrue_0_1_tx.send(ctrlTrue)?;
          ctrlTrue_0_1_tx.send(ctrlFalse)?
        } else {
          let ctrlTrue = (true, 0);
          let ctrlFalse = (true, 1);
          ctrlTrue_0_0_tx.send(ctrlTrue)?;
          ctrlTrue_0_0_tx.send(ctrlFalse)?;
          ctrlTrue_0_1_tx.send(ctrlTrue)?;
          ctrlTrue_0_1_tx.send(ctrlFalse)?
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let io2_0_0_1 = IO::new();
      io2_0_0_1_tx.send(io2_0_0_1)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let io1_0_0_1 = IO::new();
      io1_0_0_1_tx.send(io1_0_0_1)?;
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
  match d_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]
