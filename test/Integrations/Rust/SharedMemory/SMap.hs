{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.SMap where

import Ohua.Prelude

import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (Span)

spec :: Spec
spec =
    describe "SMap" $ do
      {-
        -- see issue ohua-lang/ohua-core#29
        -- this computation is deleted by dead code elimination
        -- TODO in fact this whole computation is dead code and our
        --      compiler should detect that!
--        it "stream" $
--            (showCode "Compiled: " =<< compileCode  [sourceFile|
--                use crate::funs::*;
--
--                fn test() -> () {
--                    let stream = iter();
--                    for e in stream {
--                        k(e);
--                    }
--                }
--                |]) >>=
--            (\compiled -> do
--                expected <- showCode "Expected:"
--                    [sourceFile|
--                      use crate::funs::*;
--
--                      fn test() -> () {
--                        let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel();
--                        let (ctrl_0_0_tx, ctrl_0_0_rx) = std::sync::mpsc::channel();
--                        let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
--                        let (size_0_0_tx, size_0_0_rx) = std::sync::mpsc::channel();
--                        let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel();
--                        let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
--                          Vec::new();
--                        tasks
--                          .push(Box::new(move || -> _ {
--                            let stream_0_0_0 = iter();
--                            loop {
--                              let data = stream_0_0_0;
--                              let hasSize =
--                                {
--                                  let tmp_has_size = data.iter().size_hint();
--                                  tmp_has_size.1.is_some()
--                                };
--                              if hasSize {
--                                let size = data.len();
--                                size_0_0_tx.send(size)?;
--                                let ctrl = (true, size);
--                                ctrl_0_0_tx.send(ctrl)?;
--                                ()
--                              } else {
--                                let size = 0;
--                                for d in data {
--                                  let ctrl = (false, 1);
--                                  ctrl_0_0_tx.send(ctrl)?;
--                                  size = size + 1;
--                                  ()
--                                };
--                                size_0_0_tx.send(size)?;
--                                let ctrl = (true, 0);
--                                ctrl_0_0_tx.send(ctrl)?;
--                                ()
--                              }
--                            }
--                          }));
--                        tasks
--                          .push(Box::new(move || -> _ {
--                            x_0_0_0_rx.recv()?;
--                            let x = ();
--                            b_0_0_tx.send(x)?
--                          }));
--                        tasks
--                          .push(Box::new(move || -> _ {
--                            loop {
--                              let renew = false;
--                              let lit_unit_0 = ();
--                              while !renew {
--                                let sig = ctrl_0_0_rx.recv()?;
--                                let count = sig.1;
--                                for _ in 0 .. count { let var_0 = lit_unit_0; c_0_0_tx.send(c_0_0)? };
--                                let renew_next_time = sig.0;
--                                renew = renew_next_time;
--                                ()
--                              }
--                            }
--                          }));
--                        tasks
--                          .push(Box::new(move || -> _ {
--                            loop {
--                              let num = size_0_0_rx.recv()?;
--                              let collection = Vec::new();
--                              for _ in 0 .. num {
--                                let data = c_0_0_rx.recv()?;
--                                collection.push(data)
--                              };
--                              x_0_0_0_tx.send(collection)?
--                            }
--                          }));
--                        run(tasks);
--                        b_0_0_rx.recv()?
--                      }
--                    |]
--                compiled `shouldBe` expected)-}

        it "imperative" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> State {
                    let s:State = State::new_state();
                    let stream: Iterator<S> = iter_i32();
                    for e in stream {
                        let e1: S = e; 
                        let r: i32 = h(e1);
                        s.gs(r);
                    }
                    s
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" imperative
                compiled `shouldBe` expected)
        it "env var" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use crate::funs::*;

                fn test(stream: Vec<i32>) -> State {
                    let s:State = State::new_state();
                    for e in stream {
                        let e1: i32 = e;
                        let r: i32 = h(e1);
                        s.gs(r);
                    }
                    s
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" envVar
                compiled `shouldBe` expected)

        it "FAIL: Conditions and literals - UnitLit is not removed everywhere" $ -- typing of unit literals seems broken
            (showCode "Compiled: " =<<compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> i32 {
                  let mut s:State = new();
                  for num in iter_i32() {
                    let ok: bool = random_bool();
                    let iNum:i32 =
                      if ok {
                        somefun()
                      } else {
                        otherfun()
                      };
                    s.gs(iNum);
                  }
                  s.get_num()
                }
              |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" loopEnvVar
                compiled `shouldBe` expected)
 {-      it "imperative while " $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn test() -> State {
                    let state:State = State::new_state();
                    let mut i:i32 = I32::new(1);
                    while islowerthan23(i) {
                        state.gs(i);
                        // i = i + 1 is actually a stateful function acting on a named memory slot
                        i.add(1); 
                    }
                    s
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
 use crate::funs::*;
 //ToDo
                    |]
                compiled `shouldBe` expected)-}


------------ Testoutput ------------------------
imperative :: SourceFile Span
imperative =  [sourceFile|
use crate::funs::*;

fn test() -> State {
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
  let (s_0_1_0_tx, s_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_0_tx, d_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (r_0_0_0_tx, r_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = d_0_rx.recv()?;
        let r_0_0_0 = h(var_0);
        r_0_0_0_tx.send(r_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let mut stream_0_0_0 = iter_i32();
      let hasSize =
        {
          let tmp_has_size = stream_0_0_0.iter().size_hint();
          tmp_has_size.1.is_some()
        };
      Ok(if hasSize {
        let size = stream_0_0_0.len();
        let ctrl = (true, size);
        ctrl_0_0_tx.send(ctrl)?;
        for d in stream_0_0_0 { d_0_tx.send(d)?; () }
      } else {
        let mut size = 0;
        for d in stream_0_0_0 {
          d_0_tx.send(d)?;
          let ctrl = (false, 1);
          ctrl_0_0_tx.send(ctrl)?;
          size = size + 1;
          ()
        };
        let ctrl = (true, 0);
        ctrl_0_0_tx.send(ctrl)?;
        ()
      })
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_1 = State::new_state();
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
            let var_1 = r_0_0_0_rx.recv()?;
            s_0_0_1_0.gs(var_1);
            ()
          };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        };
        s_0_1_0_tx.send(s_0_0_1_0)?;
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
  match s_0_1_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
                    |]

envVar :: SourceFile Span
envVar = [sourceFile|
use crate::funs::*;

fn test(stream: Vec<  i32,>) -> State {
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
  let (s_0_1_0_tx, s_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_0_tx, d_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (r_0_0_0_tx, r_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let hasSize =
        {
          let tmp_has_size = stream.iter().size_hint();
          tmp_has_size.1.is_some()
        };
      Ok(if hasSize {
        let size = stream.len();
        let ctrl = (true, size);
        ctrl_0_0_tx.send(ctrl)?;
        for d in stream { d_0_tx.send(d)?; () }
      } else {
        let mut size = 0;
        for d in stream {
          d_0_tx.send(d)?;
          let ctrl = (false, 1);
          ctrl_0_0_tx.send(ctrl)?;
          size = size + 1;
          ()
        };
        let ctrl = (true, 0);
        ctrl_0_0_tx.send(ctrl)?;
        ()
      })
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = d_0_rx.recv()?;
        let r_0_0_0 = h(var_0);
        r_0_0_0_tx.send(r_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_1 = State::new_state();
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
            let var_1 = r_0_0_0_rx.recv()?;
            s_0_0_1_0.gs(var_1);
            ()
          };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        };
        s_0_1_0_tx.send(s_0_0_1_0)?;
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
  match s_0_1_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]

loopEnvVar :: SourceFile Span
loopEnvVar =  [sourceFile|
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
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrl_0_2_tx, ctrl_0_2_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrl_0_3_tx, ctrl_0_3_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ok_0_0_0_0_tx, ok_0_0_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (lit_unit_0_3_tx, lit_unit_0_3_rx) = std::sync::mpsc::channel::<  (),>();
  let (ctrlTrue_0_tx, ctrlTrue_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (lit_unit_0_4_tx, lit_unit_0_4_rx) = std::sync::mpsc::channel::<  (),>();
  let (ctrlFalse_0_tx, ctrlFalse_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (ok_0_0_0_1_tx, ok_0_0_0_1_rx) = std::sync::mpsc::channel::<  bool,>();
  let (result_1_tx, result_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (s_0_1_1_tx, s_0_1_1_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        while !renew {
          let sig = ctrl_0_3_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let mut renew = false;
            let lit_unit_0_1 = lit_unit_0_4_rx.recv()?;
            while !renew {
              let sig = ctrlFalse_0_rx.recv()?;
              let count = sig.1;
              for _ in 0 .. count {
                let c_0_0 = otherfun();
                c_0_0_tx.send(c_0_0)?;
                ()
              };
              let renew_next_time = sig.0;
              renew = renew_next_time;
              ()
            };
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
          let sig = ctrl_0_2_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let mut renew = false;
            let lit_unit_0_0 = lit_unit_0_3_rx.recv()?;
            while !renew {
              let sig = ctrlTrue_0_rx.recv()?;
              let count = sig.1;
              for _ in 0 .. count {
                let b_0_0 = somefun();
                b_0_0_tx.send(b_0_0)?;
                ()
              };
              let renew_next_time = sig.0;
              renew = renew_next_time;
              ()
            };
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
          let sig = ctrl_0_1_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let res = random_bool();
            ok_0_0_0_0_tx.send(res)?;
            ok_0_0_0_1_tx.send(res)?;
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
        let mut var_0 = s_0_1_1_rx.recv()?;
        let e_0_0 = var_0.get_num();
        e_0_0_tx.send(e_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = ok_0_0_0_1_rx.recv()?;
        if branchSelection {
          let result = b_0_0_rx.recv()?;
          result_1_tx.send(result)?
        } else { let result = c_0_0_rx.recv()?; result_1_tx.send(result)? }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let branchSelection = ok_0_0_0_0_rx.recv()?;
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
      let mut a_0_0 = iter_i32();
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
        let ctrl = (true, size);
        ctrl_0_2_tx.send(ctrl)?;
        let ctrl = (true, size);
        ctrl_0_3_tx.send(ctrl)?;
        ()
      } else {
        let mut size = 0;
        for d in a_0_0 {
          let ctrl = (false, 1);
          ctrl_0_0_tx.send(ctrl)?;
          let ctrl = (false, 1);
          ctrl_0_1_tx.send(ctrl)?;
          let ctrl = (false, 1);
          ctrl_0_2_tx.send(ctrl)?;
          let ctrl = (false, 1);
          ctrl_0_3_tx.send(ctrl)?;
          size = size + 1;
          ()
        };
        let ctrl = (true, 0);
        ctrl_0_0_tx.send(ctrl)?;
        let ctrl = (true, 0);
        ctrl_0_1_tx.send(ctrl)?;
        let ctrl = (true, 0);
        ctrl_0_2_tx.send(ctrl)?;
        let ctrl = (true, 0);
        ctrl_0_3_tx.send(ctrl)?;
        ()
      })
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_1 = new();
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
            s_0_0_1_0.gs(var_1);
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