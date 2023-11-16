{-# LANGUAGE QuasiQuotes #-}

module Integrations.Rust.SharedMemory.State where

import Ohua.Prelude  ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)


spec :: Spec
spec =
  describe "State" $ do
    describe "flat" ( do
      it "simple" $
        (showCode "Compiled: " =<< compileCode  [sourceFile|
          use crate::funs::*;

          fn test(i: i32) -> i32 {
            let state: State = State::new_state(i);
            let result: i32 = state.gs(5);
            h(result)
          }
          |]) >>=
        (\compiled -> do
          expected <- showCode "Expected:" simple
          compiled `shouldBe` expected)

      it "thread" $
        (showCode "Compiled: " =<< compileCode  [sourceFile|
          use crate::funs::*;

          fn test(i: i32) -> i32 {
            let state: State  = State::new_state(i);
            state.modify(5);
            let r1:i32 = state.gs(6);
            r1
          }
          |]) >>=
        (\compiled -> do
          expected <- showCode "Expected:" thread
          compiled `shouldBe` expected)

      it "single state" $
        (showCode "Compiled: " =<< compileCode  [sourceFile|
              use crate::funs::*;

              pub fn test(i:i32) -> i32 {
                  let mut s: State = State::new_state(i);
                  let stream: Vec<i32> = iter_i32();
                  for e in stream {
                      let e1: i32 = e;
                      s.gs(e1);
                  }
                  s.gs(5)
              }
            |]) >>=
        (\compiled -> do
            expected <- showCode "Expected:" singleState 
            compiled `shouldBe` expected)
      )

    describe "loop" ( do
        it "deep state simple" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                 fn test(i:i32) -> () {
                    let stream: Iterator<S> = iter();
                    for e in stream {
                        let e1: State = e;
                        e1.gs(5);
                    }
                }
                |]) >>=
            (\compiled -> do
                -- FIXME sertel/ohua-core#12
                -- FIXME sertel/ohua-core#11
                -- FIXME the state handling is still not ok!
                -- even though the output of the stateful call is not used anywhere,
                -- it needs to be the result of the loop!
                expected <- showCode "Expected:" deepStateSimple
                compiled `shouldBe` expected)

        it "single io" $
            -- FIXME sertel/ohua-core#11
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                 fn test(i:i32) -> () {
                    let io: State= State::new_state(i);
                    let stream: Iterator<i32> = iter_i32();
                    for e in stream {
                        let e1:i32 = e;
                        io.gs(e1);
                    }
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" singleIO
                compiled `shouldBe` expected)
        )
        
    describe "combo" (do
        it "thread + loop" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                 fn test(i:i32) -> i32 {
                    let s: State= State::new_state(i);
                    let sp: State = s.clone();
                    let stream: Vec<i32> = iter_i32();
                    for e in stream {
                        let e1: i32 = e;
                        let x: i32 = f_s(sp,e1);
                        s.gs(x);
                    }
                    s.gs(5)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"  threadAndLoop
                compiled `shouldBe` expected)

        it "raw state out" $ -- This example is only valid Rust code iff State is a Copy type
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                 fn test(i:i32) -> State {
                    let s: State = State::new_state(i);
                    let sp: State = s.clone();
                    let stream: Vec<i32> = iter_i32();
                    for e in stream {
                        let e1: i32 = e;
                        let x: i32 = f_s(sp,e1);
                        s.gs(x);
                    }
                    s
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" raw_state_out
                compiled `shouldBe` expected)
        )
    describe "Minimal case" (do
        it "minimal state use" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                 use crate::funs::*;

                 fn test(i:i32) -> i32 {
                    let s: State = State::new_state();
                    s.gs(5)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"  minimal
                compiled `shouldBe` expected)

        )

----------- Testoutput ------------------

simple :: SourceFile Span
simple = [sourceFile|
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
  let (state_0_0_1_tx, state_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (result_1_0_0_tx, result_1_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = result_1_0_0_rx.recv()?;
        let result_0_0_0 = crate::funs::h(var_0);
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = state_0_0_1_rx.recv()?;
        let result_1_0_0 = var_0.gs(5);
        result_1_0_0_tx.send(result_1_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let state_0_0_1 = crate::funs::State::new_state(i);
      state_0_0_1_tx.send(state_0_0_1)?;
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

thread :: SourceFile Span
thread = [sourceFile|
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
  let (r1_0_0_0_tx, r1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (state_0_0_2_tx, state_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (state_0_0_1_0_tx, state_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = state_0_0_1_0_rx.recv()?;
        let r1_0_0_0 = var_0.gs(6);
        r1_0_0_0_tx.send(r1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = state_0_0_2_rx.recv()?;
        var_0.modify(5);
        state_0_0_1_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let state_0_0_2 = crate::funs::State::new_state(i);
      state_0_0_2_tx.send(state_0_0_2)?;
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
  match r1_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}|]

deepStateSimple :: SourceFile Span
deepStateSimple = [sourceFile|
use crate::funs::*;

fn test(i: i32) -> () {
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
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_0_tx, d_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (size_0_tx, size_0_rx) = std::sync::mpsc::channel::<  usize,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  Vec<  (),>,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        while !renew {
          let sig = ctrl_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count { c_0_0_tx.send(())?; () };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let num = size_0_rx.recv()?;
        let mut collection = Vec::new();
        for _ in 0 .. num {
          let data = c_0_0_rx.recv()?;
          collection.push(data)
        };
        x_0_0_0_tx.send(collection)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      x_0_0_0_rx.recv()?;
      Ok(b_0_0_tx.send(())?)
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop { let mut var_0 = d_0_rx.recv()?; var_0.gs(5); () }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let mut stream_0_0_0 = iter();
      let hasSize =
        {
          let tmp_has_size = stream_0_0_0.iter().size_hint();
          tmp_has_size.1.is_some()
        };
      Ok(if hasSize {
        let size = stream_0_0_0.len();
        size_0_tx.send(size)?;
        let ctrl = (true, size);
        ctrl_0_tx.send(ctrl)?;
        for d in stream_0_0_0 { d_0_tx.send(d)?; () }
      } else {
        let mut size = 0;
        for d in stream_0_0_0 {
          d_0_tx.send(d)?;
          let ctrl = (false, 1);
          ctrl_0_tx.send(ctrl)?;
          size = size + 1;
          ()
        };
        size_0_tx.send(size)?;
        let ctrl = (true, 0);
        ctrl_0_tx.send(ctrl)?;
        ()
      })
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
}|]


singleIO :: SourceFile Span
singleIO = [sourceFile|
use crate::funs::*;

fn test(i: i32) -> () {
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
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (io_0_0_1_tx, io_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_1_tx, d_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (size_0_tx, size_0_rx) = std::sync::mpsc::channel::<  usize,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  Vec<  (),>,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        while !renew {
          let sig = ctrl_0_1_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count { c_0_0_tx.send(())?; () };
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
        let mut io_0_0_1_0 = io_0_0_1_rx.recv()?;
        while !renew {
          let sig = ctrl_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = d_1_rx.recv()?;
            io_0_0_1_0.gs(var_1);
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
        let num = size_0_rx.recv()?;
        let mut collection = Vec::new();
        for _ in 0 .. num {
          let data = c_0_0_rx.recv()?;
          collection.push(data)
        };
        x_0_0_0_tx.send(collection)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      x_0_0_0_rx.recv()?;
      Ok(b_0_0_tx.send(())?)
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
        size_0_tx.send(size)?;
        let ctrl = (true, size);
        ctrl_0_0_tx.send(ctrl)?;
        let ctrl = (true, size);
        ctrl_0_1_tx.send(ctrl)?;
        for d in stream_0_0_0 { d_1_tx.send(d)?; () }
      } else {
        let mut size = 0;
        for d in stream_0_0_0 {
          d_1_tx.send(d)?;
          let ctrl = (false, 1);
          ctrl_0_0_tx.send(ctrl)?;
          let ctrl = (false, 1);
          ctrl_0_1_tx.send(ctrl)?;
          size = size + 1;
          ()
        };
        size_0_tx.send(size)?;
        let ctrl = (true, 0);
        ctrl_0_0_tx.send(ctrl)?;
        let ctrl = (true, 0);
        ctrl_0_1_tx.send(ctrl)?;
        ()
      })
    }));
  tasks
    .push(Box::new(move || -> _ {
      let io_0_0_1 = State::new_state(i);
      io_0_0_1_tx.send(io_0_0_1)?;
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
}|]

singleState :: SourceFile Span
singleState = [sourceFile|
use crate::funs::*;

pub fn test(i: i32) -> i32 {
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
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel::< i32,>();
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_1_tx, d_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (s_0_1_1_tx, s_0_1_1_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = s_0_1_1_rx.recv()?;
        let b_0_0 = var_0.gs(5);
        b_0_0_tx.send(b_0_0)?;
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
        for d in stream_0_0_0 { d_1_tx.send(d)?; () }
      } else {
        let mut size = 0;
        for d in stream_0_0_0 {
          d_1_tx.send(d)?;
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
      let s_0_0_1 = State::new_state(i);
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
            let var_1 = d_1_rx.recv()?;
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
  match b_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]

threadAndLoop :: SourceFile Span
threadAndLoop = [sourceFile|
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
  let (s_0_0_2_tx, s_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (sp_0_0_0_tx, sp_0_0_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (s_0_0_1_0_tx, s_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_1_tx, d_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (s_0_1_1_tx, s_0_1_1_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let sp_0_0_0_0 = sp_0_0_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = d_1_rx.recv()?;
            let x_0_0_0 = f_s(sp_0_0_0_0, var_1);
            x_0_0_0_tx.send(x_0_0_0)?;
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
        let b_0_0 = var_0.gs(5);
        b_0_0_tx.send(b_0_0)?;
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
        let ctrl = (true, size);
        ctrl_0_1_tx.send(ctrl)?;
        for d in stream_0_0_0 { d_1_tx.send(d)?; () }
      } else {
        let mut size = 0;
        for d in stream_0_0_0 {
          d_1_tx.send(d)?;
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
      loop {
        let mut var_0 = s_0_0_2_rx.recv()?;
        let sp_0_0_0 = var_0.clone();
        sp_0_0_0_tx.send(sp_0_0_0)?;
        s_0_0_1_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_2 = State::new_state(i);
      s_0_0_2_tx.send(s_0_0_2)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let mut s_0_0_1_0_0 = s_0_0_1_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_1_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = x_0_0_0_rx.recv()?;
            s_0_0_1_0_0.gs(var_1);
            ()
          };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        };
        s_0_1_1_tx.send(s_0_0_1_0_0)?;
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
  match b_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}|]

raw_state_out :: SourceFile Span
raw_state_out = [sourceFile|
use crate::funs::*;

fn test(i: i32) -> State {
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
  let (s_0_0_2_tx, s_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (sp_0_0_0_tx, sp_0_0_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (s_0_0_1_0_tx, s_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_1_tx, d_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let sp_0_0_0_0 = sp_0_0_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = d_1_rx.recv()?;
            let x_0_0_0 = f_s(sp_0_0_0_0, var_1);
            x_0_0_0_tx.send(x_0_0_0)?;
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
        let ctrl = (true, size);
        ctrl_0_1_tx.send(ctrl)?;
        for d in stream_0_0_0 { d_1_tx.send(d)?; () }
      } else {
        let mut size = 0;
        for d in stream_0_0_0 {
          d_1_tx.send(d)?;
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
      loop {
        let mut var_0 = s_0_0_2_rx.recv()?;
        let sp_0_0_0 = var_0.clone();
        sp_0_0_0_tx.send(sp_0_0_0)?;
        s_0_0_1_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_2 = State::new_state(i);
      s_0_0_2_tx.send(s_0_0_2)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let mut s_0_0_1_0_0 = s_0_0_1_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_1_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = x_0_0_0_rx.recv()?;
            s_0_0_1_0_0.gs(var_1);
            ()
          };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        };
        s_0_1_0_tx.send(s_0_0_1_0_0)?;
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
} |]

minimal :: SourceFile Span
minimal = [sourceFile|
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = s_0_0_1_rx.recv()?;
        let a_0_0 = var_0.gs(5);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_1 = State::new_state();
      s_0_0_1_tx.send(s_0_0_1)?;
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
