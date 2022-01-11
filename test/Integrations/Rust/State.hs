{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.State where

import Ohua.Prelude  ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils

spec :: Spec
spec =
  describe "State" $ do
    describe "flat" ( do
      it "simple" $
        (showCode "Compiled: " =<< compileCode OhuaOnly [sourceFile|
          use funs::*;

          fn test(i: i32) -> i32 {
            let state = S::new_state(i);
            let result = state.gs(5);
            h(result)
          }
          |]) >>=
        (\compiled -> do
          expected <- showCode "Expected:"
            [sourceFile|
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
  let (state_0_0_1_tx, state_0_0_1_rx) = std::sync::mpsc::channel::<  S,>();
  let (result_0_0_0_tx, result_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = result_0_0_0_rx.recv()?;
        let a_0_0 = h(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let state_0_0_1 = S::new_state(i);
      state_0_0_1_tx.send(state_0_0_1)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = state_0_0_1_rx.recv()?;
        let result_0_0_0 = var_0.gs(5);
        result_0_0_0_tx.send(result_0_0_0)?;
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
  match a_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
            |]
          compiled `shouldBe` expected)
      it "thread" $
        (showCode "Compiled: " =<< compileCode OhuaOnly [sourceFile|
          use funs::*;

          fn test(i: i32) -> String {
            let state = S::new_state(i);
            state.modify(5);
            let r1 = state.gs1(6);
            r1
          }
          |]) >>=
        (\compiled -> do
          expected <- showCode "Expected:"
            [sourceFile|
use funs::*;

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
  let (r1_0_0_0_tx, r1_0_0_0_rx) = std::sync::mpsc::channel();
  let (state_0_0_2_tx, state_0_0_2_rx) = std::sync::mpsc::channel::<  S,>();
  let (state_0_0_1_0_tx, state_0_0_1_0_rx) = std::sync::mpsc::channel::<  S,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
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
      let state_0_0_2 = S::new_state(i);
      state_0_0_2_tx.send(state_0_0_2)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = state_0_0_1_0_rx.recv()?;
        let r1_0_0_0 = var_0.gs1(6);
        r1_0_0_0_tx.send(r1_0_0_0)?;
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
  match r1_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
              |]
          compiled `shouldBe` expected)
         )
    describe "loop" ( do
        it "deep state simple" $
            (showCode "Compiled: " =<< compileCode OhuaOnly [sourceFile|
                use funs::*;

                 fn test(i:i32) -> () {
                    let stream = iter();
                    for e in stream {
                        e.gs(5);
                    }
                }
                |]) >>=
            (\compiled -> do
                -- FIXME sertel/ohua-core#12
                -- FIXME sertel/ohua-core#11
                -- FIXME the state handling is still not ok!
                -- even though the output of the stateful call is not used anywhere,
                -- it needs to be the result of the loop!
                expected <- showCode "Expected:"
                    [sourceFile|
use funs::*;

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
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel();
  let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel();
  let (d_0_tx, d_0_rx) = std::sync::mpsc::channel::<  S,>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
  let (size_0_tx, size_0_rx) = std::sync::mpsc::channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop { let mut var_0 = d_0_rx.recv()?; var_0.gs(5); () }
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
      let mut stream_0_0_0 = iter();
      loop {
        let hasSize =
          {
            let tmp_has_size = stream_0_0_0.iter().size_hint();
            tmp_has_size.1.is_some()
          };
        if hasSize {
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
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      x_0_0_0_rx.recv()?;
      Ok(b_0_0_tx.send(())?)
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        while !renew {
          let sig = ctrl_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count { c_0_0_tx.send(c_0_0)?; () };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        }
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
                compiled `shouldBe` expected)
        it "single io" $
            -- FIXME sertel/ohua-core#11
            (showCode "Compiled: " =<< compileCode OhuaOnly [sourceFile|
                use funs::*;

                 fn test(i:i32) -> () {
                    let io = S::new_state(i);
                    let stream = iter_i32();
                    for e in stream {
                        io.gs(e);
                    }
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
use funs::*;

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
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel();
  let (io_0_0_1_tx, io_0_0_1_rx) = std::sync::mpsc::channel::<  S,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) = std::sync::mpsc::channel::<  (_, _),>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) = std::sync::mpsc::channel();
  let (d_1_tx, d_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
  let (size_0_tx, size_0_rx) = std::sync::mpsc::channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
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
      let mut stream_0_0_0 = iter_i32();
      loop {
        let hasSize =
          {
            let tmp_has_size = stream_0_0_0.iter().size_hint();
            tmp_has_size.1.is_some()
          };
        if hasSize {
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
          for _ in 0 .. count { c_0_0_tx.send(c_0_0)?; () };
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
      let io_0_0_1 = S::new_state(i);
      io_0_0_1_tx.send(io_0_0_1)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      x_0_0_0_rx.recv()?;
      Ok(b_0_0_tx.send(())?)
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
                compiled `shouldBe` expected)
        it "single state" $
            (showCode "Compiled: " =<< compileCode OhuaOnly [sourceFile|
                use funs::*;

                 fn test(i:i32) -> () {
                    let s = S::new_state(i);
                    let stream = iter_i32();
                    for e in stream {
                        s.gs(e);
                    }
                    s.gs(5)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
use funs::*;

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
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel();
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  S,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) = std::sync::mpsc::channel::<  (_, _),>();
  let (d_1_tx, d_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (s_0_1_1_tx, s_0_1_1_rx) = std::sync::mpsc::channel::<  S,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_1 = S::new_state(i);
      s_0_0_1_tx.send(s_0_0_1)?;
      Ok(())
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
      loop {
        let hasSize =
          {
            let tmp_has_size = stream_0_0_0.iter().size_hint();
            tmp_has_size.1.is_some()
          };
        if hasSize {
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
        }
      }
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
                compiled `shouldBe` expected)
         )
    describe "combo" (do
        it "thread + loop" $
            (showCode "Compiled: " =<< compileCode OhuaOnly [sourceFile|
                use funs::*;

                 fn test(i:i32) -> () {
                    let s = S::new_state(i);
                    let sp = s.clone();
                    let stream = iter_i32();
                    for e in stream {
                        let x = f_s(sp,e);
                        s.gs(x);
                    }
                    s.gs(5)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
use funs::*;

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
  let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel();
  let (s_0_0_2_tx, s_0_0_2_rx) = std::sync::mpsc::channel::<  S,>();
  let (s_0_0_1_0_tx, s_0_0_1_0_rx) = std::sync::mpsc::channel::<  S,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) = std::sync::mpsc::channel::<  (_, _),>();
  let (sp_0_0_0_tx, sp_0_0_0_rx) = std::sync::mpsc::channel::<  &S,>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) = std::sync::mpsc::channel::<  (_, _),>();
  let (d_1_tx, d_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (s_0_1_1_tx, s_0_1_1_rx) = std::sync::mpsc::channel::<  S,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let mut stream_0_0_0 = iter_i32();
      loop {
        let hasSize =
          {
            let tmp_has_size = stream_0_0_0.iter().size_hint();
            tmp_has_size.1.is_some()
          };
        if hasSize {
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
        }
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
      loop {
        let mut var_0 = s_0_0_2_rx.recv()?;
        let sp_0_0_0 = var_0.clone();
        sp_0_0_0_tx.send(sp_0_0_0)?;
        s_0_0_1_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_2 = S::new_state(i);
      s_0_0_2_tx.send(s_0_0_2)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let sp_0_0_0_0 = sp_0_0_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_1_rx.recv()?;
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
        let mut renew = false;
        let mut s_0_0_1_0_0 = s_0_0_1_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_0_rx.recv()?;
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
}
                    |]
                compiled `shouldBe` expected)
        it "raw state out" $
            (showCode "Compiled: " =<< compileCode OhuaOnly [sourceFile|
                use funs::*;

                 fn test(i:i32) -> S {
                    let s = S::new_state(i);
                    let sp = s.clone();
                    let stream = iter_i32();
                    for e in stream {
                        let x = f_s(sp,e);
                        s.gs(x);
                    }
                    s
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
use funs::*;

fn test(i: i32) -> S {
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
  let (s_0_1_0_tx, s_0_1_0_rx) = std::sync::mpsc::channel();
  let (s_0_0_2_tx, s_0_0_2_rx) = std::sync::mpsc::channel::<  S,>();
  let (s_0_0_1_0_tx, s_0_0_1_0_rx) = std::sync::mpsc::channel::<  S,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) = std::sync::mpsc::channel::<  (_, _),>();
  let (sp_0_0_0_tx, sp_0_0_0_rx) = std::sync::mpsc::channel::<  &S,>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) = std::sync::mpsc::channel::<  (_, _),>();
  let (d_1_tx, d_1_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let mut stream_0_0_0 = iter_i32();
      loop {
        let hasSize =
          {
            let tmp_has_size = stream_0_0_0.iter().size_hint();
            tmp_has_size.1.is_some()
          };
        if hasSize {
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
        }
      }
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
      let s_0_0_2 = S::new_state(i);
      s_0_0_2_tx.send(s_0_0_2)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let sp_0_0_0_0 = sp_0_0_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_1_rx.recv()?;
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
        let mut renew = false;
        let mut s_0_0_1_0_0 = s_0_0_1_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_0_rx.recv()?;
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
}
                    |]
                compiled `shouldBe` expected)
        )
