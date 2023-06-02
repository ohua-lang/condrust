{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.Scoping where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (Span)


spec :: Spec
spec =
    describe "Check scoped binding types" $ do
        it "Scope Bindings" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test() -> i32 {
                        let x:i32 = f();
                        let mut s:State = State::new(x);
                        {
                            let x:String = g();
                            s.do_it(x);
                        }
                        h(x)
                    }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" scoped
                compiled `shouldBe` expected)

        it "(For)Loop Scope Bindings" $
         (showCode "Compiled: " =<< compileCode
            [sourceFile|
                use crate::funs::*;
                
                fn test() -> i32 {
                    let x:i32 = f();
                    let mut s:State = State::new(x);
                    for i in iter_i32() {
                        let x:String = g();
                        s.do_it(x);
                    }
                    h(x)
                }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" scopedFor
                compiled `shouldBe` expected)
                
        it "[ERROR] Branch Scope Bindings" $
         compileCode  
            [sourceFile|
                use crate::funs::*;

                fn test() -> i32 {
                    let x:i32 = f();
                    let s:State = State::new(x);
                    if check() {
                        let x:String = g();
                        s.do_it(x)
                        }
                    h(x)
                }
            |] `shouldThrow` anyException

        it "Global const in scope" $
         (showCode "Compiled: " =<< compileCode
            [sourceFile|
                use crate::funs::*;
                const global:i32 = 9;

                fn test() -> usize {
                    let x:i32 = f(global);
                    let y:String = from_int(x);
                    take_string(y)
                }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" scopedConst
                compiled `shouldBe` expected)

        it "Global const in 'all' algos" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                use crate::funs::*;
                
                const global:i32 = 9;
                static st_thing:Char = 'V';

                fn test() -> i32 {
                    let x:i32 = f(global);
                    let y:String = second_algo(x);
                    h(y)
                }

                fn second_algo(arg:i32)-> String {
                    let mut as_string:String = to_string(arg);
                    as_string.push(st_thing);
                    as_string
                }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" scopedStatic
                compiled `shouldBe` expected)

        it "Global vs local shadowing" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                use crate::funs::*;
                
                const global:i32 = 9;
                static st_thing:Char = 'V';

                fn test() -> usize {
                    let first:i32 = h(global);
                    let global:i32 = h2(first);
                    let y:String = take_char_i(st_thing, global);
                    take_string(y)
                }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" scopedGlobalLocal
                compiled `shouldBe` expected)

        it "local vs local shadowing" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                use crate::funs::*;
                
                fn test() -> i32 {
                    let x:i32 = f();
                    let x:String = fun_call(x);
                    h(x)
                }
            |])  `shouldThrow` anyException

        it "local vs inner local shadowing" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                use crate::funs::*;
                
                fn test() -> i32 {
                    let x:i32 = some_int(); 
                    let y:String = {
                        let x:String = from_int(42);
                        x
                    };
                    int_and_string(x,y)
                }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" scopedLocalInner
                compiled `shouldBe` expected)


        it "Error on mutable globals" $
         compileCode  
            [sourceFile|
                use crate::funs::*;
                
                static mut st_thing:String = String::from(" times hello");

                fn test() -> i32 {
                    let x:i32 = take_string(st_thing);
                    let y:String = second_algo(x);
                    h(y)
                }

            |] `shouldThrow` anyException
            
--------------- Testoutputs -----------------------
scoped :: SourceFile Span
scoped = [sourceFile|
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_1_0_0_tx, x_1_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let a_0_0 = h(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = s_0_0_1_rx.recv()?;
        let var_1 = x_1_0_0_rx.recv()?;
        var_0.do_it(var_1);
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_1_0_0 = g();
      x_1_0_0_tx.send(x_1_0_0)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let s_0_0_1 = State::new(var_0);
        s_0_0_1_tx.send(s_0_0_1)?;
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

scopedFor :: SourceFile Span
scopedFor = [sourceFile|
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (x_1_0_0_tx, x_1_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
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
            let x_1_0_0 = g();
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
        let mut s_0_0_1_0 = s_0_0_1_rx.recv()?;
        while !renew {
          let sig = ctrl_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = x_1_0_0_rx.recv()?;
            s_0_0_1_0.do_it(var_1);
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
        let var_0 = x_0_0_0_rx.recv()?;
        let c_0_0 = h(var_0);
        c_0_0_tx.send(c_0_0)?;
        ()
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
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let s_0_0_1 = State::new(var_0);
        s_0_0_1_tx.send(s_0_0_1)?;
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
  match c_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]

scopedConst :: SourceFile Span
scopedConst = [sourceFile|
use crate::funs::*;

const global: i32 = 9;

fn test() -> usize {
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  usize,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = y_0_0_0_rx.recv()?;
        let a_0_0 = take_string(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let y_0_0_0 = from_int(var_0);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = f(global);
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

scopedStatic :: SourceFile Span
scopedStatic = [sourceFile|
use crate::funs::*;
                

const global: i32 = 9;

static st_thing: Char = 'V';

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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (as_string_0_0_1_tx, as_string_0_0_1_rx) =
    std::sync::mpsc::channel::<  String,>();
  let (as_string_0_0_0_0_tx, as_string_0_0_0_0_rx) =
    std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = as_string_0_0_0_0_rx.recv()?;
        let a_0_0 = h(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = as_string_0_0_1_rx.recv()?;
        var_0.push(st_thing);
        as_string_0_0_0_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let as_string_0_0_1 = to_string(var_0);
        as_string_0_0_1_tx.send(as_string_0_0_1)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = f(global);
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

fn second_algo(arg: i32) -> String {
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
  let (as_string_0_0_0_0_tx, as_string_0_0_0_0_rx) =
    std::sync::mpsc::channel::<  String,>();
  let (as_string_0_0_1_tx, as_string_0_0_1_rx) =
    std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = as_string_0_0_1_rx.recv()?;
        var_0.push(st_thing);
        as_string_0_0_0_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let as_string_0_0_1 = to_string(arg);
      as_string_0_0_1_tx.send(as_string_0_0_1)?;
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
  match as_string_0_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]
scopedGlobalLocal :: SourceFile Span
scopedGlobalLocal = [sourceFile|
use crate::funs::*;

const global: i32 = 9;

static st_thing: Char = 'V';

fn test() -> usize {
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  usize,>();
  let (first_0_0_0_tx, first_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (global_1_0_0_tx, global_1_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = y_0_0_0_rx.recv()?;
        let a_0_0 = take_string(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_1 = global_1_0_0_rx.recv()?;
        let y_0_0_0 = take_char_i(st_thing, var_1);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = first_0_0_0_rx.recv()?;
        let global_1_0_0 = h2(var_0);
        global_1_0_0_tx.send(global_1_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let first_0_0_0 = h(global);
      first_0_0_0_tx.send(first_0_0_0)?;
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

scopedLocalInner :: SourceFile Span
scopedLocalInner = [sourceFile|
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_1_0_0_tx, x_1_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let var_1 = x_1_0_0_rx.recv()?;
        let a_0_0 = int_and_string(var_0, var_1);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_1_0_0 = from_int(42);
      x_1_0_0_tx.send(x_1_0_0)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = some_int();
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
