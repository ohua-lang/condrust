{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.ProgModRestrictions where

import Ohua.Commons.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (Span)

-- REMINDER: Many of the 'don't use vars twice'-
--           rules are already enforced by the borrow checker in Rust
--           And as we assume valid Input Code, we could say it's not our business.
--           However, I argue that we need to enforce our model anyways because in other Languages e.g. Python
--           those code snippets are valid and all our guarantees go down the tubes if we do not check enforcement.
--           So to at least have tests that check these requirements, I'm temporarily ignoring the Borrow Checker here.
spec :: Spec
spec =
    describe "Check enforced assumptions" $ do
        it "Local vars as function input can be used only once" $
         compileCode  
            [sourceFile|
                    use crate::funs::*;
                    
                    fn test() -> String {
                        let x:i32 = f();
                        let y:i32 = h(x);
                        let z:String = g(x);
                        concat(y, z) 
                    }
            |]  `shouldThrow` anyException
        
        it "EnvVars as function input can be used only once" $
         compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test(x:i32) -> String {
                        let y:i32 = h(x);
                        let z:String = g(x);
                        concat(y, z) 
                    }
            |]   `shouldThrow` anyException
        
        
        it "Vars from other algos as function input can be used only once" $
         compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test2() -> String {
                        test(23)
                    }

                    fn test(x:i32) -> String {
                        let y:i32 = h(x);
                        let z:String = g(x);
                        concat(y, z) 
                    }
            |]   `shouldThrow` anyException
        
        it "Local vars are state OR function input - Input first" $
         compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test() -> String {
                        let x:State = f();
                        let y:i32 = g(x);
                        x.do_stuff()                        
                    }
            |]  `shouldThrow` anyException

        it "Local vars are state OR function input - State first" $
         compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test() -> String {
                        let x:State = f();
                        x.do_stuff();
                        g(x)                        
                    }
            |]   `shouldThrow` anyException
        
        -- This shouldn't work because things can either be arguments OR states, not both
        it "Env vars are state OR function input - Input first" $
         compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test(x:State) -> String {
                        let y:i32 = f_s(x, 42);
                        x.do_stuff()                        
                    }
            |]   `shouldThrow` anyException

        it "Env vars are state OR function input - State first" $
         compileCode  
            [sourceFile|
                    use crate::funs::*;
                    
                    fn test(x:State) -> String {
                        x.do_stuff();
                        take_state(x)                        
                    }
            |]   `shouldThrow` anyException
        
        it "States can be used multiple times - return State" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test(i:i32) -> State {
                        let x:State = State::new(i);
                        x.do_stuff(i);
                        x.io();
                        x                     
                    }
            |]) >>=  (\compiled -> do
                       expected <- showCode "Expected:" returnState
                       compiled `shouldBe` expected)

        it "States can be used multiple times - return State-Call" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test(i:i32) -> () {
                        let x:State = State::new(i);
                        x.do_stuff(i);
                        x.io()                   
                    }
            |]) >>= (\compiled -> do
                        expected <- showCode "Expected:" returnStateCall
                        compiled `shouldBe` expected)

        it "States can be used multiple times - return Output of State-Call" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test(i:i32) -> i32 {
                        let x:State = State::new(i);
                        x.io();
                        let y:i32 = x.do_stuff(i);
                        y                   
                    }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" returnStateCallOut
                compiled `shouldBe` expected)

        it "States can be used once in loops" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    use crate::funs::*;

                    fn test(i:i32) -> State {
                        let x:State = State::new(i);
                        for e in iter_i32() {
                            x.do_stuff(e)
                        } 
                        x      
                    }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" loopUsesState
                compiled `shouldBe` expected)

        it "States can be used multiple times but NOT in loops" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    use crate::funs::*;
                    
                    fn test(i:i32) -> State {
                        let x:State = State::new(i);
                        for e in iter_i32() {
                            x.do_stuff(e);
                            x.io()
                        } 
                        x
                    }
            |] ) `shouldThrow` anyException {->>=
            (\compiled -> do
                expected <- showCode "Expected:" loopUsesState
                compiled `shouldBe` expected)-}


----------- Testoutput ------------------------------------------

placeholder :: SourceFile Span
placeholder = [sourceFile|

pub fn hello() -> i32 {
    let x = something();
    x
}

|]

returnState :: SourceFile Span
returnState = [sourceFile|
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
  let (x_0_0_0_0_tx, x_0_0_0_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_2_tx, x_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_1_0_tx, x_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_1_0_rx.recv()?;
        var_0.io();
        x_0_0_0_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_2_rx.recv()?;
        var_0.do_stuff(i);
        x_0_0_1_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_2 = crate::funs::State::new(i);
      x_0_0_2_tx.send(x_0_0_2)?;
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
  match x_0_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]

returnStateCall :: SourceFile Span
returnStateCall = [sourceFile|
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
  let (result_0_0_0_tx, result_0_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (x_0_0_2_tx, x_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_1_0_tx, x_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_1_0_rx.recv()?;
        let result_0_0_0 = var_0.io();
        result_0_0_0_tx.send(result_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_2_rx.recv()?;
        var_0.do_stuff(i);
        x_0_0_1_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_2 = crate::funs::State::new(i);
      x_0_0_2_tx.send(x_0_0_2)?;
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

returnStateCallOut :: SourceFile Span
returnStateCallOut = [sourceFile|
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
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (x_0_0_2_tx, x_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_1_0_tx, x_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_1_0_rx.recv()?;
        let y_0_0_0 = var_0.do_stuff(i);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_2_rx.recv()?;
        var_0.io();
        x_0_0_1_0_tx.send(var_0)?
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_2 = crate::funs::State::new(i);
      x_0_0_2_tx.send(x_0_0_2)?;
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

loopUsesState :: SourceFile Span
loopUsesState = [sourceFile|
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
  let (x_0_1_0_tx, x_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_1_tx, x_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_0_tx, d_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let mut a_0_0 = crate::funs::iter_i32();
      let hasSize =
        {
          let tmp_has_size = a_0_0.iter().size_hint();
          tmp_has_size.1.is_some()
        };
      Ok(if hasSize {
        let size = a_0_0.len();
        let ctrl = (true, size);
        ctrl_0_tx.send(ctrl)?;
        for d in a_0_0 { d_0_tx.send(d)?; () }
      } else {
        let mut size = 0;
        for d in a_0_0 {
          d_0_tx.send(d)?;
          let ctrl = (false, 1);
          ctrl_0_tx.send(ctrl)?;
          size = size + 1;
          ()
        };
        let ctrl = (true, 0);
        ctrl_0_tx.send(ctrl)?;
        ()
      })
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_1 = crate::funs::State::new(i);
      x_0_0_1_tx.send(x_0_0_1)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let mut x_0_0_1_0 = x_0_0_1_rx.recv()?;
        while !renew {
          let sig = ctrl_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let var_1 = d_0_rx.recv()?;
            x_0_0_1_0.do_stuff(var_1);
            ()
          };
          let renew_next_time = sig.0;
          renew = renew_next_time;
          ()
        };
        x_0_1_0_tx.send(x_0_0_1_0)?;
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
  match x_0_1_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]
