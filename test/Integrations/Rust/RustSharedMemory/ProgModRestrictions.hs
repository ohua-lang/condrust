{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustSharedMemory.ProgModRestrictions where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.RustSharedMemory.RustSetup
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

                    fn test(x:i32) -> String {
                        let y:i32 = h(x);
                        let z:String = g(x);
                        concat(y, z) 
                    }
            |]   `shouldThrow` anyException
        
        
        it "Vars from other algos as function input can be used only once" $
         compileCode  
            [sourceFile|

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
                    
                    fn test() -> String {
                        let x:State = f();
                        let y:i32 = g(x);
                        x.do_stuff()                        
                    }
            |]  `shouldThrow` anyException

        it "Local vars are state OR function input - State first" $
         compileCode  
            [sourceFile|
                    
                    fn test() -> String {
                        let x:State = f();
                        x.do_stuff();
                        g(x)                        
                    }
            |]   `shouldThrow` anyException
        
        -- QUESTION: I'm not sure if this should work anyways. Question is are 
        --           States passed as algorithm in put already 'used as function input'
        --           i.e. can envvars ever be state and are algorithms and function the same in this regard?
        it "Env vars are state OR function input - Input first" $
         compileCode  
            [sourceFile|
                    
                    fn test(x:State) -> String {
                        let y:i32 = g(x);
                        x.do_stuff()                        
                    }
            |]   `shouldThrow` anyException

        it "Env vars are state OR function input - State first" $
         compileCode  
            [sourceFile|
                    
                    fn test(x:State) -> String {
                        x.do_stuff();
                        g(x)                        
                    }
            |]   `shouldThrow` anyException
        
        it "States can be used multiple times - return State" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    
                    fn test(i:i32) -> String {
                        let x:State = State::new();
                        x.do_stuff(i);
                        x.make();
                        x                     
                    }
            |]) >>=  (\compiled -> do
                       expected <- showCode "Expected:" return_state
                       compiled `shouldBe` expected)

        it "States can be used multiple times - return State-Call" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    
                    fn test(i:i32) -> String {
                        let x:State = State::new();
                        x.do_stuff(i);
                        x.make()                   
                    }
            |]) >>= (\compiled -> do
                        expected <- showCode "Expected:" return_state_call
                        compiled `shouldBe` expected)

        it "States can be used multiple times - return Output of State-Call" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    
                    fn test(i:i32) -> String {
                        let x:State = State::new();
                        x.do_stuff(i);
                        let y:i32 = x.make();
                        y                   
                    }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" return_state_call_out
                compiled `shouldBe` expected)

        it "States can be used once in loops" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|
                    
                    fn test(i:i32) -> String {
                        let iter:Iterator<i32> = range(0,23);
                        let x:State = State::new();
                        for i in iter {
                            let i1:i32 = id(i);
                            x.do_stuff(i1)
                        } 
                        x      
                    }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" loop_uses_state
                compiled `shouldBe` expected)

        it "States can be used multiple times but NOT in loops" $
         compileCode  
            [sourceFile|
                    
                    fn test(i:i32) -> String {
                        let iter:Iterator<i32> = range(0,23);
                        let x:State = State::new();
                        for i in iter {
                            let i:i32 = id(i);
                            x.do_stuff(i);
                            x.make()
                        } 
                        x
                    }
            |]  `shouldThrow` anyException



----------- Testoutput ------------------------------------------

placeholder = [sourceFile|

pub fn hello() -> i32d {
    let x = something();
    x
}

|]

return_state = [sourceFile|
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
  let (x_0_0_0_0_tx, x_0_0_0_0_rx) = std::sync::mpsc::channel();
  let (x_0_0_2_tx, x_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_1_0_tx, x_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_1_0_rx.recv()?;
        var_0.make();
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
      let x_0_0_2 = State::new();
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

return_state_call = [sourceFile|
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
  let (x_0_0_2_tx, x_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_1_0_tx, x_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_1_0_rx.recv()?;
        let a_0_0 = var_0.make();
        a_0_0_tx.send(a_0_0)?;
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
      let x_0_0_2 = State::new();
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
  match a_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }

}
|]
return_state_call_out = [sourceFile|
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
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel();
  let (x_0_0_2_tx, x_0_0_2_rx) = std::sync::mpsc::channel::<  State,>();
  let (x_0_0_1_0_tx, x_0_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut var_0 = x_0_0_1_0_rx.recv()?;
        let y_0_0_0 = var_0.make();
        y_0_0_0_tx.send(y_0_0_0)?;
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
      let x_0_0_2 = State::new();
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

loop_uses_state = [sourceFile|
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
  let (x_0_1_0_tx, x_0_1_0_rx) = std::sync::mpsc::channel();
  let (x_0_0_1_tx, x_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel::<  (_, _),>();
  let (d_0_tx, d_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i1_0_0_0_tx, i1_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = d_0_rx.recv()?;
        let i1_0_0_0 = id(var_0);
        i1_0_0_0_tx.send(i1_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_1 = State::new();
      x_0_0_1_tx.send(x_0_0_1)?;
      Ok(())
    }));
  tasks
    .push(Box::new(move || -> _ {
      let mut iter_0_0_0 = range(0, 23);
      loop {
        let hasSize =
          {
            let tmp_has_size = iter_0_0_0.iter().size_hint();
            tmp_has_size.1.is_some()
          };
        if hasSize {
          let size = iter_0_0_0.len();
          let ctrl = (true, size);
          ctrl_0_tx.send(ctrl)?;
          for d in iter_0_0_0 { d_0_tx.send(d)?; () }
        } else {
          let mut size = 0;
          for d in iter_0_0_0 {
            d_0_tx.send(d)?;
            let ctrl = (false, 1);
            ctrl_0_tx.send(ctrl)?;
            size = size + 1;
            ()
          };
          let ctrl = (true, 0);
          ctrl_0_tx.send(ctrl)?;
          ()
        }
      }
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
            let var_1 = i1_0_0_0_rx.recv()?;
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