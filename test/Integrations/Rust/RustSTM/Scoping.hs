{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustSTM.Scoping where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.RustSTM.RustSetup


spec :: Spec
spec =
    describe "Check scoped binding types" $ do
        it "Scope Bindings" $
         (showCode "Compiled: " =<< compileCode  
            [sourceFile|

                    fn test() -> i32 {
                        let x:i32 = f();
                        let s:State = State::new();
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

                fn test() -> i32 {
                    let x:i32 = f();
                    let s:State = State::new();
                    for i in iter() {
                        let x:String = g();
                        s.do_it(x);
                    }
                    h(x)
                }
            |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" scoped_for
                compiled `shouldBe` expected)
                
        it "[ERROR] Branch Scope Bindings" $
         compileCode  
            [sourceFile|

                fn test() -> i32 {
                    let x:i32 = f();
                    let s:State = State::new();
                    if check() {
                        let x:String = g();
                        s.do_it(x)
                        }
                    h(x)
                }
            |] `shouldThrow` anyException


scoped = [sourceFile|
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
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
      let s_0_0_1 = State::new();
      s_0_0_1_tx.send(s_0_0_1)?;
      Ok(())
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

scoped_for = [sourceFile|
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
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) = std::sync::mpsc::channel::<  (_, _),>();
  let (ctrl_0_1_tx, ctrl_0_1_rx) = std::sync::mpsc::channel();
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
      let mut a_0_0 = iter();
      loop {
        let hasSize =
          {
            let tmp_has_size = a_0_0.iter().size_hint();
            tmp_has_size.1.is_some()
          };
        if hasSize {
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
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_1 = State::new();
      s_0_0_1_tx.send(s_0_0_1)?;
      Ok(())
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