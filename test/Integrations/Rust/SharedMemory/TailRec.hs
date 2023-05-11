{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.TailRec where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup


spec :: Spec
spec =
    describe "TailRec" $ do
     
        it "simple one argument" $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn rec(i:i32) -> i32 {
                    let j:i32 = h(i);
                    if check(j) {
                        rec(j)
                    } else {
                        j
                    }
                }

                fn test() -> i32 {
                    rec(2)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" simpleOneArgument
                compiled `shouldBe` expected)
        it "multi-argument" $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn rec(one:i32, two:i32) -> i32 {
                    let i:i32 = h(one);
                    let j:i32 = h(two);
                    let k:i32 = h2(i, j);
                    if check(k) {
                        rec(i,j)
                    } else {
                        k
                    }
                }

                fn test() -> i32 {
                    rec(2,4)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" multiArgument                    
                compiled `shouldBe` expected)
        it "contexted function" $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn rec(one: i32) -> i32 {
                    let i:i32 = h(one);
                    let j:i32 = f();
                    let k:i32 = h2(i, j);
                    if check(k) {
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
                expected <- showCode "Expected:" contextFunction
                compiled `shouldBe` expected)
   

        it "returning a unit" $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn rec(one: i32) -> () {
                    let i:i32 = h(one);
                    let k:() = h2(i);
                    if check(k) {
                        rec(i)
                    } else {
                        k
                    }
                }

                fn test() -> () {
                    rec(42)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" return_unit
                compiled `shouldBe` expected)
                
        it "FAIL: Use local result more than once. This should error but returns invalid code" $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn rec(one: i32) -> () {
                    let i:i32 = h(one);
                    let k:() = h2(i);
                    if check(i) {
                        rec(i)
                    } else {
                        k
                    }
                }

                pub fn test() -> () {
                    rec(42)
                }
                |])>>=
            (\compiled -> do
                expected <- showCode "Expected:" reuseLocal
                compiled `shouldBe` expected)

        -- we turn the below code into a recursion that never ends.
        -- the boolean to check inside the termination is input to the
        -- initial recursive call!
        it "FAIL: endless (server) loops with state runs only once" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use crate::funs::*;

                pub fn test() -> () {
                    let s:State = State::new(23);
                    loop {
                      s.io()
                    }
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" endlessLoopsWithState
                compiled `shouldBe` expected)

        -- FIXME: [ERROR] Rec like imperative while - because we return a tuple  
        it "[ERROR] Rec like imperative while - because we return a tuple  " $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn while_loop_fun(state:State, i:i32) -> S{
                  state.gs(i);
                  i.add(1);
                  if islowerthan23(i) {
                    while_loop_fun(state, i)
                  } else {
                    (state, i)
                  }
                }

                fn test() -> State {
                    let state: State  = State::new_state();
                    let mut i:i32 = I32::new(1);
                    let state: State = while_loop_fun(state, i);
                    state
                }
                |]) `shouldThrow` anyException
       {-         
      -- FIXME: SSA is broken ... see FIXME in SSA.hs
        it "Rec like imperative while - assign rec call " $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;
                /* fn while_loop_fun(state:State, i:i32) -> State{
                  if islowerthan23(i) {
                    state.gs(i);
                    i.add(1);
                    while_loop_fun(state, i)
                  } else {
                    state
                  }
                }

                fn test() -> State {
                    let state:State = State::new_state();
                    let mut i:i32 = I32::new(1);
                    let state:State = while_loop_fun(state, i);
                    state
                }*/

                fn test_rename() -> i32 {
                  let x:i32 = f();
                  let x:i32 = fun(x);
                  let y:i32 = fun2(x);
                  y
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
 use crate::funs::*;
 //ToDo
                    |]
                compiled `shouldBe` expected)


        it "Rec like imperative while- return rec call" $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use crate::funs::*;

                fn while_loop_fun(state:State, i:i32) -> State{
                  if islowerthan23(i) {
                    state.gs(i);
                    i.add(1);
                    while_loop_fun(state, i)
                  } else {
                    state
                  }
                }

                fn test() -> State {
                    let state:State = State::new_state();
                    let mut i:i32 = I32::new(1);
                    while_loop_fun(state, i);
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
 use crate::funs::*;
 //ToDo
                    |]
                compiled `shouldBe` expected)-}


----- Expected Outputs -------------------------------
simpleOneArgument = [sourceFile|
use crate::funs::*;

fn rec(i: i32) -> i32 {
  let j: i32 = h(i);
  if check(j) { rec(j) } else { j }
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (j_0_0_0_tx, j_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = j_0_0_0_rx.recv()?;
        let a_0_0 = check(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_0_0_0_rx.recv()?;
        let j_0_0_0 = h(var_0);
        j_0_0_0_tx.send(j_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      i_0_0_0_tx.send(2)?;
      while a_0_0_rx.recv()? {
        let loop_res_0 = j_0_0_0_rx.recv()?;
        i_0_0_0_tx.send(loop_res_0)?;
        ()
      };
      let finalResult = j_0_0_0_rx.recv()?;
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

multiArgument = [sourceFile|
use crate::funs::*;

fn rec(one: i32, two: i32) -> i32 {
  let i: i32 = h(one);
  let j: i32 = h(two);
  let k: i32 = h2(i, j);
  if check(k) { rec(i, j) } else { k }
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (one_0_0_0_tx, one_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (two_0_0_0_tx, two_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (j_0_0_0_tx, j_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = k_0_0_0_rx.recv()?;
        let a_0_0 = check(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_0_0_0_rx.recv()?;
        let var_1 = j_0_0_0_rx.recv()?;
        let k_0_0_0 = h2(var_0, var_1);
        k_0_0_0_tx.send(k_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = two_0_0_0_rx.recv()?;
        let j_0_0_0 = h(var_0);
        j_0_0_0_tx.send(j_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = one_0_0_0_rx.recv()?;
        let i_0_0_0 = h(var_0);
        i_0_0_0_tx.send(i_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      one_0_0_0_tx.send(2)?;
      two_0_0_0_tx.send(4)?;
      while a_0_0_rx.recv()? {
        k_0_0_0_rx.recv()?;
        let loop_res_0 = i_0_0_0_rx.recv()?;
        let loop_res_1 = j_0_0_0_rx.recv()?;
        one_0_0_0_tx.send(loop_res_0)?;
        two_0_0_0_tx.send(loop_res_1)?;
        ()
      };
      i_0_0_0_rx.recv()?;
      j_0_0_0_rx.recv()?;
      let finalResult = k_0_0_0_rx.recv()?;
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


contextFunction = [sourceFile|
use crate::funs::*;

fn rec(one: i32) -> i32 {
  let i: i32 = h(one);
  let j: i32 = f();
  let k: i32 = h2(i, j);
  if check(k) { rec(k) } else { k }
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (ctrl_0_0_0_tx, ctrl_0_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (one_0_0_0_tx, one_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (j_0_0_0_tx, j_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        while !renew {
          let sig = ctrl_0_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let j_0_0_0 = f();
            j_0_0_0_tx.send(j_0_0_0)?;
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
        let var_0 = k_0_0_0_rx.recv()?;
        let a_0_0 = check(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_0_0_0_rx.recv()?;
        let var_1 = j_0_0_0_rx.recv()?;
        let k_0_0_0 = h2(var_0, var_1);
        k_0_0_0_tx.send(k_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = one_0_0_0_rx.recv()?;
        let i_0_0_0 = h(var_0);
        i_0_0_0_tx.send(i_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let ctrlSig = (true, 1);
      ctrl_0_0_0_tx.send(ctrlSig)?;
      one_0_0_0_tx.send(2)?;
      while a_0_0_rx.recv()? {
        let ctrlSig = (true, 1);
        ctrl_0_0_0_tx.send(ctrlSig)?;
        let loop_res_0 = k_0_0_0_rx.recv()?;
        one_0_0_0_tx.send(loop_res_0)?;
        ()
      };
      let ctrlSig = (false, 0);
      ctrl_0_0_0_tx.send(ctrlSig)?;
      let finalResult = k_0_0_0_rx.recv()?;
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

return_unit = [sourceFile|
use crate::funs::*;

fn rec(one: i32) -> () {
  let i: i32 = h(one);
  let k: () = h2(i);
  if check(k) { rec(i) } else { k }
}

fn test() -> () {
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (one_0_0_0_tx, one_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = k_0_0_0_rx.recv()?;
        let a_0_0 = check(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_0_0_0_rx.recv()?;
        let k_0_0_0 = h2(var_0);
        k_0_0_0_tx.send(k_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = one_0_0_0_rx.recv()?;
        let i_0_0_0 = h(var_0);
        i_0_0_0_tx.send(i_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      one_0_0_0_tx.send(42)?;
      while a_0_0_rx.recv()? {
        k_0_0_0_rx.recv()?;
        let loop_res_0 = i_0_0_0_rx.recv()?;
        one_0_0_0_tx.send(loop_res_0)?;
        ()
      };
      i_0_0_0_rx.recv()?;
      let finalResult = k_0_0_0_rx.recv()?;
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

reuseLocal = [sourceFile|
use crate::funs::*;

fn rec(one: i32) -> () {
  let i: i32 = h(one);
  let k: () = h2(i);
  if check(i) { rec(i) } else { k }
}

pub fn test() -> () {
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let (one_0_0_0_tx, one_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel::<  i32,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_0_0_0_rx.recv()?;
        let a_0_0 = check(var_0);
        a_0_0_tx.send(a_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = i_0_0_0_rx.recv()?;
        let k_0_0_0 = h2(var_0);
        k_0_0_0_tx.send(k_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = one_0_0_0_rx.recv()?;
        let i_0_0_0 = h(var_0);
        i_0_0_0_tx.send(i_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      one_0_0_0_tx.send(42)?;
      while a_0_0_rx.recv()? {
        k_0_0_0_rx.recv()?;
        let loop_res_0 = i_0_0_0_rx.recv()?;
        one_0_0_0_tx.send(loop_res_0)?;
        ()
      };
      i_0_0_0_rx.recv()?;
      let finalResult = k_0_0_0_rx.recv()?;
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

endlessLoopsWithState = [sourceFile|
use crate::funs::*;

pub fn test() -> () {
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
  let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (result_0_0_0_tx, result_0_0_0_rx) = std::sync::mpsc::channel::<  (),>();
  let (localCondition_0_0_0_tx, localCondition_0_0_0_rx) =
    std::sync::mpsc::channel::<  bool,>();
  let (copyLocalCond_0_0_0_tx, copyLocalCond_0_0_0_rx) =
    std::sync::mpsc::channel::<  bool,>();
  let (s_0_1_0_tx, s_0_1_0_rx) = std::sync::mpsc::channel::<  State,>();
  let (ctrl_0_0_0_tx, ctrl_0_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (cond_0_0_0_tx, cond_0_0_0_rx) = std::sync::mpsc::channel::<  bool,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let mut renew = false;
        let mut s_0_0_0_1 = s_0_1_0_rx.recv()?;
        while !renew {
          let sig = ctrl_0_0_0_rx.recv()?;
          let count = sig.1;
          for _ in 0 .. count {
            let result_0_0_0 = s_0_0_0_1.io();
            result_0_0_0_tx.send(result_0_0_0)?;
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
        let var_0 = cond_0_0_0_rx.recv()?;
        let res = host_id(var_0);
        let localCondition_0_0_0 = res.0;
        localCondition_0_0_0_tx.send(localCondition_0_0_0)?;
        let copyLocalCond_0_0_0 = res.1;
        copyLocalCond_0_0_0_tx.send(copyLocalCond_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let ctrlSig = (true, 1);
      ctrl_0_0_0_tx.send(ctrlSig)?;
      cond_0_0_0_tx.send(true)?;
      while localCondition_0_0_0_rx.recv()? {
        result_0_0_0_rx.recv()?;
        let ctrlSig = (true, 1);
        ctrl_0_0_0_tx.send(ctrlSig)?;
        let loop_res_0 = copyLocalCond_0_0_0_rx.recv()?;
        cond_0_0_0_tx.send(loop_res_0)?;
        ()
      };
      let ctrlSig = (false, 0);
      ctrl_0_0_0_tx.send(ctrlSig)?;
      copyLocalCond_0_0_0_rx.recv()?;
      let finalResult = result_0_0_0_rx.recv()?;
      Ok(c_0_0_tx.send(finalResult)?)
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_1_0 = State::new(23);
      s_0_1_0_tx.send(s_0_1_0)?;
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
