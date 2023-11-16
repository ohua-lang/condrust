{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.WIP where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

spec :: Spec
spec =
    describe "Typing tests" $ do
      {-ToDo? Without type propagation, the first three tests will fail 
        it "VarType from algo return" $
        -- The only annotation we have is the algorythm return type but that should suffice here
            (showCode "Compiled: " =<< compileCode [sourceFile|
              
                fn test() -> String {
                    let x = hello_world();
                    x
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" typeFromReturn
                compiled `shouldBe` expected)

        it "FunType from algo return" $
        -- The only annotation we have is the algorythm return type but that should suffice here
            (showCode "Compiled: " =<< compileCode [sourceFile|

                fn test() -> String {
                    hello_world()
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" funTypeFromReturn
                compiled `shouldBe` expected)



        it "Type from Assignment Annotation" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                
                fn test() -> usize {
                    let x: String = hello_world();
                    let y: usize = take_string(x);
                    y
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" typeFromAnnotation                    
                compiled `shouldBe` expected)
        -}
        -- ToDo: The Output here should be identical to the output of the above case, except for the
        --       `use` statement. It would be nice if we could test it that way.
        it "Type from Extracted Function Types" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use crate::funs::*;

                fn test() -> usize {
                    let x = hello_world();
                    let y = take_string(x);
                    y
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" typeFromExtraction              
                compiled `shouldBe` expected)

------------- Testouput -------------------------------------

typeFromReturn:: SourceFile Span 
typeFromReturn =  [sourceFile|
fn test() -> String {
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
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = hello_world();
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
  match x_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
   
|]

funTypeFromReturn:: SourceFile Span 
funTypeFromReturn =  [sourceFile|
fn test() -> String {
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
  let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      let a_0_0 = hello_world();
      a_0_0_tx.send(a_0_0)?;
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


typeFromAnnotation:: SourceFile Span 
typeFromAnnotation =  [sourceFile|
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
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  usize,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let y_0_0_0 = take_string(var_0);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = hello_world();
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
  match y_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]

typeFromExtraction :: SourceFile Span
typeFromExtraction =  [sourceFile|
use crate::funs::*;

 
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
  let (y_0_0_0_tx, y_0_0_0_rx) = std::sync::mpsc::channel::<  usize,>();
  let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel::<  String,>();
  let mut tasks: Vec<  Box<  dyn FnOnce() -> Result<(), RunError> + Send,>,> =
    Vec::new();
  tasks
    .push(Box::new(move || -> _ {
      loop {
        let var_0 = x_0_0_0_rx.recv()?;
        let y_0_0_0 = crate::funs::take_string(var_0);
        y_0_0_0_tx.send(y_0_0_0)?;
        ()
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let x_0_0_0 = crate::funs::hello_world();
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
  match y_0_0_0_rx.recv() {
    Ok(res) => res,
    Err(e) => panic!("[Ohua Runtime Internal Exception] {}", e),
  }
}
|]