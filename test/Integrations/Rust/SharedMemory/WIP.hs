{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.WIP where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

spec :: Spec
spec =
    describe "Typing tests" $ do
        -- This currently errors because we have to implement extracting the type
        -- of bound values i.e. let x = somefun(), from the return type of somefun 
        it "Type from imported function" $
            compileCodeWithDebug  [sourceFile|
                use crate::funs::hello_world;

                fn test() -> String {
                    let x = hello_world();
                    x
                }
                |]  `shouldThrow` anyException
                {-) >>=
            (\compiled -> do
                expected <- showCode "Expected:" typeFromExtraction
                compiled `shouldBe` expected)-}


        it "Type from Annotation" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x: String = not_in_lib();
                    x
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" typeFromAnnotation                    
                compiled `shouldBe` expected)

        -- In this trivial example we would be able to derive the type anyways. But for now I'll 
        -- have it failing.
        it "Type from Neither" $
            compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x = not_in_lib();
                    x
                }
                |] `shouldThrow` anyException


typeFromExtraction:: SourceFile Span 
typeFromExtraction =  [sourceFile|
fn placeholder(){}
|]

typeFromAnnotation:: SourceFile Span 
typeFromAnnotation =  [sourceFile|
use crate::funs::*;

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
      let x_0_0_0 = not_in_lib();
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