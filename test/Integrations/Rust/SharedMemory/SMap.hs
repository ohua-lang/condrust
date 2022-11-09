{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.SMap where

import Ohua.Prelude

import Integrations.Rust.SharedMemory.Setup


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
--                use funs::*;
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
--                      use funs::*;
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
                use funs::*;

                fn test() -> S {
                    let s:S = S::new_state();
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
 {-      it "imperative while " $
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                use funs::*;

                fn test() -> S {
                    let state:State = S::new_state();
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
 use funs::*;
 //ToDo
                    |]
                compiled `shouldBe` expected)-}


------------ Testoutput ------------------------
imperative =  [sourceFile|
use funs::*;

fn test() -> S {
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
  let (s_0_1_0_tx, s_0_1_0_rx) = std::sync::mpsc::channel::<  S,>();
  let (s_0_0_1_tx, s_0_0_1_rx) = std::sync::mpsc::channel::<  S,>();
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    std::sync::mpsc::channel::<  (bool, usize),>();
  let (d_0_tx, d_0_rx) = std::sync::mpsc::channel::<  S,>();
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
        }
      }
    }));
  tasks
    .push(Box::new(move || -> _ {
      let s_0_0_1 = S::new_state();
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
