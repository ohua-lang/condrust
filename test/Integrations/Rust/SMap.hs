{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SMap where

import Ohua.Prelude

import Integrations.Rust.Utils


spec :: Spec
spec =
    describe "SMap" $ do
        -- this computation is deleted by dead code elimination
        -- TODO in fact this whole computation is dead code and our
        --      compiler should detect that!
        it "stream" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                fn test() -> () {
                    let stream = iter();
                    for e in stream {
                        k(e);
                    }
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                      use funs::*;

                      fn test() -> () {
                        let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel();
                        let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel();
                        let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
                        let (size_0_tx, size_0_rx) = std::sync::mpsc::channel();
                        let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let num = size_0_rx.recv()?;
                              let collection = Vec::new();
                              for _ in 0 .. num {
                                let data = c_0_0_rx.recv()?;
                                collection.push(data)
                              };
                              x_0_0_0_tx.send(collection)?
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            let stream_0_0_0 = iter();
                            loop {
                              let data = stream_0_0_0;
                              let hasSize =
                                {
                                  let tmp_has_size = data.iter().size_hint();
                                  tmp_has_size.1.is_some()
                                };
                              if hasSize {
                                let size = data.len();
                                size_0_tx.send(size)?;
                                let ctrl = (true, size);
                                ctrl_0_tx.send(ctrl)?;
                                for d in data { d_0_tx.send(d)? }
                              } else {
                                let size = 0;
                                for d in data {
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
                            let x = ();
                            b_0_0_tx.send(x)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let renew = false;
                              let lit_unit_0 = ();
                              while !renew {
                                let sig = ctrl_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count { let var_0 = lit_unit_0; c_0_0_tx.send(c_0_0)? };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                              }
                            }
                          }));
                        run(tasks);
                        b_0_0_rx.recv()?
                      }
                    |]
                compiled `shouldBe` expected)
        it "imperative" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                fn test() -> std::Vec<i32> {
                    let s = S::new();
                    let stream = iter_i32();
                    for e in stream {
                        let r = h(e);
                        s.gs(r);
                    }
                    s
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                      use funs::*;

                      // TODO
                    |]
                compiled `shouldBe` expected)
