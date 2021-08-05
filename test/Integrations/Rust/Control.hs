{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.Control where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils


spec :: Spec
spec =
    describe "Control" $
    describe "var reuse" $ do
        -- FIXME see issue sertel/ohua-core#16
        it "simple" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one:i32) -> i32 {
                    // FIXME(feliix42): This is very unfortunate! Normally there'd be no cloning necessary here...
                    let o2 = one.clone();
                    let i = h(o2);
                    let j = h(one);
                    let k = h2(i, j);
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
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        fn rec(one: i32) -> i32 {
                          let i = h(one);
                          let j = h(one);
                          let k = h2(i, j);
                          if check(k) { rec(k) } else { k }
                        }

                        fn test() -> i32 {
                          let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
                          let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
                          let (one_0_1_0_tx, one_0_1_0_rx) = std::sync::mpsc::channel();
                          let (ctrl_0_0_0_tx, ctrl_0_0_0_rx) = std::sync::mpsc::channel();
                          let (j_0_0_0_tx, j_0_0_0_rx) = std::sync::mpsc::channel();
                          let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel();
                          let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel();
                          let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                          tasks
                            .push(Box::new(move || -> _ {
                              loop {
                                let renew = false;
                                let one_0_0_0_0_0 = one_0_1_0_rx.recv()?;
                                while !renew {
                                  let sig = ctrl_0_0_0_rx.recv()?;
                                  let count = sig.1;
                                  for _ in 0 .. count {
                                    let var_0 = one_0_0_0_0_0;
                                    let i_0_0_0 = h(var_0);
                                    i_0_0_0_tx.send(i_0_0_0)?
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
                                let var_0 = k_0_0_0_rx.recv()?;
                                let a_0_0 = check(var_0);
                                a_0_0_tx.send(a_0_0)?
                              }
                            }));
                          tasks
                            .push(Box::new(move || -> _ {
                              let ctrlSig = (true, 1);
                              ctrl_0_0_0_tx.send(ctrlSig)?;
                              let init_0 = 2;
                              one_0_1_0_tx.send(init_0)?;
                              while a_0_0_rx.recv()? {
                                k_0_0_0_rx.recv()?;
                                let ctrlSig = (true, 1);
                                ctrl_0_0_0_tx.send(ctrlSig)?;
                                let loop_res_0 = k_0_0_0_rx.recv()?;
                                one_0_1_0_tx.send(loop_res_0)?;
                                ()
                              };
                              let ctrlSig = (false, 0);
                              ctrl_0_0_0_tx.send(ctrlSig)?;
                              let finaResult = k_0_0_0_rx.recv()?;
                              c_0_0_tx.send(finalResult)?
                            }));
                          tasks
                            .push(Box::new(move || -> _ {
                              loop {
                                let var_0 = i_0_0_0_rx.recv()?;
                                let var_1 = j_0_0_0_rx.recv()?;
                                let k_0_0_0 = h2(var_0, var_1);
                                k_0_0_0_tx.send(k_0_0_0)?
                              }
                            }));
                          tasks
                            .push(Box::new(move || -> _ {
                              loop {
                                let renew = false;
                                let one_0_0_0_0_1 = one_0_1_0_rx.recv()?;
                                while !renew {
                                  let sig = ctrl_0_0_0_rx.recv()?;
                                  let count = sig.1;
                                  for _ in 0 .. count {
                                    let var_0 = one_0_0_0_0_1;
                                    let j_0_0_0 = h(var_0);
                                    j_0_0_0_tx.send(j_0_0_0)?
                                  };
                                  let renew_next_time = sig.0;
                                  renew = renew_next_time;
                                  ()
                                };
                                ()
                              }
                            }));
                          run(tasks);
                          c_0_0_rx.recv()?
                        }
                    |]
                compiled `shouldBe` expected)
        it "dep" $
          -- fusion is more tricky here because `one` is used by data dependent calls!
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one:i32) -> i32 {
                    let i = h(one);
                    let k = h2(one, i);
                    if check(k) {
                        rec(k)
                    } else {
                        k
                    }
                }

                fn test() -> i32 {
                    rec(2,4)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                      use funs::*;

                      fn rec(one: i32) -> i32 {
                        let i = h(one);
                        let k = h2(one, i);
                        if check(k) { rec(k) } else { k }
                      }

                      fn test() -> i32 {
                        let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
                        let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
                        let (one_0_1_0_tx, one_0_1_0_rx) = std::sync::mpsc::channel();
                        let (ctrl_0_0_0_tx, ctrl_0_0_0_rx) = std::sync::mpsc::channel();
                        let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel();
                        let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let renew = false;
                              let one_0_0_0_0_0 = one_0_1_0_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_0_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = one_0_0_0_0_0;
                                  let i_0_0_0 = h(var_0);
                                  i_0_0_0_tx.send(i_0_0_0)?
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
                              let var_0 = k_0_0_0_rx.recv()?;
                              let a_0_0 = check(var_0);
                              a_0_0_tx.send(a_0_0)?
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            let ctrlSig = (true, 1);
                            ctrl_0_0_0_tx.send(ctrlSig)?;
                            let init_0 = 2;
                            one_0_1_0_tx.send(init_0)?;
                            while a_0_0_rx.recv()? {
                              k_0_0_0_rx.recv()?;
                              let ctrlSig = (true, 1);
                              ctrl_0_0_0_tx.send(ctrlSig)?;
                              let loop_res_0 = k_0_0_0_rx.recv()?;
                              one_0_1_0_tx.send(loop_res_0)?;
                              ()
                            };
                            let ctrlSig = (false, 0);
                            ctrl_0_0_0_tx.send(ctrlSig)?;
                            let finaResult = k_0_0_0_rx.recv()?;
                            c_0_0_tx.send(finalResult)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let renew = false;
                              let one_0_0_0_0_1 = one_0_1_0_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_0_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = one_0_0_0_0_1;
                                  let var_1 = i_0_0_0_rx.recv()?;
                                  let k_0_0_0 = h2(var_0, var_1);
                                  k_0_0_0_tx.send(k_0_0_0)?
                                };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                              };
                              ()
                            }
                          }));
                        run(tasks);
                        c_0_0_rx.recv()?
                      }
                    |]
                compiled `shouldBe` expected)
