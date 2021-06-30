{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.TailRec where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils


spec :: Spec
spec =
    describe "TailRec" $ do
        it "simple one argument" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(i:i32) -> i32 {
                    let j = h(i);
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
                expected <- showCode "Expected:"
                    [sourceFile|
                      use funs::*;

                      fn rec(i: i32) -> i32 {
                        let j = h(i);
                        if check(j) { rec(j) } else { j }
                      }

                      fn test() -> i32 {
                        let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
                        let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
                        let (i_0_1_0_tx, i_0_1_0_rx) = std::sync::mpsc::channel();
                        let (ctrl_0_0_0_tx, ctrl_0_0_0_rx) = std::sync::mpsc::channel();
                        let (j_0_0_0_tx, j_0_0_0_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                          .push(Box::new(move || -> _ {
                            let ctrlSig = (true, 1);
                            ctrl_0_0_0_tx.send(ctrlSig)?;
                            let init_0 = 2;
                            i_0_1_0_tx.send(init_0)?;
                            while a_0_0_rx.recv()? {
                              j_0_0_0_rx.recv()?;
                              let ctrlSig = (true, 1);
                              ctrl_0_0_0_tx.send(ctrlSig)?;
                              let loop_res_0 = j_0_0_0_rx.recv()?;
                              i_0_1_0_tx.send(loop_res_0)?;
                              ()
                            };
                            let ctrlSig = (false, 0);
                            ctrl_0_0_0_tx.send(ctrlSig)?;
                            let finaResult = j_0_0_0_rx.recv()?;
                            c_0_0_tx.send(finalResult)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let renew = false;
                              let i_0_0_0_0 = i_0_1_0_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_0_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = i_0_0_0_0;
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
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let var_0 = j_0_0_0_rx.recv()?;
                              let a_0_0 = check(var_0);
                              a_0_0_tx.send(a_0_0)?
                            }
                          }));
                        run(tasks);
                        c_0_0_rx.recv()?
                      }
                    |]
                compiled `shouldBe` expected)
        it "multi-argument" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one:i32, two:i32) -> i32 {
                    let i = h(one);
                    let j = h(two);
                    let k = h2(i, j);
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
                expected <- showCode "Expected:"
                    [sourceFile|
                      use funs::*;

                      fn rec(one: i32, two: i32) -> i32 {
                        let i = h(one);
                        let j = h(two);
                        let k = h2(i, j);
                        if check(k) { rec(i, j) } else { k }
                      }

                      fn test() -> i32 {
                        let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
                        let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
                        let (one_0_1_0_tx, one_0_1_0_rx) = std::sync::mpsc::channel();
                        let (two_0_1_0_tx, two_0_1_0_rx) = std::sync::mpsc::channel();
                        let (ctrl_0_0_0_tx, ctrl_0_0_0_rx) = std::sync::mpsc::channel();
                        let (j_0_0_0_tx, j_0_0_0_rx) = std::sync::mpsc::channel();
                        let (i_0_0_0_tx, i_0_0_0_rx) = std::sync::mpsc::channel();
                        let (k_0_0_0_tx, k_0_0_0_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
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
                            loop {
                              let renew = false;
                              let two_0_0_0_0 = two_0_1_0_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_0_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = two_0_0_0_0;
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
                            let ctrlSig = (true, 1);
                            ctrl_0_0_0_tx.send(ctrlSig)?;
                            let init_0 = 2;
                            let init_1 = 4;
                            one_0_1_0_tx.send(init_0)?;
                            two_0_1_0_tx.send(init_1)?;
                            while a_0_0_rx.recv()? {
                              k_0_0_0_rx.recv()?;
                              let ctrlSig = (true, 1);
                              ctrl_0_0_0_tx.send(ctrlSig)?;
                              let loop_res_0 = i_0_0_0_rx.recv()?;
                              let loop_res_1 = j_0_0_0_rx.recv()?;
                              one_0_1_0_tx.send(loop_res_0)?;
                              two_0_1_0_tx.send(loop_res_1)?;
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
                              let one_0_0_0_0 = one_0_1_0_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_0_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = one_0_0_0_0;
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
                        run(tasks);
                        c_0_0_rx.recv()?
                      }
                    |]
                compiled `shouldBe` expected)
        it "contexted function" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one: i32) -> i32 {
                    let i = h(one);
                    let j = f();
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
                        let j = f();
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
                              let one_0_0_0_0 = one_0_1_0_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_0_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = one_0_0_0_0;
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
                              let renew = false;
                              let lit_unit_0_0_0 = ();
                              while !renew {
                                let sig = ctrl_0_0_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = lit_unit_0_0_0;
                                  let j_0_0_0 = f(var_0);
                                  j_0_0_0_tx.send(j_0_0_0)?
                                };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                              }
                            }
                          }));
                        run(tasks);
                        c_0_0_rx.recv()?
                      }
                    |]
                compiled `shouldBe` expected)
