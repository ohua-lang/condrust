{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.State where

import Ohua.Prelude  ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils

spec :: Spec
spec =
  describe "State" $ do
    describe "flat" ( do
      it "simple" $
        (showCode "Compiled: " =<< compileCode [sourceFile|
          use funs::*;

          fn test(i: i32) -> i32 {
            let mut state = S::new(i);
            let result = state.gs(5);
            h(result)
          }
          |]) >>=
        (\compiled -> do
          expected <- showCode "Expected:"
            [sourceFile|
              use funs::*;

              fn test(i: i32) -> i32 {
                let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                let (state_0_1_tx, state_0_1_rx) = std::sync::mpsc::channel();
                let (result_0_0_tx, result_0_0_rx) = std::sync::mpsc::channel();
                let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                tasks
                  .push(Box::new(move || -> _ {
                  loop {
                    let var_0 = result_0_0_rx.recv()?;
                    let a_0 = h(var_0);
                    a_0_tx.send(a_0)?
                  }
                  }));
                tasks
                  .push(Box::new(move || -> _ {
                  loop {
                    let mut var_0 = state_0_1_rx.recv()?;
                    let var_1 = 5;
                    let result_0_0 = var_0.gs(var_1);
                    result_0_0_tx.send(result_0_0)?;
                    ()
                  }
                  }));
                tasks
                  .push(Box::new(move || -> _ {
                    let var_0 = i;
                    let state_0_1 = S::new(var_0);
                    state_0_1_tx.send(state_0_1)?
                  }));
                run(tasks);
                a_0_rx.recv()?
              }
            |]
          compiled `shouldBe` expected)
      it "thread" $
        (showCode "Compiled: " =<< compileCode [sourceFile|
          use funs::*;

          fn test(i: i32) -> String {
            let state = S::new(i);
            let r0 = state.gs(5);
            let r1 = state.gs(6);
            r1
          }
          |]) >>=
        (\compiled -> do
          expected <- showCode "Expected:"
            [sourceFile|
              use funs::*;

              fn test(i: i32) -> String {
                let (r1_0_0_tx, r1_0_0_rx) = std::sync::mpsc::channel();
                let (state_0_2_tx, state_0_2_rx) = std::sync::mpsc::channel();
                let (state_0_1_0_tx, state_0_1_0_rx) = std::sync::mpsc::channel();
                let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                tasks
                  .push(Box::new(move || -> _ {
                    loop {
                      let var_0 = state_0_1_0_rx.recv()?;
                      let var_1 = 6;
                      let r1_0_0 = var_0.gs(var_1);
                      r1_0_0_tx.send(r1_0_0)?;
                      ()
                    }
                  }));
                tasks
                  .push(Box::new(move || -> _ {
                    loop {
                      let var_0 = state_0_2_rx.recv()?;
                      let var_1 = 5;
                      let r0_0_0 = var_0.gs(var_1);
                      r0_0_0_tx.send(r0_0_0)?;
                      state_0_1_0_tx.send(var_0)?;
                      ()
                    }
                  }));
                tasks
                  .push(Box::new(move || -> _ {
                    let var_0 = i;
                    let state_0_2 = S::new(var_0);
                    state_0_2_tx.send(state_0_2)?
                  }));
                run(tasks);
                r1_0_0_rx.recv()?
              }
              |]
          compiled `shouldBe` expected)
         )
    describe "loop" ( do
        it "deep state simple" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                 fn test(i:i32) -> () {
                    let stream = iter();
                    for e in stream {
                        e.gs(5);
                    }
                }
                |]) >>=
            (\compiled -> do
                -- FIXME sertel/ohua-core#12
                -- FIXME sertel/ohua-core#11
                expected <- showCode "Expected:"
                    [sourceFile|
                    use funs::*;

                    fn test(i: i32) -> () {
                      let (b_0_tx, b_0_rx) = std::sync::mpsc::channel();
                      let (stream_0_0_tx, stream_0_0_rx) = std::sync::mpsc::channel();
                      let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel();
                      let (d_0_tx, d_0_rx) = std::sync::mpsc::channel();
                      let (c_0_tx, c_0_rx) = std::sync::mpsc::channel();
                      let (size_0_tx, size_0_rx) = std::sync::mpsc::channel();
                      let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                      tasks
                        .push(Box::new(move || -> _ {
                          loop {
                            let renew = false;
                            let lit_unit_0 = ();
                            while !renew {
                              let sig = ctrl_0_rx.recv()?;
                              let count = sig.1;
                              for _ in 0 .. count {
                                let _var_0 = lit_unit_0;
                                let c_0 = ohua::lang::id(); // FIXME sertel/ohua-core#11
                                c_0_tx.send(c_0)?
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
                            let num = size_0_rx.recv()?;
                            let collection = Vec::new();
                            for _ in 0 .. num { let data = c_0_rx.recv()?; collection.push(data) };
                            b_0_tx.send(collection)?
                          }
                        }));
                      tasks
                        .push(Box::new(move || -> _ {
                          loop {
                            let var_0 = d_0_rx.recv()?;
                            let var_1 = 5;
                            let __0_0 = var_0.gs(var_1);
                            __0_0_tx.send(__0_0)?; // FIXME sertel/ohua-core#11
                            ()
                          }
                        }));
                      tasks
                        .push(Box::new(move || -> _ {
                          loop {
                            let data = stream_0_0_rx.recv()?;
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
                              ()
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
                          let stream_0_0 = iter();
                          stream_0_0_tx.send(stream_0_0)?
                        }));
                      run(tasks);
                      b_0_rx.recv()?
                    }
                    |]
                compiled `shouldBe` expected)
        it "single io" $
            -- FIXME sertel/ohua-core#11
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                 fn test(i:i32) -> () {
                    let io = S::new(i);
                    let stream = iter_i32();
                    for e in stream {
                        io.gs(e);
                    }
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                      use funs::*;

                      fn test(i: i32) -> () {
                        let (b_0_tx, b_0_rx) = std::sync::mpsc::channel();
                        let (stream_0_0_tx, stream_0_0_rx) = std::sync::mpsc::channel();
                        let (io_0_1_tx, io_0_1_rx) = std::sync::mpsc::channel();
                        let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel();
                        let (d_1_tx, d_1_rx) = std::sync::mpsc::channel();
                        let (c_0_tx, c_0_rx) = std::sync::mpsc::channel();
                        let (size_0_tx, size_0_rx) = std::sync::mpsc::channel();
                        let (x_0_0_tx, x_0_0_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let data = stream_0_0_rx.recv()?;
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
                                ()
                              } else {
                                let size = 0;
                                for d in data {
                                  d_1_tx.send(d)?;
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
                            let stream_0_0 = iter_i32();
                            stream_0_0_tx.send(stream_0_0)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            x_0_0_rx.recv()?;
                            let x = ();
                            b_0_tx.send(x)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let renew = false;
                              let lit_unit_0 = ();
                              while !renew {
                                let sig = ctrl_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count { let var_0 = lit_unit_0; c_0_tx.send(c_0)? };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                              }
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let num = size_0_rx.recv()?;
                              let collection = Vec::new();
                              for _ in 0 .. num { let data = c_0_rx.recv()?; collection.push(data) };
                              x_0_0_tx.send(collection)?
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let io_0_1 = S::new(var_0);
                            io_0_1_tx.send(io_0_1)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let renew = false;
                              let io_0_1_0 = io_0_1_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = io_0_1_0;
                                  let var_1 = d_1_rx.recv()?;
                                  let __0_0 = var_0.gs(var_1);
                                  __0_0_tx.send(__0_0)?;
                                  ()
                                };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                              };
                              io_0_2_tx.send(io_0_1_0)?;
                              ()
                            }
                          }));
                        run(tasks);
                        b_0_rx.recv()?
                      }
                   |]
                compiled `shouldBe` expected)
        it "single state" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                 fn test(i:i32) -> () {
                    let s = S::new(i);
                    let stream = iter_i32();
                    for e in stream {
                        s.gs(e);
                    }
                    s.gs(5)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                      use funs::*;

                      fn test(i: i32) -> () {
                        let (b_0_tx, b_0_rx) = std::sync::mpsc::channel();
                        let (stream_0_0_tx, stream_0_0_rx) = std::sync::mpsc::channel();
                        let (s_0_2_tx, s_0_2_rx) = std::sync::mpsc::channel();
                        let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel();
                        let (d_1_tx, d_1_rx) = std::sync::mpsc::channel();
                        let (c_0_tx, c_0_rx) = std::sync::mpsc::channel();
                        let (size_0_tx, size_0_rx) = std::sync::mpsc::channel();
                        let (s_0_3_tx, s_0_3_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                          .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let s_0_2 = S::new(var_0);
                            s_0_2_tx.send(s_0_2)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let data = stream_0_0_rx.recv()?;
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
                                ()
                              } else {
                                let size = 0;
                                for d in data {
                                  d_1_tx.send(d)?;
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
                            let stream_0_0 = iter_i32();
                            stream_0_0_tx.send(stream_0_0)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let num = size_0_rx.recv()?;
                              let collection = Vec::new();
                              for _ in 0 .. num { let data = c_0_rx.recv()?; collection.push(data) };
                              __0_0_tx.send(collection)?
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let var_0 = s_0_3_rx.recv()?;
                              let var_1 = 5;
                              let b_0 = var_0.gs(var_1);
                              b_0_tx.send(b_0)?;
                              ()
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let renew = false;
                              let lit_unit_0 = ();
                              while !renew {
                                let sig = ctrl_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let _var_0 = lit_unit_0;
                                  c_0_tx.send(c_0)?
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
                              let renew = false;
                              let s_0_2_0 = s_0_2_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = s_0_2_0;
                                  let var_1 = d_1_rx.recv()?;
                                  let __1_0 = var_0.gs(var_1);
                                  __1_0_tx.send(__1_0)?;
                                  ()
                                };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                              };
                              s_0_3_tx.send(s_0_2_0)?;
                              ()
                            }
                          }));
                        run(tasks);
                        b_0_rx.recv()?
                      }
                    |]
                compiled `shouldBe` expected)
         )
    describe "combo" (do
        it "thread + loop" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                 fn test(i:i32) -> () {
                    let s = S::new(i);
                    let sp = s.clone();
                    let stream = iter_i32();
                    for e in stream {
                        let x = f_s(sp,e);
                        s.gs(x);
                    }
                    s.gs(5)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                      use funs::*;

                      fn test(i: i32) -> () {
                        let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel();
                        let (s_0_0_2_tx, s_0_0_2_rx) = std::sync::mpsc::channel();
                        let (stream_0_0_0_tx, stream_0_0_0_rx) = std::sync::mpsc::channel();
                        let (s_0_0_1_0_tx, s_0_0_1_0_rx) = std::sync::mpsc::channel();
                        let (sp_0_0_0_tx, sp_0_0_0_rx) = std::sync::mpsc::channel();
                        let (ctrl_0_tx, ctrl_0_rx) = std::sync::mpsc::channel();
                        let (d_1_tx, d_1_rx) = std::sync::mpsc::channel();
                        let (x_0_0_0_tx, x_0_0_0_rx) = std::sync::mpsc::channel();
                        let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
                        let (size_0_tx, size_0_rx) = std::sync::mpsc::channel();
                        let (s_0_1_1_tx, s_0_1_1_rx) = std::sync::mpsc::channel();
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                          .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let s_0_0_2 = S::new(var_0);
                            s_0_0_2_tx.send(s_0_0_2)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let var_0 = s_0_0_2_rx.recv()?;
                              let sp_0_0_0 = var_0.clone();
                              sp_0_0_0_tx.send(sp_0_0_0)?;
                              s_0_0_1_0_tx.send(var_0)?;
                              ()
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let num = size_0_rx.recv()?;
                              let collection = Vec::new();
                              for _ in 0 .. num {
                                let data = c_0_0_rx.recv()?;
                                collection.push(data)
                              };
                              __0_0_0_tx.send(collection)?
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let var_0 = s_0_1_1_rx.recv()?;
                              let var_1 = 5;
                              let b_0_0 = var_0.gs(var_1);
                              b_0_0_tx.send(b_0_0)?;
                              ()
                            }
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let data = stream_0_0_0_rx.recv()?;
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
                                ()
                              } else {
                                let size = 0;
                                for d in data {
                                  d_1_tx.send(d)?;
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
                        tasks
                          .push(Box::new(move || -> _ {
                            let stream_0_0_0 = iter_i32();
                            stream_0_0_0_tx.send(stream_0_0_0)?
                          }));
                        tasks
                          .push(Box::new(move || -> _ {
                            loop {
                              let renew = false;
                              let sp_0_0_0_0 = sp_0_0_0_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = sp_0_0_0_0;
                                  let var_1 = d_1_rx.recv()?;
                                  let x_0_0_0 = f_s(var_0, var_1);
                                  x_0_0_0_tx.send(x_0_0_0)?
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
                              let s_0_0_1_0_0 = s_0_0_1_0_rx.recv()?;
                              while !renew {
                                let sig = ctrl_0_rx.recv()?;
                                let count = sig.1;
                                for _ in 0 .. count {
                                  let var_0 = s_0_0_1_0_0;
                                  let var_1 = x_0_0_0_rx.recv()?;
                                  let __1_0_0 = var_0.gs(var_1);
                                  __1_0_0_tx.send(__1_0_0)?;
                                  ()
                                };
                                let renew_next_time = sig.0;
                                renew = renew_next_time;
                                ()
                              };
                              s_0_1_1_tx.send(s_0_0_1_0_0)?;
                              ()
                            }
                          }));
                        run(tasks);
                        b_0_0_rx.recv()?
                    }
                    |]
                compiled `shouldBe` expected)
        )
