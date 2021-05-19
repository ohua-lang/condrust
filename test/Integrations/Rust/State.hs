{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.State where

import Ohua.Prelude  ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils

spec :: Spec
spec =
    describe "State" $ do
        it "simple" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                fn test(i: i32) -> i32 {
                    let state = S::new(i);
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
                                    let var_0 = state_0_1_rx.recv()?;
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
        it "state thread" $
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

--use funs::*;
--
--fn test(i: i32) -> String {
--  let (r1_0_0_tx, r1_0_0_rx) = std::sync::mpsc::channel();
--  let (state_0_0_tx, state_0_0_rx) = std::sync::mpsc::channel();
--  let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
--  tasks
--    .push(Box::new(move || -> _ {
--      loop {
--        let var_0 = state_0_0_rx.recv()?;
--        let var_1 = 6;
--        let r1_0_0 = var_0.gs(var_1);
--        r1_0_0_tx.send(r1_0_0)?;
--        ()
--      }
--    }));
--  tasks
--    .push(Box::new(move || -> _ {
--      loop {
--        let var_0 = state_0_0_rx.recv()?;
--        let var_1 = 5;
--        let r0_0_0 = var_0.gs(var_1);
--        r0_0_0_tx.send(r0_0_0)?;
--        ()
--      }
--    }));
--  tasks
--    .push(Box::new(move || -> _ {
--      let var_0 = i;
--      let state_0_0 = S::new(var_0);
--      state_0_0_tx.send(state_0_0)?
--   }));
--   run(tasks);
--   r1_0_0_rx.recv()?
--}
