{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.State where

import Ohua.Prelude

import Integrations.Rust.Utils

spec :: Spec
spec = 
    describe "State" $ -- do
        it "simple" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                fn test(i: i32) -> String {
                    let state = f(i);
                    let result = state.g(5);
                    h(result)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        fn test(i: i32) -> String {
                            let (a_0_tx, a_0_rx) = std::sync::mpsc::channel();
                            let (state_0_0_tx, state_0_0_rx) = std::sync::mpsc::channel();
                            let (result_0_0_tx, result_0_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                loop {
                                    let var_0 = result_0_0_rx.recv();
                                    let result = h(var_0);
                                    a_0_tx.send(result)
                                }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                loop {
                                    let var_0 = state_0_0_rx.recv();
                                    let var_1 = 5;
                                    let result = var_0.g(var_1);
                                    result_0_0_tx.send(result);
                                    ()
                                }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let var_0 = i;
                                let result = f(var_0);
                                state_0_0_tx.send(result)
                                }));
                            run(tasks);
                            a_0_rx.recv()
                        }
                    |]
                compiled `shouldBe` expected)
        -- currently fails
        -- it "state thread" $
        --     (showCode "Compiled: " =<< compileCode [sourceFile| 
        --         fn test(i: i32) -> String {
        --             let state = f(i);
        --             let r0 = state.g(5);
        --             let r1 = state.h(6);
        --             r1
        --         }
        --         |]) >>= 
        --     (\compiled -> do
        --         expected <- showCode "Expected:"
        --             [sourceFile| 
        --                 fn test(i: i32) -> String {
        --                     let state_0_0 = std::sync::mpsc::channel();
        --                     let r1_0_0 = std::sync::mpsc::channel();
        --                     let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
        --                     tasks
        --                         .push(Box::new(move || -> _ {
        --                             let var_0 = state_0_0_rx.recv();
        --                             let var_1 = 5;
        --                             let _result = var_0.g(var_1);
        --                             let var_2 = 6;
        --                             let result = var_0.h(var_2);
        --                             r1_0_0_tx.send(result);
        --                             ()
        --                         }));
        --                     tasks
        --                         .push(Box::new(move || -> _ {
        --                             let var_0 = i;
        --                             let result = f(var_0);
        --                             state_0_0_tx.send(result)
        --                         }));
        --                     run(tasks);
        --                     r1_0_0_rx.recv()
        --                 }
        --             |]
        --         compiled `shouldBe` expected)
