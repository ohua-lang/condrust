{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.If where

import Ohua.Prelude

import Integrations.Rust.Utils


spec :: Spec
spec = 
    describe "Conditionals" $ do
        it "simple condition" $ -- in most languages, a condition is not a function!
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                use funs::*;

                fn test(i: i32) -> i32 {
                    let a = f0(i);
                    let b = f1(i);
                    let c = f2(i);
                    let d = if a {
                        g0(b)
                    } else {
                        g1(c)
                    };
                    h(d)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs::*;

                        fn test(i: i32) -> i32 {
                            let (g_0_tx, g_0_rx) = std::sync::mpsc::channel();
                            let (b_0_0_tx, b_0_0_rx) = std::sync::mpsc::channel();
                            let (ctrlTrue_0_tx, ctrlTrue_0_rx) = std::sync::mpsc::channel();
                            let (c_0_0_tx, c_0_0_rx) = std::sync::mpsc::channel();
                            let (ctrlFalse_0_tx, ctrlFalse_0_rx) = std::sync::mpsc::channel();
                            let (f_0_tx, f_0_rx) = std::sync::mpsc::channel();
                            let (e_0_tx, e_0_rx) = std::sync::mpsc::channel();
                            let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
                            let (result_0_tx, result_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                loop {
                                    let renew = false;
                                    let c_0_0_0 = c_0_0_rx.recv();
                                    while !renew {
                                    let sig = ctrlFalse_0_rx.recv();
                                    let count = sig.1;
                                    for _ in 0..count {
                                        let var_0 = c_0_0_0;
                                        let f_0 = g1(var_0);
                                        f_0_tx.send(f_0)
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
                                    let b_0_0_0 = b_0_0_rx.recv();
                                    while !renew {
                                    let sig = ctrlTrue_0_rx.recv();
                                    let count = sig.1;
                                    for _ in 0..count {
                                        let var_0 = b_0_0_0;
                                        let e_0 = g0(var_0);
                                        e_0_tx.send(e_0)
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
                                    let var_0 = result_0_rx.recv();
                                    let g_0 = h(var_0);
                                    g_0_tx.send(g_0)
                                }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                loop {
                                    let branchSelection = a_0_0_rx.recv();
                                    if branchSelection {
                                    let result = e_0_rx.recv();
                                    result_0_tx.send(result)
                                    } else { let result = f_0_rx.recv(); result_0_tx.send(result) }
                                }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                loop {
                                    let branchSelection = a_0_0_rx.recv();
                                    if branchSelection {
                                    let ctrlTrue = (true, 1);
                                    let ctrlFalse = (true, 0);
                                    ctrlTrue_0_tx.send(ctrlTrue);
                                    ctrlFalse_0_tx.send(ctrlFalse)
                                    } else {
                                    let ctrlTrue = (true, 0);
                                    let ctrlFalse = (true, 1);
                                    ctrlTrue_0_tx.send(ctrlTrue);
                                    ctrlFalse_0_tx.send(ctrlFalse)
                                    }
                                }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let var_0 = i;
                                let c_0_0 = f2(var_0);
                                c_0_0_tx.send(c_0_0)
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let var_0 = i;
                                let b_0_0 = f1(var_0);
                                b_0_0_tx.send(b_0_0)
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let var_0 = i;
                                let a_0_0 = f0(var_0);
                                a_0_0_tx.send(a_0_0)
                                }));
                            run(tasks);
                            g_0_rx.recv()
                        }                    
                    |]
                compiled `shouldBe` expected)
        it "context functions" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                use funs::*;

                fn test(i: i32) -> i32 {
                    let a = f0(i);
                    let d = if a {
                        g0(5)
                    } else {
                        g1()
                    };
                    h(d)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs::*;
                        
                        fn test(i: i32) -> i32 {
                            let (e_0_tx, e_0_rx) = std::sync::mpsc::channel();
                            let (ctrlTrue_0_tx, ctrlTrue_0_rx) = std::sync::mpsc::channel();
                            let (ctrlFalse_0_tx, ctrlFalse_0_rx) = std::sync::mpsc::channel();
                            let (c_0_tx, c_0_rx) = std::sync::mpsc::channel();
                            let (b_0_tx, b_0_rx) = std::sync::mpsc::channel();
                            let (a_0_0_tx, a_0_0_rx) = std::sync::mpsc::channel();
                            let (result_0_tx, result_0_rx) = std::sync::mpsc::channel();
                            let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                            tasks
                                .push(Box::new(move || -> _ {
                                loop {
                                    let renew = false;
                                    let lit_unit_0 = ();
                                    while !renew {
                                        let sig = ctrlFalse_0_rx.recv();
                                        let count = sig.1;
                                        for _ in 0..count {
                                            let _var_0 = lit_unit_0;
                                            let c_0 = g1();
                                            c_0_tx.send(c_0)
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
                                    let lit_5_0 = 5;
                                    while !renew {
                                        let sig = ctrlTrue_0_rx.recv();
                                        let count = sig.1;
                                        for _ in 0..count {
                                            let var_0 = lit_5_0;
                                            let b_0 = g0(var_0);
                                            b_0_tx.send(b_0)
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
                                    let var_0 = result_0_rx.recv();
                                    let e_0 = h(var_0);
                                    e_0_tx.send(e_0)
                                }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                loop {
                                    let branchSelection = a_0_0_rx.recv();
                                    if branchSelection {
                                    let result = b_0_rx.recv();
                                    result_0_tx.send(result)
                                    } else { let result = c_0_rx.recv(); result_0_tx.send(result) }
                                }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                loop {
                                    let branchSelection = a_0_0_rx.recv();
                                    if branchSelection {
                                    let ctrlTrue = (true, 1);
                                    let ctrlFalse = (true, 0);
                                    ctrlTrue_0_tx.send(ctrlTrue);
                                    ctrlFalse_0_tx.send(ctrlFalse)
                                    } else {
                                    let ctrlTrue = (true, 0);
                                    let ctrlFalse = (true, 1);
                                    ctrlTrue_0_tx.send(ctrlTrue);
                                    ctrlFalse_0_tx.send(ctrlFalse)
                                    }
                                }
                                }));
                            tasks
                                .push(Box::new(move || -> _ {
                                let var_0 = i;
                                let a_0_0 = f0(var_0);
                                a_0_0_tx.send(a_0_0)
                                }));
                            run(tasks);
                            e_0_rx.recv()
                        }                    
                    |]
                compiled `shouldBe` expected)
