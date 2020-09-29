{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.If where

import Ohua.Prelude

import Integrations.Rust.Utils


spec :: Spec
spec = 
    describe "Conditionals" $ do
        it "simple condition" $ -- in most languages, a condition is not a function!
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                fn test(i: i32) -> String {
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
                        fn test(i: i32) -> String {
                        let a_0_0 = ohua::arcs::Channel::new(2);
                        let b_0_0 = ohua::arcs::Channel::new(1);
                        let c_0_0 = ohua::arcs::Channel::new(1);
                        let ctrlTrue_0 = ohua::arcs::Channel::new(1);
                        let ctrlFalse_0 = ohua::arcs::Channel::new(1);
                        let e_0 = ohua::arcs::Channel::new(1);
                        let f_0 = ohua::arcs::Channel::new(1);
                        let result_0 = ohua::arcs::Channel::new(1);
                        let g_0 = ohua::arcs::Channel::new(1);
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let renew = false;
                                let c_0_0_0 = c_0_0.recv(0);
                                while !renew {
                                let sig = ctrlFalse_0.recv(0);
                                let count = sig.1;
                                for _ in [0; count] {
                                    let var_0 = c_0_0_0;
                                    let result = g1(var_0);
                                    f_0.send(result)
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
                                let b_0_0_0 = b_0_0.recv(0);
                                while !renew {
                                let sig = ctrlTrue_0.recv(0);
                                let count = sig.1;
                                for _ in [0; count] {
                                    let var_0 = b_0_0_0;
                                    let result = g0(var_0);
                                    e_0.send(result)
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
                                let var_0 = result_0.recv(0);
                                let result = h(var_0);
                                g_0.send(result)
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let branchSelection = a_0_0.recv(0);
                                if branchSelection {
                                let result = e_0.recv(0);
                                result_0.send(result)
                                } else { let result = f_0.recv(0); result_0.send(result) }
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let branchSelection = a_0_0.recv(0);
                                if branchSelection {
                                let ctrlTrue = (true, 1);
                                let ctrlFalse = (true, 0);
                                ctrlTrue_0.send(ctrlTrue);
                                ctrlFalse_0.send(ctrlFalse)
                                } else {
                                let ctrlTrue = (true, 0);
                                let ctrlFalse = (true, 1);
                                ctrlTrue_0.send(ctrlTrue);
                                ctrlFalse_0.send(ctrlFalse)
                                }
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let result = f2(var_0);
                            c_0_0.send(result)
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let result = f1(var_0);
                            b_0_0.send(result)
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let result = f0(var_0);
                            a_0_0.send(result)
                            }));
                        run(tasks);
                        g_0.recv(0)
                        }                    
                    |]
                compiled `shouldBe` expected)
        it "context functions" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                fn test(i: i32) -> String {
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
                        fn test(i: i32) -> String {
                        let a_0_0 = ohua::arcs::Channel::new(2);
                        let ctrlTrue_0 = ohua::arcs::Channel::new(1);
                        let ctrlFalse_0 = ohua::arcs::Channel::new(1);
                        let b_0 = ohua::arcs::Channel::new(1);
                        let c_0 = ohua::arcs::Channel::new(1);
                        let result_0 = ohua::arcs::Channel::new(1);
                        let e_0 = ohua::arcs::Channel::new(1);
                        let mut tasks: Vec<Box<FnOnce() -> Result<(), RunError> + Send>> = Vec::new();
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let renew = false;
                                let lit_unit_0 = ();
                                while !renew {
                                    let sig = ctrlFalse_0.recv(0);
                                    let count = sig.1;
                                    for _ in [0; count] {
                                        let _var_0 = lit_unit_0;
                                        let result = g1();
                                        c_0.send(result)
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
                                    let sig = ctrlTrue_0.recv(0);
                                    let count = sig.1;
                                    for _ in [0; count] {
                                        let var_0 = lit_5_0;
                                        let result = g0(var_0);
                                        b_0.send(result)
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
                                let var_0 = result_0.recv(0);
                                let result = h(var_0);
                                e_0.send(result)
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let branchSelection = a_0_0.recv(0);
                                if branchSelection {
                                let result = b_0.recv(0);
                                result_0.send(result)
                                } else { let result = c_0.recv(0); result_0.send(result) }
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            loop {
                                let branchSelection = a_0_0.recv(0);
                                if branchSelection {
                                let ctrlTrue = (true, 1);
                                let ctrlFalse = (true, 0);
                                ctrlTrue_0.send(ctrlTrue);
                                ctrlFalse_0.send(ctrlFalse)
                                } else {
                                let ctrlTrue = (true, 0);
                                let ctrlFalse = (true, 1);
                                ctrlTrue_0.send(ctrlTrue);
                                ctrlFalse_0.send(ctrlFalse)
                                }
                            }
                            }));
                        tasks
                            .push(Box::new(move || -> _ {
                            let var_0 = i;
                            let result = f0(var_0);
                            a_0_0.send(result)
                            }));
                        run(tasks);
                        e_0.recv(0)
                        }                    
                    |]
                compiled `shouldBe` expected)

