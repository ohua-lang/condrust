{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.Basics where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.M3.Setup
import Integrations.Rust.M3.TestCode.BasicsOutput as Expect

spec :: Spec
spec =
    describe "Basic Constructs" $ do
        it "hello world" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::hello_world;

                    fn test() -> String {
                        hello_world()
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.helloWorld
                compiled `shouldBe` expected)
 
        it "simple composition" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::{f, g};

                    fn test() -> String {
                        let x:i32 = f();
                        g(x)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.simpleComposition
                compiled `shouldBe` expected)

        -- FIXME see issue ohua-lang/ohua-frontend#8
        -- We need this to error because the output produces is just wrong i.e. channels will be used twice.
        it "function with two calculated params" $ 
            compileCode [sourceFile|
                    use crate::funs::*;

                    fn test() -> String {
                        let x:i32 = f();
                        let y:i32 = h(x);
                        h2(x,y)
                    }
                |] `shouldThrow` anyException

        -- due to the transformation to state threads, this transforms into:
          -- let x = f();
          -- let x1 = Arc::new();
          -- let (x2,x1') = x1.arc_clone();
          -- let y = h(x1');
          -- h2(x2,y)
          -- where no variable is used more than once!
        it "Arc and clone local var" $ 
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                    use crate::funs::*;

                    fn test() -> String {
                        let x:i32 = f();
                        let x1:Arc<i32> = std::sync::Arc::new(x);
                        let x2:Arc<i32> = x1.clone();
                        let y:i32 = h(x1);
                        h2(x2,y)
                    }
                |]  ) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.arcAndClone
                compiled `shouldBe` expected)

        -- due to the transformation to state threads, this transforms into:
          -- let x = f();
          -- let (x1,x') = x.clone();
          -- let y = h(x');
          -- h2(x1,y)
          -- where no variable is used more than once!
        it "Clone local var" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test() -> String {
                        let x:String = f();
                        let x1:String = x.clone();
                        let y:String = h(x);
                        h2(x1,y)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.cloneVar
                compiled `shouldBe` expected)

        it "Use imported function" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test(i: i32) -> String {
                        let x:i32 = funs::h(i);
                        funs::g(x)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.useFun
                compiled `shouldBe` expected)

        it "Use algo in other algo" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn algo(i: i32) -> String {
                        let x:i32 = h(i);
                        g(x)
                    }

                    fn test() -> String {
                        algo(4)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.inlineAlgo
                compiled `shouldBe` expected)

        it "Use algo with imported function in other algo" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn algo(i: i32) -> String {
                        let x:i32 = funs::h(i);
                        funs::g(x)
                    }

                    fn test() -> String {
                        algo(4)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algoLoadingEnv
                compiled `shouldBe` expected)

        it "Tuple destruct from unit fun" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test() -> i32 {
                        let (x0,y0):(i32,String) = f_tup();
                        let x1:i32 = f0(x0);
                        let y1:String = f1(y0);
                        h2(x1,y1)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tupleFromUnit
                compiled `shouldBe` expected)
                
        it "Tuple destruct from param fun" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test() -> i32 {
                        let (x0,y0):(i32,String) = f_tup(23);
                        let x1:i32 = f0(x0);
                        let y1:String = f1(y0);
                        h2(x1,y1)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tupleFromParam
                compiled `shouldBe` expected)     
        
        -- FIXME: This case fails for more than one reason. Obviously, the reuase of i is one of them. However it also has a problem we
        --  really need to adress. Namely that i as a parameter of the compiled "outermost" algo becomes an "EnvVariable" and we need to fix passing "EnvVariables" for M3
        --  Right now they are, just as in the SharedMemory Backend treaded as just being available i.e.
        --          i_0_0_0_child_tx.send(i)?;
        -- will just happen in the code without receiving i from somewhere.
        it "FAIL: While Loop as If-Recursion only recursive call in branches" $ 
            compileCodeWithRec  [sourceFile|
                    use crate::funs::*;

                    fn algo_rec(i:i32, state:State) -> State{
                        state.gs(i);
                        let i_new:i32 = add(i, 1);
                        if islowerthan23(i) {
                            algo_rec(i_new, state)
                        } else {
                            state
                        }
                    }

                    fn test(i:i32) -> State {
                        let mut state:State = State::new_state();
                        algo_rec(i, state) 
                    }
                |] `shouldThrow` anyException


        it "if condition with binop" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test(i:i32) -> i32{
                        if i < 13 {
                            i
                        } else {
                            i+1
                        }
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.conditionBinOp
                compiled `shouldBe` expected)

        it "Smap/For-Loop bound range" $
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test() -> State {
                        let s:State = State::new_state();
                        let stream:Vec<i32> = iter_i32();
                        for e in stream {
                            let e1:i32 = e;
                            let r:i32 = h(e1);
                            s.gs(r);
                        }
                        s
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.smapBound
                compiled `shouldBe` expected)
   
        it "Smap/For-Loop on unbound range" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;
                    
                    fn test(i:i32) -> () {
                        let s:State = State::new_state();
                        for e in range_from(i) {
                            let e1:i32 = e;
                            let r:i32 = h(e1);
                            s.gs(r);
                        }
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.smapUnbound
                compiled `shouldBe` expected)
                
{-
        it "While Loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug 
                [sourceFile|
                    use crate::funs::*;

                    fn test() -> I32 {
                        //let state:State = State::new_state();
                        let mut i:i32 = I32::new(1);
                        while i.islowerthan23() {
                            //let e:i32
                            //state.gs(i);
                            // i = i + 1 is actually a stateful function acting on a named memory slot
                            i.add(1); 
                        }
                        i
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.hello_world
                compiled `shouldBe` expected)
       -}         
