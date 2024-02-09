{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.Basics where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

import Ohua.Commons.Prelude ( ($), Monad((>>=)), (=<<) )

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
                    use crate::funs::{f, g, from_int};

                    fn test() -> String {
                        let x:i32 = f();
                        from_int(x)
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

                    fn test() -> i32 {
                        let x:i32 = f();
                        let x1:Arc<i32> = std::sync::Arc::new(x);
                        let x2:Arc<i32> = x1.clone();
                        let y:i32 = h_Arc(x1);
                        y
                    }
                |]  ) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.arcAndClone
                compiled `shouldBe` expected)

        it "Struct Namespace: distinguish clones" $
          -- to check that the type system can distiguish namespaces of the clone function
          -- and to check that use_arc is found in the impl space of State, not as a pure function from 
          -- top-level definitions 
          (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x1:Arc<i32> = std::sync::Arc::new(1);
                    let x2:State = State::new(2);
                    let y1 = x1.clone();
                    let y2 = x2.clone();
                    y2.use_arc(y1)                    
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.cloneVar
                compiled `shouldBe` expected)

        it "Use imported function" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test(i: i32) -> String {
                        let x:i32 = h(i);
                        from_int(x)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.useFun
                compiled `shouldBe` expected)

        it "Use algo in other algo" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs;

                    fn algo(i: i32, j:char) -> String {
                        let x:i32 = funs::h(i);
                        funs::take_char_i(j, x)
                    }

                    fn test() -> String {
                        let c = funs::make_char();
                        let i = funs::somefun();
                        algo(i, c)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.inlineAlgo
                compiled `shouldBe` expected)

        it "Use algo with imported function in other algo" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn algo(i: i32) -> String {
                        let x:i32 = h(i);
                        g()
                    }

                    fn test() -> String {
                        let inp = some_int();
                        algo(inp)
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algoLoadingEnv
                compiled `shouldBe` expected)

        it "Tuple destruct from unit fun" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test() -> i32 {
                        let (x0,y0):(i32, i32) = fu_tup();
                        let x1:i32 = f0(x0);
                        let y1:i32 = f1(y0);
                        h4(x1,y1)
                    }
                    
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tupleFromUnit
                compiled `shouldBe` expected)
       
        it "Tuple destruct from param fun" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test(i:i32) -> i32 {
                        let (x0 ,y0):(i32, i32) = fi_tup(i);
                        let x1:i32 = f0(x0);
                        let y1:i32 = f1(y0);
                        h4(x1,y1)
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
            (showCode "Compiled: " =<< compileCodeWithRec  [sourceFile|
                    use crate::funs::*;

                    fn algo_rec(i:i32, state:State) -> State{
                        state.gs(i);
                        let i_new:i32 = h4(i, 1);
                        if islowerthan23(i) {
                            algo_rec(i_new, state)
                        } else {
                            state
                        }
                    }

                    fn test(i:i32) -> State {
                        let mut state:State = State::new_state(12);
                        algo_rec(i, state) 
                    }
                |]) >>= 
                (\compiled -> do 
                    expected <- showCode "Expected:" Expect.whileAsIf
                    compiled `shouldBe` expected) 


        it "if condition" $ 
            (showCode "Compiled: " =<< compileCode [sourceFile|
                    use crate::funs::*;

                    fn test(i:i32) -> i32{
                        if check(13) {
                            i
                        } else {
                            h4(i,1)
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
                        let s:State = State::new_state(2);
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
                        let s:State = State::new_state(3);
                        for e in iter_i32() {
                            let e1:i32 = e;
                            let r:i32 = h(e1);
                            s.gs(r);
                        }
                    }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.smapUnbound
                compiled `shouldBe` expected)
     