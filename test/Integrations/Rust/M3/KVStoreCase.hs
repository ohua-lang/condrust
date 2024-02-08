{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.KVStoreCase where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.M3.Setup
import Integrations.Rust.M3.TestCode.KVAppCode as Input
import Integrations.Rust.M3.TestCode.KVAppCompiled as Expect

spec :: Spec
spec =
    describe "Syntax structures we need" $ do
        it "Required: States in Branches" $ 
            (showCode "Compiled: " =<< compileCode 
                [sourceFile|
                    use funs::*;

                    pub fn main() -> i32 {
                        let obj1 = State::new(42);
                        let obj2 = std::sync::Arc::new(23);
                        let condition: bool = check(23);
                        let result = if condition {obj1.get_num()} else  {obj2.deref()};
                        result
                    }
                    |]
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.stateInBranches 
                compiled `shouldBe` expected)
        
        it "Required: States in Loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug 
                [sourceFile|
                    use funs::*;

                    pub fn main() {
                        let obj1 = State::new(42);
                        let obj2 = std::sync::Arc::new(23);
                        loop {
                            let x = obj2.deref();
                            obj1.modify(x);
                        }
                    }
                    |]            
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)

        {- -- That's probably not required for now.
        it "Required: States in Loop - Assign loop result" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug
                [sourceFile|
                    use funs::*;

                    pub fn main() {
                        let obj1: Something = Something::new();
                        let obj2: OtherThing = Some::thingelse();
                        let result:() = loop {
                            let x: Sometype = obj1.calculate();
                            obj2.react(x)
                        };
                        result
                    }
                    |]            
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)
        -}

        it "Required: States in Loop - Return Unit from loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug
                [sourceFile|
                    use funs::*;

                    pub fn main() {
                        let obj1 = State::new(42);
                        let obj2 = std::sync::Arc::new(23);
                        let result:() = loop {
                            let x = obj2.deref();
                            obj1.modify(x);
                        };
                        result
                    }
                    |]            
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)

        -- Actually we need stateful functions in branches in loops but this is an intermediate step.
        it "Required: Branches in Loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRec 
                [sourceFile|
                    use funs::*;

                    pub fn main(){
                        let obj1  = State::new(17); 
                        loop {
                            let time: u64 = get_time();
                            let current = if check_time(time) {iffun(time)} else {elsefun(time)};
                            obj1.modify(current);
                        }
                    }
                |]) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)

        it "Required: States in Branches in Loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRec 
                [sourceFile|
                    use funs::*;

                    pub fn main() {
                        let obj1 = State::new(23); 
                        let obj2 = State::new(42);
                        let obj3 = State::new(0);
                        loop {
                            let next = somefun();
                            let current = if check(next) {obj1.get_num()} else {obj2.get_num()};
                            obj3.modify(current);
                        }
                    }
                    |]          
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)

        it "Actual Application" $
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug Input.kvApplication) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kvApplication
                compiled `shouldBe` expected)

