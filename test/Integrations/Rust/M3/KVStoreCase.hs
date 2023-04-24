{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.KVStoreCase where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.M3.Setup
import Integrations.Rust.M3.TestCode.KVAppCode as Input
import Integrations.Rust.M3.TestCode.KVAppCompiled as Expect

spec :: Spec
spec =
    describe "Syntax strcutures we need" $ do
        {-
        it "Required: States in Branches" $ 
            (showCode "Compiled: " =<< compileCode 
                [sourceFile|
                    use stuff::*;

                    pub fn main() -> Something {
                        let obj1: Something = Something::new();
                        let obj2: OtherThing = Some::thingelse();
                        let condition: bool = function(23);
                        let result: Something = if condition {obj1.calculate()} else  {obj2.compute()};
                        result
                    }
                    |]
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.stateInBranches 
                compiled `shouldBe` expected)
        -}
        it "Required: States in Loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug 
                [sourceFile|
                    use stuff::*;

                    pub fn main() {
                        let obj1: Something = Something::new();
                        let obj2: OtherThing = Some::thingelse();
                        loop {
                            let x: Sometype = obj1.calculate();
                            obj2.react(x)
                        }
                    }
                    |]            
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)
        it "Required: States in Loop - Assign loop result" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug
                [sourceFile|
                    use stuff::*;

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
        it "Required: States in Loop - Return Unit from loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug
                [sourceFile|
                    use stuff::*;

                    pub fn main() {
                        let obj1: Something = Something::new();
                        let obj2: OtherThing = Some::thingelse();
                        let result:() = loop {
                            let x: Sometype = obj1.calculate();
                            obj2.react(x);
                            ()
                        };
                        result
                    }
                    |]            
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)

    {-
        it "Required: Branches in Loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRec 
                [sourceFile|
                    use stuff::*;

                    pub fn main() -> Something{
                        let obj1: Something = Something::new(); 
                        let result:() = loop {
                            let time: u64 = get_time();
                            let current: u64 = if time < 42 {iffun(time)} else {elsefun(time)};
                            obj1.aggregate(current);
                        }
                    }
                    |]           
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)

        it "Required: States in Branches in Loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRec 
                [sourceFile|
                    use stuff::*;

                    pub fn main() -> Something{
                        let obj1: Something = Something::new(); 
                        let obj2: OtherThing = OtherThing::new();
                        let obj3: Aggregator = Aggregator::default();
                        loop {
                            let next: usize = toss_coin();
                            let current: u64 = if is_heads(next) {obj1.calculate()} else {obj2.compute()};
                            obj3.aggregate(current);
                        }
                    }
                    |]          
            ) >>= 
            (\compiled -> do 
                expected <- showCode "Expected: " Expect.basicsPlaceholder
                compiled `shouldBe` expected)

    describe "Actual Application" $ do
        it "Current State" $
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug Input.kvApplication) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kvApplication
                compiled `shouldBe` expected)

    -}