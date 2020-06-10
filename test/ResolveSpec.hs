{-# LANGUAGE OverloadedStrings #-}
module ResolveSpec (spec) where

import Ohua.Prelude

import Ohua.Frontend.Transform.Resolve as R
import Ohua.Frontend.Lang as FrLang
import qualified Data.HashMap.Strict as HM
import Test.Hspec


spec :: Spec
spec =
    describe "ns resolution" $ do
        let resolve = runCompM LevelWarn . R.resolveNS
        it "loading a 'normal' expression" $
            resolve
                ( Namespace 
                    ["some_ns"]
                    [Full (makeThrow ["other_ns"]) "g"]
                    [Algo 
                        "f"
                        $ LamE ["x"] 
                            ((LitE $ FunRefLit $ FunRef "other_ns/g" Nothing) `AppE` ["x"])]
                , HM.fromList
                    [("other_ns/g", 
                        LamE ["y"] 
                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) "h") Nothing) `AppE` ["y"]))]
                )
            >>= (`shouldBe`
                Namespace 
                    ["some_ns"]
                    [Full (makeThrow ["other_ns"]) "g"]
                    [Algo 
                        "f"
                        $ LamE ["x"]
                            $ LetE 
                                "other_ns.g" (LamE ["y"] 
                                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) "h") Nothing) `AppE` ["y"]))
                                ("other_ns.g" `AppE` ["x"])]
                )
        it "loading a recursive expression" $
            resolve
                ( Namespace 
                    ["some_ns"]
                    [Full (makeThrow ["other_ns"]) "g"]
                    [Algo 
                        "f"
                        $ LamE ["x"] 
                            ((LitE $ FunRefLit $ FunRef "other_ns/g" Nothing) `AppE` ["x"])]
                , HM.fromList
                    [("other_ns/g", 
                        LamE ["y"] 
                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow ["other_ns"]) "g") Nothing) `AppE` ["y"]))]
                )
            >>= (`shouldBe`
                Namespace 
                    ["some_ns"]
                    [Full (makeThrow ["other_ns"]) "g"]
                    [Algo 
                        "f"
                        $ LamE ["x"]
                            $ LetE "other_ns.g" (LamE ["y"] ("other_ns.g" `AppE` ["y"]))
                                ("other_ns.g" `AppE` ["x"])]
                )
