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
        let resolve = runErrAndLogM LevelWarn . R.resolveNS
        it "loading a 'normal' expression" $
            resolve
                ( Namespace 
                    (NSRef ["some_ns"])
                    [Full (makeThrow ["other_ns"]) "g"]
                    [Algo 
                        "f"
                        (LamE ["x"] 
                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) "other_ns/g") Nothing Untyped) `AppE` ["x"]))
                        "pointlessStringAnno"
                    "sometype"]

                , HM.fromList
                    [("other_ns/g", 
                        LamE ["y"] 
                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) "h") Nothing Untyped) `AppE` ["y"]))]
                )
            >>= (`shouldBe`
                    Namespace 
                        (NSRef ["some_ns"])
                        [Full (makeThrow ["other_ns"]) "g"]
                        [Algo 
                            "f"
                            (LamE ["x"]
                                $ LetE 
                                    "other_ns.g" (LamE ["y"] 
                                                ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) "h") Nothing Untyped) `AppE` ["y"]))
                                    ("other_ns.g" `AppE` ["x"]))
                            "pointlessStringAnno" "madeUpType"])
                
        it "loading a recursive expression" $
            resolve
                ( Namespace 
                    (NSRef ["some_ns"])
                    [Full (makeThrow ["other_ns"]) "g"]
                    [Algo 
                        "f"
                        (LamE ["x"] 
                            ((LitE $ FunRefLit $ FunRef "other_ns/g" Nothing Untyped) `AppE` ["x"]))
                        "pointlessStringAnno" "madeUpType"]
                , HM.fromList
                    [("other_ns/g", 
                        LamE ["y"] 
                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow ["other_ns"]) "g") Nothing Untyped) `AppE` ["y"]))]
                )
            >>= (`shouldBe`
                    Namespace 
                        (NSRef ["some_ns"])
                        [Full (makeThrow ["other_ns"]) "g"]
                        [Algo 
                            "f"
                            (LamE ["x"]
                                $ LetE "other_ns.g" (LamE ["y"] ("other_ns.g" `AppE` ["y"]))
                                    ("other_ns.g" `AppE` ["x"]))
                        "pointlessStringAnno" "madeUpType"]
                    )
