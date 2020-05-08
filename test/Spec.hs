{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Ohua.Prelude

import Ohua.Compile.Config as C
import Ohua.Compile.Compiler as Comp
import Ohua.Compile.Util as U
import Ohua.Compile.Transform.Resolve as R
import Ohua.Frontend.Lang as FrLang
import Ohua.DFGraph
import Ohua.DFLang.Lang
import Ohua.Stage (coreAlang, normalizedAlang)
import Ohua.Parser.Common as P
import Ohua.Frontend.NS (FunAnn(..))
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import Text.RawString.QQ (r)
import Test.Hspec


newtype TestAnn = TestAnn Text deriving (Eq, Generic, Y.ToJSON, Y.FromJSON, Show)

testAnno = Annotated (TestAnn "noise") $ FunAnn [] $ TyRef "()"

main :: IO ()
main = hspec $
    exprTests >>
    configTests >>
    resolveTests

debugStageHandling :: StageName -> (DumpCode, Bool)
debugStageHandling x | x == normalizedAlang = (DumpPretty, False)
debugStageHandling x | x == coreAlang       = (DumpPretty, False)
debugStageHandling _                        = (Don'tDump, False)

compileAndShow :: FrLang.Expr -> IO OutGraph
compileAndShow expr = do
    gr <- U.runCompM 
        LevelWarn
        -- $ Comp.ohuaCoreCompilation debugStageHandling False expr 
        $ Comp.ohuaCoreCompilation C.defaultStageHandling False expr
    return gr

resolve = (U.runCompM LevelWarn) . R.resolveNS

resolveTests :: SpecWith ()
resolveTests =
    describe "ns resolution" $ do
        it "loading a 'normal' expression" $
            resolve
                ( P.Namespace 
                    (Just ["some_ns"])
                    [Import (makeThrow ["other_ns"]) ["g"]]
                    [Algo 
                        "f"
                        testAnno
                        $ LamE ["x"] 
                            ((LitE $ FunRefLit $ FunRef "other_ns/g" Nothing) `AppE` ["x"])]
                , HM.fromList
                    [("other_ns/g", 
                        (LamE ["y"] 
                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) "h") Nothing) `AppE` ["y"])))]
                )
            >>= (`shouldBe`
                P.Namespace 
                    (Just ["some_ns"])
                    [Import (makeThrow ["other_ns"]) ["g"]]
                    [Algo 
                        "f"
                        testAnno
                        $ LetE 
                            "other_ns.g" (LamE ["y"] 
                                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) "h") Nothing) `AppE` ["y"]))
                            $ LamE ["x"] ("other_ns.g" `AppE` ["x"])]
                )
        it "loading a recursive expression" $
            resolve
                ( P.Namespace 
                    (Just ["some_ns"])
                    [Import (makeThrow ["other_ns"]) ["g"]]
                    [Algo 
                        "f"
                        testAnno
                        $ LamE ["x"] 
                            ((LitE $ FunRefLit $ FunRef "other_ns/g" Nothing) `AppE` ["x"])]
                , HM.fromList
                    [("other_ns/g", 
                        (LamE ["y"] 
                            ((LitE $ FunRefLit $ FunRef (QualifiedBinding (makeThrow ["other_ns"]) "g") Nothing) `AppE` ["y"])))]
                )
            >>= (`shouldBe`
                P.Namespace 
                    (Just ["some_ns"])
                    [Import (makeThrow ["other_ns"]) ["g"]]
                    [Algo 
                        "f"
                        testAnno
                        $ LetE "other_ns.g" (LamE ["y"] ("other_ns.g" `AppE` ["y"]))
                            $ LamE ["x"] ("other_ns.g" `AppE` ["x"])]
                )

-- TODO maybe this wants to become a test suite that every parser needs to 
--      pass. it defines the semantics for ALang expressions.
exprTests :: SpecWith ()
exprTests = 
    describe "expressions" $ do
        it "parse simple expression" $
            compileAndShow 
                (LamE [] 
                    (LetE "result" (AppE (LitE $ FunRefLit $ FunRef "produceconsume/Produce" Nothing) []) 
                        $ StmtE 
                            (AppE (LitE $ FunRefLit $ FunRef "produceconsume/Consume" Nothing) 
                                   ["result"]) $ LitE UnitLit))
            >>= (`shouldBe` 
                OutGraph 
                    { operators = 
                        [ Operator {
                            operatorId = 1, 
                            operatorType = "ohua.lang/unitFn", 
                            operatorNType = FunctionNode }
                        , Operator {
                            operatorId = 2, 
                            operatorType = "produceconsume/Consume", 
                            operatorNType = FunctionNode }
                        , Operator {
                            operatorId = 3, 
                            operatorType = "ohua.lang/seqFun", 
                            operatorNType = FunctionNode }
                        , Operator {
                            operatorId = 4, 
                            operatorType = "ohua.lang/ctrl", 
                            operatorNType = OperatorNode }
                        , Operator {
                            operatorId = 6, 
                            operatorType = "ohua.lang/unitFn", 
                            operatorNType = FunctionNode }
                        ]
                    , arcs = Arcs {
                        direct = 
                            [ Arc 
                                { target = Target {operator = 1, index = 0}
                                , source = EnvSource (FunRefLit (FunRef "produceconsume/Produce" Nothing)) }
                            , Arc 
                                { target = Target {operator = 1, index = 1}
                                , source = EnvSource UnitLit }
                            , Arc 
                                { target = Target {operator = 2, index = 0}
                                , source = LocalSource (Target {operator = 1, index = 0}) }
                            ,Arc 
                                { target = Target {operator = 3, index = 0} 
                                , source = LocalSource (Target {operator = 2, index = 0}) }
                            , Arc 
                                { target = Target {operator = 4, index = 0}
                                , source = LocalSource (Target {operator = 3, index = 0}) }
                            , Arc 
                                { target = Target {operator = 4, index = 1}
                                , source = EnvSource UnitLit }
                            , Arc 
                                { target = Target {operator = 6, index = 0}
                                , source = EnvSource (FunRefLit (FunRef "ohua.lang/id" Nothing)) }
                            , Arc 
                                { target = Target {operator = 6, index = 1}
                                , source = LocalSource (Target {operator = 4, index = 0})}
                            ]
                        , state = []
                        , dead = [] }
                    , returnArc = Target {operator = 6, index = 0}})

parseConfig :: ByteString -> IO CompilerOptions
parseConfig = Y.decodeThrow

configTests :: SpecWith ()
configTests = 
    describe "config parsing" $ do
        it "parse full config" $
            parseConfig [r|
                output-format: json-graph
                compilation-scope:
                - some/ns/module.go
                - some/other/ns/module.go
                extra-features:
                - tail-rec
                debug:
                    log-level: verbose
                    core-stages: 
                        - stage: "before-normalization"
                          dump: yes
                          abort-after: no
                        - stage: "after-normalization"
                          dump: yes
                          abort-after: yes
            |] >>= (`shouldBe` 
                C.CompilerOptions 
                { outputFormat = C.JsonGraph
                , compilationScope = HM.fromList [ (["some","other","ns","module"],".go")
                                                , (["some","ns","module"],".go")]
                , extraFeatures = ["tail-rec"]
                , debug = C.DebugOptions { logLevel = LevelOther "verbose"
                                        , stageHandlingOpt = C.defaultStageHandling
                                        }
                }
            )
        it "debug is optional" $
            parseConfig [r|
                output-format: json-graph
                compilation-scope:
                - some/ns/module.go
                extra-features:
                - tail-rec
            |] >>= (`shouldBe` 
                C.CompilerOptions 
                { outputFormat = C.JsonGraph
                , compilationScope = HM.fromList [ (["some","ns","module"],".go") ]
                , extraFeatures = ["tail-rec"]
                , debug = C.DebugOptions { logLevel = LevelWarn
                                        , stageHandlingOpt = C.defaultStageHandling
                                        }
                }
            )
        it "output-format is optional" $
            parseConfig [r|
                compilation-scope:
                - some/ns/module.go
                extra-features:
                - tail-rec
            |] >>= (`shouldBe` 
                C.CompilerOptions 
                { outputFormat = C.JsonGraph
                , compilationScope = HM.fromList [ (["some","ns","module"],".go") ]
                , extraFeatures = ["tail-rec"]
                , debug = C.DebugOptions { logLevel = LevelWarn
                                        , stageHandlingOpt = C.defaultStageHandling
                                        }
                }
            )
        it "extra-features is optional" $
            parseConfig [r|
                compilation-scope:
                - some/ns/module.go
            |] >>= (`shouldBe` 
                C.CompilerOptions 
                { outputFormat = C.JsonGraph
                , compilationScope = HM.fromList [ (["some","ns","module"],".go") ]
                , extraFeatures = []
                , debug = C.DebugOptions { logLevel = LevelWarn
                                        , stageHandlingOpt = C.defaultStageHandling
                                        }
                }
            )
        it "log-level read properly" $
            parseConfig [r|
                compilation-scope:
                - some/ns/module.go
                debug:
                    log-level: debug
            |] >>= (`shouldBe` 
                C.CompilerOptions 
                { outputFormat = C.JsonGraph
                , compilationScope = HM.fromList [ (["some","ns","module"],".go") ]
                , extraFeatures = []
                , debug = C.DebugOptions { logLevel = LevelDebug
                                        , stageHandlingOpt = C.defaultStageHandling
                                        }
                }
            )
