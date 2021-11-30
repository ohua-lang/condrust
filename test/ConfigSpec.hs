{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ConfigSpec (spec) where

import Ohua.Prelude

import Ohua.Compile.Config as C
import Ohua.Compile.Compiler as Comp
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import Text.RawString.QQ (r)
import Test.Hspec


parseConfig :: ByteString -> IO CompilerOptions
parseConfig = Y.decodeThrow

spec :: Spec
spec =
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
                { compilationScope = HM.fromList [ (["some","other","ns","module"],".go")
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
                { compilationScope = HM.fromList [ (["some","ns","module"],".go") ]
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
                { compilationScope = HM.fromList [ (["some","ns","module"],".go") ]
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
                { compilationScope = HM.fromList [ (["some","ns","module"],".go") ]
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
                { compilationScope = HM.fromList [ (["some","ns","module"],".go") ]
                , extraFeatures = []
                , debug = C.DebugOptions { logLevel = LevelDebug
                                        , stageHandlingOpt = C.defaultStageHandling
                                        }
                }
            )
        it "empty compilation scope" $
            parseConfig [r|
                compilation-scope:
                debug:
                    log-level: debug
            |] >>= (`shouldBe`
                C.CompilerOptions
                { compilationScope = HM.empty
                , extraFeatures = []
                , debug = C.DebugOptions { logLevel = LevelDebug
                                        , stageHandlingOpt = C.defaultStageHandling
                                        }
                }
            )
        it "no compilation scope" $
            parseConfig [r|
                debug:
                    log-level: debug
            |] >>= (`shouldBe`
                C.CompilerOptions
                { compilationScope = HM.empty
                , extraFeatures = []
                , debug = C.DebugOptions { logLevel = LevelDebug
                                        , stageHandlingOpt = C.defaultStageHandling
                                        }
                }
            )
