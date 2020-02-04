{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Ohua.Prelude

import Ohua.Compile.Config as C
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import Text.RawString.QQ (r)
import Test.Hspec

parseConfig :: ByteString -> IO CompilerOptions
parseConfig = Y.decodeThrow

main :: IO ()
main = hspec $
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
