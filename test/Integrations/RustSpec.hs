{-# LANGUAGE QuasiQuotes #-}
module Integrations.RustSpec
    ( spec
    )  where

import Ohua.Prelude

import Test.Hspec

import Ohua.Core.Types.Environment (stageHandling)
import Ohua.Core.Types.Stage (DumpCode(..))
import Ohua.Core.Stage

import Ohua.Compile.Compiler
import Ohua.Compile.Config (intoLogLevel, intoStageHandling, Stage(..))
import qualified Data.HashMap.Lazy as HM

import System.FilePath
import System.IO.Temp

import Language.Rust.Pretty ( pretty' )
import Language.Rust.Quote
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (parse', readInputStream, Span)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc.Render.String (renderShowS)
import Data.Text as T (concat, Text)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy.IO as LT


renderRustCode :: SourceFile Span -> L.ByteString
renderRustCode = 
    encodeUtf8 . 
    (<> "\n") . 
    renderLazy . 
    layoutSmart defaultLayoutOptions . 
    pretty'

debugOptions = def & stageHandling .~ debugStageHandling 

debugStageHandling = 
    intoStageHandling DumpStdOut
        $ Just [Stage coreDflang True False]

compileCode :: SourceFile Span -> IO (SourceFile Span)
compileCode inCode = 
    withSystemTempFile 
        "test.rs" 
        $ \inFile _inHandle -> do
            hClose _inHandle
            L.writeFile inFile $ renderRustCode inCode
            withSystemTempDirectory "output" 
                $ \outDir -> do
                    let compScope = HM.empty
                    let options = debugOptions -- def 
                    runCompM 
                        (intoLogLevel "warn")
                        $ compile inFile compScope options outDir
                    outCode :: SourceFile Span 
                        <- parse' <$> readInputStream (outDir </> takeFileName inFile)
                    return outCode

showCode :: T.Text -> SourceFile Span -> IO T.Text
showCode msg code = 
    let 
        c = renderStrict $ layoutSmart defaultLayoutOptions $ pretty' code
    in do
        print c
        return c
    where
        print code = putStr $ boundary <> header <> code <> boundary
        boundary = "\n" <> T.concat (replicate 20 ("-"::T.Text)) <> "\n"
        header = msg <> "\n\n"

spec :: Spec
spec = 
    describe "Rust integration (compilation) " $ do
        it "simple" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                fn test() -> String {
                    let x = f();
                    g(x)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        fn test() -> String {
                            let x = f();
                            g(x)
                        }
                    |]
                expected `shouldBe` compiled)
        -- it "env vars" $
        --     (showCode <$> compileCode [sourceFile| 
        --         fn test(i: i32) -> String {
        --             let x = f(i);
        --             g(x)
        --         }
        --         |]) >>= 
        --     (`shouldBe`
        --         showCode
        --             [sourceFile| 
        --                 fn test(i: i32) -> String {
        --                     let x = f(i);
        --                     g(x)
        --                 }
        --             |])