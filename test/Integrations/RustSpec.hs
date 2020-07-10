{-# LANGUAGE QuasiQuotes #-}
module Integrations.RustSpec
    ( spec
    )  where

import Ohua.Prelude

import Test.Hspec

import Ohua.Compile.Compiler
import Ohua.Compile.Config (intoLogLevel)
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
import Data.Text
import qualified Data.ByteString.Lazy.Char8 as L


renderRustCode :: SourceFile Span -> L.ByteString
renderRustCode = 
    encodeUtf8 . 
    (<> "\n") . 
    renderLazy . 
    layoutSmart defaultLayoutOptions . 
    pretty'

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
                    let options = def 
                    runCompM 
                        (intoLogLevel "warn")
                        $ compile inFile compScope options outDir
                    outCode :: SourceFile Span 
                        <- parse' <$> readInputStream (outDir </> takeFileName inFile)
                    return outCode

showCode :: SourceFile Span -> String
showCode code = 
    let opts = LayoutOptions (AvailablePerLine 50 1.0)
    in renderShowS (layoutPretty opts $ pretty' code) ""

spec :: Spec
spec = 
    describe "Rust integration (compilation) " $
        it "simple" $
            (showCode <$> compileCode [sourceFile| 
                fn test() -> String {
                    let x = f();
                    g(x)
                }
                |]) >>= 
            (`shouldBe`
                showCode
                    [sourceFile| 
                        fn test() -> String {
                            let x = f();
                            g(x)
                        }
                    |])