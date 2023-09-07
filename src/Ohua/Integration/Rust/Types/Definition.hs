{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Ohua.Integration.Rust.Types.Definition where
{-
import Ohua.Prelude hiding ((=:))

import Language.Rust.Quote hiding ((=:))
import Language.Rust.Pretty (pretty', Resolve, Pretty)
import Toml.Type as TV (TOML (..), Value (..))
import Toml.Type.Edsl as TE (mkToml, table, (=:))
import Toml as TP (pretty)
import qualified Data.ByteString.Lazy.Char8 as L hiding ((=:))
import qualified Data.Text.IO as T hiding ((=:))
import Data.Text.Prettyprint.Doc hiding (Pretty, (=:))
import Data.Text.Prettyprint.Doc.Render.Text hiding ((=:))


macro_support :: IO ()
macro_support = do
  T.putStrLn "Please create the following setup in your Rust project:"
  T.putStrLn "lib.rs ===================================="
  L.putStrLn $ render macro_rs
  T.putStrLn ""
  T.putStrLn "lib.rs ===================================="
  T.putStrLn $ TP.pretty lib_toml
  T.putStrLn ""
  T.putStrLn "lib.rs ===================================="
  T.putStrLn $ TP.pretty project_toml
  T.putStrLn ""
  where
    macro_rs = void
      [sourceFile|
        extern crate proc_macro;
        use proc_macro::TokenStream;

        #[proc_macro_attribute]
        pub fn extern_spec(args: TokenStream, input: TokenStream) -> TokenStream {
          TokenStream::new()
        }
      |]

    lib_toml :: TOML
    lib_toml = mkToml $ do
        table "package" $ do
          "name" =: Text "extern_specs"
          "version" =: TV.Text "0.1.0"
          "authors" =: TV.Array [TV.Text "The ConDRust compiler"]
          "edition" =: TV.Text "2018"

        table "lib" $ do
          "proc-macro" =: TV.Bool True

    project_toml :: TOML
    project_toml = mkToml $ do
        table "workspace" $
          "members" =: TV.Array [TV.Text "extern_specs"]

    render =
      encodeUtf8
        . (<> "\n")
        . renderLazy
        . layoutSmart defaultLayoutOptions
        . pretty'
-}