module Ohua.Integration.Rust.Util where

import Ohua.Prelude

import Language.Rust.Data.Ident (Ident(..))
import Language.Rust.Parser ( parse' , Span)
import Language.Rust.Data.InputStream
import Language.Rust.Syntax (SourceFile, SourceFile(..))
import Language.Rust.Pretty (pretty', Resolve, Pretty)

import System.FilePath
import System.Directory (doesFileExist)

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Prettyprint.Doc hiding (Pretty)
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Lazy (unpack)


deSpan ::(Functor a) =>  a anno -> a ()
deSpan = map (const ())

toBinding :: Ident -> Binding
toBinding Ident{name=n} = fromString n

filePathToNsRef :: FilePath -> NSRef
filePathToNsRef = makeThrow . filePathToList

filePathToList :: FilePath -> [Binding]
filePathToList = map fromString . splitDirectories . dropExtension

loadRustFile :: FilePath -> IO (SourceFile Span)
-- We want library files to be optionally provided for imports. Hence we might end up here
-- trying to open a library file that is not provided at the given path.
loadRustFile srcFile = do
    exists <- doesFileExist srcFile
    if exists
        then parse' <$> readInputStream srcFile
        else (trace $ "Imported file not found: "<> show srcFile) return placeholderModule

-- REMINDER: Remove when real encapsulation library is produced
placeholderModule :: SourceFile Span
placeholderModule = SourceFile Nothing [] []

render :: (Resolve a, Pretty a) => a -> L.ByteString
render =
  encodeUtf8
    . (<> "\n")
    . renderLazy
    . layoutSmart defaultLayoutOptions
    . pretty'

renderStr :: (Resolve a, Pretty a) => a -> String
renderStr =
    unpack
    . renderLazy
    . layoutSmart defaultLayoutOptions
    . pretty'

