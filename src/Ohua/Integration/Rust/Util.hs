module Ohua.Integration.Rust.Util where

import Ohua.Prelude

import Language.Rust.Data.Ident (Ident(..))
import Language.Rust.Parser ( parse' , Span )
import Language.Rust.Data.InputStream
import Language.Rust.Syntax (SourceFile, SourceFile(..))


import System.FilePath

deSpan ::(Functor a) =>  a Span -> a ()
deSpan = map (const ())

toBinding :: Ident -> Binding
toBinding Ident{name=n} = fromString n

filePathToNsRef :: FilePath -> NSRef
filePathToNsRef = makeThrow . map fromString . splitDirectories . dropExtension

load :: FilePath -> IO (SourceFile Span)
load srcFile = parse' <$> readInputStream srcFile

-- REMINDER: Remove when real encapsulation library is produced
placeholderModule :: SourceFile Span
placeholderModule = SourceFile Nothing [] []
