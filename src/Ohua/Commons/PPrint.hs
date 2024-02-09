module Ohua.Commons.PPrint where

import Universum

import Ohua.Commons.Types.Literal
import Ohua.Commons.Types.Bindings
import Ohua.Commons.Types.Make
import Ohua.Commons.Types.Classes
import Ohua.Commons.LensClasses

import Data.Text as T hiding (map)
import Prettyprinter as PP
import Prettyprinter.Render.Text as PP

import qualified Data.Text.IO as LT
import Data.List.NonEmpty ( (<|) )

ohuaDefaultLayoutOpts :: PP.LayoutOptions
ohuaDefaultLayoutOpts =
    PP.defaultLayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine 100 1.0}

ohuaDefaultLayoutDoc :: PP.Doc ann -> Text
ohuaDefaultLayoutDoc = PP.renderStrict . PP.layoutSmart ohuaDefaultLayoutOpts

quickRender :: PP.Pretty a => a -> Text
quickRender = ohuaDefaultLayoutDoc . PP.pretty

quickRenderM :: (PP.Pretty a, MonadIO m) => a -> m ()
quickRenderM = liftIO . LT.putStr . quickRender

-- | Throws an error with a document as message
errorD :: PP.Doc ann -> a
errorD = error . ohuaDefaultLayoutDoc

-- | Takes a text and converts it to a doc that preserves line breaks, but also
-- concatenates words such that lines that are too long will automatically break.
flexText :: Text -> PP.Doc ann
flexText = PP.vsep . map (PP.fillSep . map PP.pretty . words) . lines

instance Pretty FnId where
    pretty = pretty . unwrap

instance Pretty Binding where
    pretty = pretty . (unwrap :: Binding -> Text)

instance Pretty QualifiedBinding where
    pretty qb = pretty (qb ^. namespace) <> slash <> pretty (qb ^. name)

instance Pretty NSRef where
    pretty = hcat . punctuate dot . map pretty . unwrap
