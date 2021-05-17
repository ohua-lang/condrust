module Ohua.PPrint where

import Universum

import Ohua.Types.Literal
import Ohua.Types.Reference
import Ohua.Types.Make
import Ohua.LensClasses

import Data.Text as T hiding (map)
import Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text as PP

import qualified Data.Text.IO as LT


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


prettyFunRef :: FunRef ty -> Doc ann
prettyFunRef (FunRef sf fid _) = pretty sf <> maybe emptyDoc (angles . pretty) fid

prettyLit :: Lit ty -> Doc ann
prettyLit =
    \case
        FunRefLit funRef -> pretty funRef
        NumericLit n -> pretty n
        UnitLit -> "()"
        EnvRefLit he -> "$" <> pretty he
        BoolLit b -> pretty b

instance Pretty HostExpr where
    pretty = pretty . unwrap

instance Pretty FnId where
    pretty = pretty . unwrap

instance Pretty Binding where
    pretty = pretty . (unwrap :: Binding -> Text)

instance Pretty (FunRef ty) where
    pretty = prettyFunRef

instance Pretty QualifiedBinding where
    pretty qb = pretty (qb ^. namespace) <> slash <> pretty (qb ^. name)

instance Pretty NSRef where
    pretty = hcat . punctuate dot . map pretty . unwrap

instance Pretty (Lit ty) where
    pretty = prettyLit
