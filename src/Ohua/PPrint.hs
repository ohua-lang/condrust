module Ohua.PPrint where

import Universum

import Ohua.Types.Literal
import Ohua.Types.Reference
import Ohua.Types.Make
import Ohua.Types.Classes
import Ohua.LensClasses

import Data.Text as T hiding (map)
import Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text as PP

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


prettyFunRef :: FunRef ty -> Doc ann
prettyFunRef (FunRef sf fid ty) = pretty sf <> angles (pretty ty)  <> maybe emptyDoc (angles . pretty) fid

prettyLit :: Lit ty -> Doc ann
prettyLit =
    \case
        FunRefLit funRef -> pretty funRef
        NumericLit n -> pretty n
        UnitLit -> "()"
        EnvRefLit he ty -> "$" <> pretty he <> "::" <> pretty ty
        BoolLit b -> pretty b 
        StringLit str -> pretty str

prettyFunType :: FunType ty -> Doc ann
prettyFunType = 
    \case
        FunType [] retTy -> "() -> " <> pretty retTy
        FunType argsTys retTy -> 
            let pArgs = hsep (punctuate comma $ toList $ map pretty argsTys)
                pRet = pretty retTy
            in parens pArgs <> " -> " <> pRet
        STFunType stateTy [] retTy -> pretty stateTy  <> "-> () -> " <> pretty retTy
        STFunType stateTy argsTys retTy ->
            let pArgs = hsep (punctuate comma $ toList $ map pretty argsTys)
                pRet = pretty retTy
            in pretty stateTy <>" -> "<> parens pArgs <> " -> " <> pRet

instance Pretty (VarType ty) where
    pretty = flexText . showNoType

instance Pretty (FunType ty) where
    pretty = prettyFunType

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
