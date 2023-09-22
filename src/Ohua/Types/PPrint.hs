module Ohua.Types.PPrint where

import Universum

import Ohua.Types.Literal
import Ohua.Types.Reference
import Ohua.PPrint
import Prettyprinter as PP


prettyFunRef :: FunRef ty res -> Doc ann
prettyFunRef (FunRef sf fid ty) = pretty sf <> angles (pretty ty)  <> maybe emptyDoc (angles . pretty) fid

prettyLit :: Lit ty res -> Doc ann
prettyLit =
    \case
        FunRefLit funRef -> pretty funRef
        NumericLit n -> pretty n
        UnitLit -> "()"
        EnvRefLit he ty -> "$" <> pretty he <> "::" <> pretty ty
        BoolLit b -> pretty b
        StringLit str -> pretty str

prettyFunType :: FunType ty res -> Doc ann
prettyFunType =
    \case
        FunType argsTys retTy ->
            let pArgs = hsep (punctuate comma $ toList $ map pretty argsTys)
                pRet = pretty retTy
            in parens pArgs <> " -> " <> pRet
        STFunType stateTy [] retTy -> pretty stateTy  <> "-> () -> " <> pretty retTy
        STFunType stateTy argsTys retTy ->
            let pArgs = hsep (punctuate comma $ toList $ map pretty argsTys)
                pRet = pretty retTy
            in pretty stateTy <>" -> "<> parens pArgs <> " -> " <> pRet

instance Pretty (OhuaType ty res) where
    pretty = flexText . show

instance Pretty (FunType ty res) where
    pretty = prettyFunType

instance Pretty (FunRef ty res) where
    pretty = prettyFunRef

instance Pretty (Lit ty res) where
    pretty = prettyLit
