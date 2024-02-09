module Ohua.Commons.Types.PPrint where

import Universum

import Ohua.Commons.Types.Literal
import Ohua.Commons.Types.Reference
import Ohua.Commons.PPrint
import Prettyprinter as PP


prettyFunRef :: FunRef ty res -> Doc ann
prettyFunRef (FunRef sf ty) = pretty sf <> angles (pretty ty)

prettyLit :: Lit lang ty res -> Doc ann
prettyLit =
    \case
        FunRefLit funRef -> pretty funRef
        NumericLit n -> pretty n
        UnitLit -> "()"
        HostLit he ty -> "<<" <> show he <> ">>"
        EnvRefLit he ty -> "$" <> pretty he <> "::" <> pretty ty
        BoolLit b -> pretty b
        StringLit str -> pretty str

prettyFunType :: FunType ty res -> Doc ann
prettyFunType =
    \case
        FunType argsTys retTy ->
            let pArgs = prettyArgs argsTys
                pRet = pretty retTy
            in parens pArgs <> " -> " <> pRet
        -- STFunType stateTy [] retTy -> pretty stateTy  <> "-> () -> " <> pretty retTy
        STFunType stateTy argsTys retTy ->
            let pArgs = prettyArgs argsTys
                pRet = pretty retTy
            in pretty stateTy <>" -> "<> parens pArgs <> " -> " <> pRet

prettyArgs :: Either () (NonEmpty (OhuaType ty res)) -> Doc ann
prettyArgs (Left _ ) = "[]"
prettyArgs (Right argsTys) = hsep (punctuate comma $ toList $ map pretty argsTys)

instance Pretty (OhuaType ty res) where
    pretty = flexText . show

instance Pretty (FunType ty res) where
    pretty = prettyFunType

instance Pretty (FunRef ty res) where
    pretty = prettyFunRef

instance Pretty (Lit lang ty res) where
    pretty = prettyLit
