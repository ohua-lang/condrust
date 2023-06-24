module Ohua.Types.Unresolved.PPrint where

import Universum

import Ohua.Types.Unresolved.Literal
import Ohua.Types.Bindings
import Ohua.Types.Unresolved.Reference
import Ohua.PPrint
import Ohua.Types.Make
import Ohua.Types.Classes
import Ohua.LensClasses

import Data.Text as T hiding (map)
import Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text as PP

import qualified Data.Text.IO as LT
import Data.List.NonEmpty ( (<|) )

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

instance Pretty (FunRef ty) where
    pretty = prettyFunRef

instance Pretty (Lit ty) where
    pretty = prettyLit
