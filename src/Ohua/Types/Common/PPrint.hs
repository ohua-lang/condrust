module Ohua.Types.Common.PPrint where

import Universum

import Ohua.Types.Common.Literal
-- import Ohua.Types.Bindings
import Ohua.Types.Common.Reference
import Ohua.PPrint
-- import Ohua.Types.Make
-- import Ohua.Types.Classes
-- import Ohua.LensClasses

-- import Data.Text as T hiding (map)
import Prettyprinter as PP
-- import Prettyprinter.Render.Text as PP

-- import qualified Data.Text.IO as LT
-- import Data.List.NonEmpty ( (<|) )

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
