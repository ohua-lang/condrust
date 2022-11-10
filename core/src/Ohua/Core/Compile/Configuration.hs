module Ohua.Core.Compile.Configuration where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang (Expr)
import Ohua.Core.DFLang.Lang (NormalizedDFExpr)

data CustomPasses ty = CustomPasses
  { passBeforeNormalize :: Expr ty -> OhuaM (Expr ty)
  , passAfterNormalize  :: Expr ty -> OhuaM (Expr ty)
  , passAfterDFLowering :: NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
  }

noCustomPasses :: CustomPasses ty
noCustomPasses = CustomPasses pure pure pure

instance Default (CustomPasses ty) where
  def = noCustomPasses
