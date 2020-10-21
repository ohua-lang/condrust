module Ohua.Core.Compile.Configuration where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang (Expr)
import Ohua.Core.DFLang.Lang (NormalizedDFExpr)

data CustomPasses = CustomPasses
  { passBeforeNormalize  :: forall ty. Expr ty -> OhuaM (Expr ty)
  , passAfterNormalize  :: forall ty. Expr ty -> OhuaM (Expr ty)
  , passAfterDFLowering :: forall ty. NormalizedDFExpr ty -> OhuaM (NormalizedDFExpr ty)
  }

noCustomPasses :: CustomPasses
noCustomPasses = CustomPasses pure pure pure

instance Default CustomPasses where
  def = noCustomPasses
