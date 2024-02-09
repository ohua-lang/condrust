module Ohua.Core.Compile.Configuration where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang (Expr)
import Ohua.Core.DFLang.Lang (NormalizedDFExpr)

data CustomPasses embExpr ty = CustomPasses
  { passBeforeNormalize :: Expr embExpr ty -> OhuaM (Expr embExpr ty)
  , passAfterNormalize  :: Expr embExpr ty -> OhuaM (Expr embExpr ty)
  , passAfterDFLowering :: NormalizedDFExpr  embExpr ty -> OhuaM (NormalizedDFExpr embExpr ty)
  }

noCustomPasses :: CustomPasses embExpr ty
noCustomPasses = CustomPasses pure pure pure

instance Default (CustomPasses embExpr ty) where
  def = noCustomPasses
