module Ohua.Core.Compile.Configuration where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang (Expr)
import Ohua.Core.DFLang.Lang (NormalizedDFExpr)

data CustomPasses embExpr annot ty = CustomPasses
  { passBeforeNormalize :: Expr embExpr annot ty -> OhuaM (Expr embExpr annot ty)
  , passAfterNormalize  :: Expr embExpr annot ty -> OhuaM (Expr embExpr annot ty)
  , passAfterDFLowering :: NormalizedDFExpr  embExpr annot ty -> OhuaM (NormalizedDFExpr embExpr annot ty)
  }

noCustomPasses :: CustomPasses embExpr annot ty
noCustomPasses = CustomPasses pure pure pure

instance Default (CustomPasses embExpr annot ty) where
  def = noCustomPasses
