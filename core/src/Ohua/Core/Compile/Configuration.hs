module Ohua.Core.Compile.Configuration where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.DFLang.Lang

data CustomPasses = CustomPasses
  { passBeforeNormalize  :: Expression -> OhuaM Expression
  , passAfterNormalize  :: Expression -> OhuaM Expression
  , passAfterDFLowering :: DFExpr -> OhuaM DFExpr
  }

noCustomPasses :: CustomPasses
noCustomPasses = CustomPasses pure pure pure

instance Default CustomPasses where
  def = noCustomPasses
