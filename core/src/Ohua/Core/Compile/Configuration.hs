module Ohua.Core.Compile.Configuration where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.DFLang.Lang

data CustomPasses env = CustomPasses
  { passBeforeNormalize  :: Expression -> OhuaM env Expression
  , passAfterNormalize  :: Expression -> OhuaM env Expression
  , passAfterDFLowering :: DFExpr -> OhuaM env DFExpr
  }

noCustomPasses :: CustomPasses env
noCustomPasses = CustomPasses pure pure pure

instance Default (CustomPasses env) where
  def = noCustomPasses
