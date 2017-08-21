-- |
-- Module      : $Header$
-- Description : Implementation for the @seq@ higher order function.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
module Ohua.DFLang.HOF.Seq where

import           Data.Sequence        as S
import           Ohua.DFLang.HOF
import           Ohua.Types
import           Ohua.DFLang.Lang
import           Ohua.Monad
import           Control.Monad.Except
import           Control.Monad.State

data SeqFn = SeqFn {
  before :: DFVar,
  afterExpr :: Lambda
}

instance HigherOrderFunction SeqFn where
  name = "com.ohua.lang/seq"

  parseCallAndInitState [Variable before, LamArg after] = return $ SeqFn before after
  parseCallAndInitState as = throwError "seq not defined for arguments: " -- TODO ++ show as

  createContextEntry = return S.empty

  createContextExit assignment = return S.empty

  scopeFreeVariables lam freeVars = return (S.empty, [])

  contextifyUnboundFunctions _ = undefined
