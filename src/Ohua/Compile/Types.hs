{-|

Module      : $Header$
Description : Types for the compiler.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}
{-# LANGUAGE CPP, ConstraintKinds #-}
module Ohua.Compile.Types where

import Ohua.Prelude
import Control.Monad.Trans.Control (MonadBaseControl) -- TODO find out if this is really needed

type CompM m
     = (MonadIO m, MonadBaseControl IO m, MonadError Error m, MonadLogger m)

-- type TyAnnMap = HM.HashMap Binding (FunAnn (TyExpr SomeBinding))

type LanguageFileSuffix = Text
-- TODO this needs fixing because the namespace ref does not contain the file type,
--      , e.g., "go", "rs" or "java". We need a way to differentiate here!
type CompilationScope = HM.HashMap NSRef LanguageFileSuffix

type FileRef = Text
