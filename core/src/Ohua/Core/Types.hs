-- |
-- Module      : $Header$
-- Description : Basic types for the Ohua compiler
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
--
-- Fundamental types used in the ohua compilation process. For many
-- types this module exposes only the type, not its concrete
-- construction. This is intentional, as internal representations may
-- change. The type classes 'Make' and 'Unwrap' are provided to
-- convert to and from those types as needed.

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiWayIf                 #-}

#include "compat.h"

module Ohua.Core.Types
    ( SomeBinding(..)
    , Environment
    , options
    , Options
    , callEnvExpr
    , callLocalFunction
    , transformRecursiveFunctions
    , stageHandling
    , StageHandling
    , StageName
    , AbortCompilation
    , DumpCode(..)
    , OhuaState
    , nameGenerator
    , idCounter
    ) where

import Ohua.Core.Types.Environment
import Ohua.Core.Types.Reference
import Ohua.Core.Types.Stage
