{-|

Module      : $Header$
Description : Interface for the code generation.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}

module Ohua.Compile.CodeGen.Iface where

import Ohua.Prelude

import Ohua.Compile.Types
import Ohua.Frontend.NS (FunAnn)
import Ohua.DFGraph (OutGraph)

import qualified Data.HashSet as Set

data Fun = Fun 
  { graph :: OutGraph
  , annotations :: FunAnn (TyExpr SomeBinding)
  , name :: Binding
  } deriving (Eq, Show, Generic)

data CodeGenData = CodeGenData
  { namespace :: Maybe NSRef
  , sfDependencies :: Set.HashSet QualifiedBinding
  , funs :: [Fun]
  } deriving (Eq, Show, Generic)

type CodeGen
     = forall m.( MonadError Text m
                , MonadLoggerIO m
                 ) =>
                     CodeGenData -> m LByteString
