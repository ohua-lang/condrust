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
import Ohua.DFGraph (OutGraph, AbstractOutGraph(..))
import Ohua.Parser.Common

import qualified Data.HashSet as Set

import Data.Aeson.TH

data Fun where
  Fun :: BackendSupport a => 
    { graph :: OutGraph
    , annotations :: Annotated a (FunAnn (TyExpr SomeBinding))
    , name :: Binding
    } 
    -> Fun

deriving instance Show Fun
-- not supported: https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/
-- deriving instance Eq Fun
-- deriving instance Generic Fun

data CodeGenData = CodeGenData
  { namespace :: Maybe NSRef
  , sfDependencies :: Set.HashSet QualifiedBinding
  , funs :: [Fun]
  } deriving (Show)

type CodeGen
     = forall m.( MonadError Text m
                  , MonadLoggerIO m
                  ) =>
                     CodeGenData -> m LByteString