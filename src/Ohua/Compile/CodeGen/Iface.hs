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

import qualified Data.HashSet as Set

import Ohua.Standalone -- Used for TyAnnMap, that alias should be moved so I can
                       -- remove that dependency here
import Ohua.DFGraph
import qualified Ohua.DFGraph.File as GR

data CodeGenOpts = CodeGenOpts

data Fun = Fun 
  { graph :: OutGraph
  , annotations :: Maybe TyAnnMap
  , name :: Binding
  } deriving (Eq, Show, Generic)

data CodeGenData = CodeGenData
  { namespace :: NSRef
  , sfDependencies :: Set.HashSet QualifiedBinding
  , funs :: [Fun]
  } deriving (Eq, Show, Generic)

type CodeGen
     = forall m. ( --MonadReader CodeGenOpts m,
                 MonadError Text m
                , MonadLoggerIO m
                 ) =>
                     CodeGenData -> m LByteString
