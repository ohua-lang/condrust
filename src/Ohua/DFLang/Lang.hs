-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file

-- This module defines the dataflow IR.
-- It introduces the notion of a Flow type and defines a new stateful function execution that
-- works based on flows instead of variables.
-- The ALang IR is transformed straight into the dataflow IR.
-- One important aspect of DFLang: it does not define any abstractions, i.e., there are no function definitions.
--
{-# LANGUAGE StandaloneDeriving  #-}
module Ohua.DFLang.Lang where

import           Control.DeepSeq
import           Data.Foldable   (toList)
import           Data.Hashable
import           Data.List       (intercalate)
import           Data.Monoid
import           Data.Sequence
import           Data.Foldable
import           Data.List
import           Data.String
import qualified Data.Text       as T
import           Ohua.ALang.Lang
import           Ohua.Types
import qualified Ohua.Util.Str as Str

-- | A sequence of let statements with a terminating binding to be used as return value
data DFExpr = DFExpr
    { letExprs  :: Seq LetExpr
    , returnVar :: !Binding
    }
    deriving (Eq, Show)

data LetExpr = LetExpr
    { callSiteId       :: !FnId
    , returnAssignment :: !Assignment
    , functionRef      :: !DFFnRef
    , callArguments    :: ![DFVar]
    , contextArg       :: !(Maybe Binding)
    }
    deriving (Eq, Show)

data DFFnRef
    = DFFunction !QualifiedBinding -- a build-in function of DFLang
    | EmbedSf !QualifiedBinding -- an generic dataflow function that wraps a stateful function call
    deriving (Eq, Show)

data DFVar
    = DFEnvVar !HostExpr
    | DFVar !Binding
    deriving (Eq, Show)


instance Hashable DFVar where
   hashWithSalt s (DFVar v)    = hashWithSalt s (0::Int, v)
   hashWithSalt s (DFEnvVar e) = hashWithSalt s (1::Int, e)

instance IsString DFVar where fromString = DFVar . fromString
instance Num DFVar where fromInteger = DFEnvVar . fromInteger


instance NFData DFExpr where
    rnf (DFExpr a b) = a `deepseq` rnf b

instance NFData LetExpr where
    rnf (LetExpr a b c d e) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` rnf e

instance NFData DFFnRef where
    rnf (DFFunction bnd) = rnf bnd
    rnf (EmbedSf bnd)    = rnf bnd

instance NFData DFVar where
    rnf (DFEnvVar e) = rnf e
    rnf (DFVar v)    = rnf v


showDFExpr :: DFExpr -> Str.Str
showDFExpr (DFExpr lets retVar) =
    Str.unlines $ (toList $ fmap showLet lets) <> [showBnd retVar]
  where
    showBnd (Binding b) = b
    showAssign (Direct v)       = showBnd v
    showAssign (Destructure vs) = "[" <> Str.intercalate ", " (map showBnd vs) <> "]"
    showRef (DFFunction a) = Str.showS a
    showRef (EmbedSf a)    = Str.showS a
    showVar (DFEnvVar h) = Str.showS h
    showVar (DFVar v)    = showBnd v
    showLet (LetExpr id ass ref args ctxArc) =
        "let " <> showAssign ass <> " = " <> showRef ref <> "<" <> Str.showS id <> "> ("
        <> Str.intercalate ", " (map showVar args) <> ")" <> maybe "" (\a -> " [" <> showBnd a <>"]") ctxArc
