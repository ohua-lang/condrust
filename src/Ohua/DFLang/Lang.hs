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
module Ohua.DFLang.Lang where

import           Control.DeepSeq
import           Data.Foldable   (toList)
import           Data.Hashable
import           Data.List       (intercalate)
import           Data.Monoid
import           Data.Sequence
import           Data.String
import qualified Data.Text       as T
import           Ohua.ALang.Lang
import           Ohua.Types

-- | A sequence of let statements with a terminating binding to be used as return value
data DFExpr = DFExpr
    { letExprs  :: Seq LetExpr
    , returnVar :: !Binding
    }
    deriving Eq

data LetExpr = LetExpr
    { callSiteId       :: !FnId
    , returnAssignment :: !Assignment
    , functionRef      :: !DFFnRef
    , callArguments    :: ![DFVar]
    , contextArg       :: !(Maybe Binding)
    }
    deriving Eq

data DFFnRef
    = DFFunction !QualifiedBinding -- a build-in function of DFLang
    | EmbedSf !QualifiedBinding -- an generic dataflow function that wraps a stateful function call
    deriving Eq

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


showDFExpr :: DFExpr -> T.Text
showDFExpr (DFExpr lets retVar) =
    T.unlines $ (toList $ fmap showLet lets) <> [showBnd retVar]
  where
    showT :: Show a => a -> T.Text
    showT = T.pack . show
    showBnd (Binding b) = b
    showAssign (Direct v)       = showBnd v
    showAssign (Destructure vs) = "[" <> T.intercalate ", " (map showBnd vs) <> "]"
    showRef (DFFunction a) = showT a
    showRef (EmbedSf a)    = showT a
    showVar (DFEnvVar h) = showT h
    showVar (DFVar v)    = showBnd v
    showLet (LetExpr id ass ref args ctxArc) =
        "let " <> showAssign ass <> " = " <> showRef ref <> "<" <> showT id <> "> ("
        <> T.intercalate ", " (map showVar args) <> ")" <> maybe "" (\a -> " [" <> showBnd a <>"]") ctxArc
