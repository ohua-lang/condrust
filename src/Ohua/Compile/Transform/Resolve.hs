{-|

Module      : $Header$
Description : Transformation for splicing in algos for algo references.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

This module is a transformation that turns a function reference into an expression.
The resulting expression does not contain any (function reference) anymore.
Function references in this transformation are just references to variable that refer to
a function. That is, this transformation does not deal with namespaces or such.

-}

module Ohua.Compile.Transform.Resolve where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang (Expr(LitE, LetE, VarE, LamE), Pat(VarP), ExprF(LitEF))
import Ohua.Compile.Types

import Control.Lens (over)
import qualified Data.HashMap.Strict as HM
import Data.Functor.Foldable (cata, embed)
import qualified Data.HashSet as HS
import qualified Data.Text as T

-- TODO: It feels to me like this function needs a rewrite. I'm quite sure there was no testing at
--       all to make sure it works for algorithms from other namespaces.
--       More urgently, we need to define what a namespace reference that a parser returns should look like.
--       For example in Go:
--       import some/other/ns
--                ^
--                |
--                 - What is this?
--       ns.foo()
--        ^
--        |
--         - Or this?
--
--       Or in Rust:
--       use some::other::ns::{foo};
--      
--       foo(); <-- This is not a local algo!
--
-- For this transformation, the expression only has the following form:
-- > some/other/ns:foo()
--   |               |
--    ---------------
--           |
--            -> This is a function literal with a qualified binding.
--
-- The algo is registered with the key 
-- > some/other/ns:foo
-- In the form of a qualified binding.
resolveNS :: (MonadError Error m)
          => (P.Namespace, NamespaceRegistry)
          -> m P.Namespace
resolveNS (ns, registry) = 
    return $ over algos (map (over algoCode work)) ns
    where
        work :: FrLang.Expr -> FrLang.Expr
        work e = 
            let calledFunctions = collectAllFunctionRefs registry e
                expr = foldl (flip addExpr) e calledFunctions
            in resolveExpr expr

        collectAllFunctionRefs :: NamespaceRegistry -> FrLang.Expr -> HS.HashSet QualifiedBinding
        collectAllFunctionRefs available =
            HS.unions .
            HS.toList . 
            HS.map (\qb ->
                maybe 
                    HS.empty
                    (HS.insert qb . collectAllFunctionRefs (HM.delete qb available))
                    $ HM.lookup qb available) . 
            collectFunctionRefs

        collectFunctionRefs :: FrLang.Expr -> HS.HashSet QualifiedBinding
        collectFunctionRefs e =
            HS.fromList [r | LitE (FunRefLit (FunRef r _)) <- universe e]
        
        pathToVar :: QualifiedBinding -> Binding
        pathToVar (QualifiedBinding ns bnd) = 
            (makeThrow . T.intercalate ".") $ map unwrap $ unwrap ns ++ [bnd]
        
        addExpr :: QualifiedBinding -> FrLang.Expr -> FrLang.Expr
        -- TODO This is an assumption that fails in Ohua.Compile.Compiler.prepareRootAlgoVars
        --      We should enforce this via the type system rather than a runtime error!
        addExpr bnd (LamE vars body) = LamE vars $ addExpr bnd body
        addExpr bnd e = 
            LetE
                (VarP $ pathToVar bnd)
                (fromMaybe 
                    (error "impossible") -- the path was originally retrieved from this list
                    $ HM.lookup bnd registry)
                e

        resolveExpr :: FrLang.Expr -> FrLang.Expr
        resolveExpr = cata $ \case
            LitEF (FunRefLit (FunRef ref _id)) | HM.member ref registry -> VarE $ pathToVar ref
            e -> embed e
