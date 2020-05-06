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

import Ohua.Parser.Common as P
import Ohua.Frontend.Lang as FrLang (Expr(LitE, LetE, VarE), Pat(VarP), ExprF(LitEF))
import Ohua.Compile.Types (NamespaceRegistry)

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
--       use some::other::ns{foo};
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
    return $ over algos (map (\algo -> (over algoCode work algo))) ns
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
                    (collectAllFunctionRefs $ HM.delete qb available)
                    $ HM.lookup qb available) . 
            collectFunctionRefs

        collectFunctionRefs :: FrLang.Expr -> HS.HashSet QualifiedBinding
        collectFunctionRefs e = HS.fromList [ref | LitE (FunRefLit (FunRef ref _id)) <- universe e]
        
        pathToVar :: QualifiedBinding -> Binding
        pathToVar (QualifiedBinding ns bnd) = 
            (makeThrow . (T.intercalate ".")) $ map unwrap $ (unwrap ns) ++ [bnd]
        
        addExpr :: QualifiedBinding -> FrLang.Expr -> FrLang.Expr
        addExpr bnd = 
            LetE
                (VarP $ pathToVar bnd)
                $ fromMaybe 
                    (error "impossible") -- the path was originally retrieved from this list
                    $ HM.lookup bnd registry

        resolveExpr :: FrLang.Expr -> FrLang.Expr
        resolveExpr = cata $ \expr ->
            case expr of
                LitEF (FunRefLit (FunRef ref _id)) -> VarE $ pathToVar ref
                e -> embed e
        
        -- resolveExpr :: QualifiedBinding -> FrLang.Expr -> FrLang.Expr
        -- resolveExpr current = cata $ \expr ->
        --     case expr of
        --         -- TODO Recursive function support:
        --         -- Even the recursive resolution of expressions does not work for recursive functions.
        --         -- More importantly, we can at this point only understand what is a recursive function 
        --         -- until we spliced it in and resolved into the spliced in expression.
        --         -- We need to understand here when we are splicing in a recursive function and if so then
        --         -- we need to splice in this code:
        --         -- let f = rec_fun_expr in f(...)
        --         -- We need to make sure that "f" is a unique id though.
        --         e@(LitEF (FunRefLit (FunRef ref _id))) | not $ ref == current  ->
        --             -- resolve the expression to be spliced in
        --             maybe (embed e) (resolveExpr ref) $ HM.lookup ref registry
        --         e -> embed e

