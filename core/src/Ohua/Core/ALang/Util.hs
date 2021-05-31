-- |
-- Module      : $Header$
-- Description : Utilities for working with the algorithm language
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Core.ALang.Util where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import qualified Ohua.Core.ALang.Refs as Refs (nth)

import Control.Comonad
import qualified Control.Lens as Lens (para)
import Data.Functor.Foldable (embed, para)
import qualified Data.HashSet as HS


substitute :: Binding -> Expr ty -> Expr ty -> Expr ty
-- Postwalk avoids an infinite recursion in a case where `val` uses a
-- `var` binding.  This should never happen but might if this
-- invariant is violated for some reason and the violation is not
-- caught.
substitute !var val =
    transform $ \case
        Var v
            | v == var -> val
        other -> other

-- | Note that this only performs the initial lifting of the lambda expression.
-- It does not find calls to the lambda and rewrites them accordingly.
-- This is due to the fact that we often want to package the freeVars via a call to
-- Ohua.Core.lang/array to make the backend implementation easier and I'm not sure whether
-- this is always true.
-- This is also the reason why I keep this in Util into of making it an own pass.
destructure :: Expr ty -> [Binding] -> Expr ty -> Expr ty
destructure source bnds =
    foldl (.) id $
    map (\(idx, bnd0) -> Let bnd0 $ mkNthExpr idx source) (zip [0 ..] bnds)
  where
    mkNthExpr idx source0 =
        pureFunction Refs.nth Nothing (FunType $ Right [TypeVar,TypeVar,TypeVar]) `Apply` (Lit $ NumericLit idx) `Apply`
        (Lit $ NumericLit $ toInteger $ length bnds) `Apply`
        source0

lambdaLifting :: forall ty m.
       (MonadGenBnd m) => Expr ty -> m (Expr ty, [Expr ty])
lambdaLifting e = do
    (e', actuals) <- go findFreeVariables e
    (e'', actuals') <- go findLonelyLiterals e'
    return (e'', actuals ++ actuals')
  where
    go :: (Expr ty -> [Expr ty]) -> Expr ty -> m (Expr ty, [Expr ty])
    go findFreeExprs expr
        | null freeExprs = pure (expr, [])
        | otherwise = do
            newFormals <- mapM bindingFromAny freeExprs
            let rewrittenExp =
                    foldl
                        (\newExpr (from, to) -> renameExpr from (Var to) newExpr)
                        body
                        (zip freeExprs newFormals)
            return (mkLambda (formalVars ++ newFormals) rewrittenExp, freeExprs)
      where
        (formalVars, body) =
            case expr of
                (Lambda _ _) -> lambdaArgsAndBody expr
                _            -> ([], expr)
        freeExprs = findFreeExprs expr
        renameExpr from to =
            rewrite $ \expression ->
                if expression == from
                    then Just to
                    else Nothing
    bindingFromAny (Var v) = generateBindingWith v
    bindingFromAny (Lit l) = generateBindingWith $ "lit_" <> litType
      where
        litType =
            case l of
                NumericLit li -> show li
                UnitLit -> "unit"
                FunRefLit ref -> bindifyFunRef ref
                EnvRefLit li -> "env_" <> show li
    bindifyFunRef :: FunRef ty -> Binding
    bindifyFunRef _ = error "Unsupported transformation of fun_ref literal" -- FIXME

mkLambda :: [Binding] -> Expr ty -> Expr ty
mkLambda args expr = go expr $ reverse args
  where
    go e (a:as) = flip go as $ Lambda a e
    go e [] = e

-- FIXME pattern match failure because ALang not precise enough (see issue #8)
replaceLit :: Expr ty -> (Expr ty, Binding) -> Expr ty
replaceLit e (Lit old, new) =
    flip transform e $ \case
        Lit l
            | l == old -> Var new
        other -> other
        
-- FIXME pattern match failure because ALang not precise enough (see issue #8)
renameVar :: Expr ty -> (Expr ty, Binding) -> Expr ty
renameVar e (Var old, new) =
    flip transform e $ \case
        Var v
            | v == old -> Var new
        other -> other

-- | All bindings defined in an expression *with duplicates*
definedBindings :: Expr ty -> [Binding]
definedBindings e =
    [ v
    | e' <- universe e
    , v <-
          case e' of
              Let v' _ _ -> [v']
              Lambda v' _ -> [v']
              _ -> []
    ]

-- | A very simple function that calculates all bindings that are used in an
-- expression but not defined in it. This is implemented as a simple set
-- intersection, therefore it relies on the fact that the expression is in SSA
-- form.
findFreeVariables :: Expr ty -> [Expr ty]
findFreeVariables e =
    map Var $
    sort $ -- makes the list of args deterministic
    toList $
    HS.difference
        (HS.fromList [v | Var v <- universe e])
        (HS.fromList $ definedBindings e)

findLiterals :: Expr ty -> [Expr ty]
findLiterals e =
    [ Lit lit
    | Lit l <- universe e
    , lit <-
          case l of
              UnitLit -> [l]
              NumericLit _ -> [l]
              EnvRefLit _ -> [l]
              _ -> []
    ]

-- | A literal is lonely if it does not accompany a var in the argument list to a call.
findLonelyLiterals :: HasCallStack => Expr ty -> [Expr ty]
findLonelyLiterals =
    Lens.para $ \case
        f@Apply {} ->
            const $
            if areAllLits args
                then take 1 $
                     -- HACK see #34
                     filter
                         (\case
                              Lit (FunRefLit _) -> False
                              _ -> True)
                         args
                -- We could also return `[]` in the else branch, because the
                -- expression should be normalized, but this is cleaner
                else args >>= findLonelyLiterals
            where args = getFunctionArgs f --snd $ fromApplyToList f
        _ -> join
  where
    areAllLits =
        all $ \case
            Lit _ -> True
            _ -> False

mkApply :: Expr ty -> [Expr ty] -> Expr ty
mkApply f args = go $ reverse args
  where
    go [v] = Apply f v
    go (v:vs) = Apply (go vs) v
    go [] = f

fromListToApply :: FunRef ty -> [Expr ty] -> Expr ty
fromListToApply f = mkApply $ Lit $ FunRefLit f

getFunctionArgs :: HasCallStack => Expr ty -> [Expr ty]
getFunctionArgs e = args
  where
    (_, _, args) = fromApplyToList' e

-- FIXME The errors in these functions should not be here. Either we enforce these things
--       via other means in the type system or we should change the return type to Maybe.
-- FIXME Using this function always creates more warnings because the type is not expressive enough.
fromApplyToList :: HasCallStack => Expr ty -> (FunRef ty, [Expr ty])
fromApplyToList e =
    case stateExpr of
        Just s ->
            error $ "Expected pure function, but found bound state: " <> show s
        _ -> (f, args)
  where
    (f, stateExpr, args) = fromApplyToList' e

fromApplyToList' :: HasCallStack => Expr ty -> (FunRef ty, Maybe (Expr ty), [Expr ty])
fromApplyToList' =
    para $ \case
        ApplyF (extract -> (f, s, args)) (arg, _) -> (f, s, args ++ [arg])
        LitF (FunRefLit f) -> (f, Nothing, [])
        BindStateF (stateExpr, _) (method, _) ->
            case method of
                Lit (FunRefLit f) -> (f, Just stateExpr, [])
                other ->
                    error $
                    "Expected state to be bound to function, found: " <>
                    show other
        other ->
            error $
            "Expected apply or function reference, got: " <>
            show (embed $ fmap fst other)

mkDestructured :: [Binding] -> Binding -> Expr ty -> Expr ty
mkDestructured formals compound = destructure (Var compound) formals

findDestructured :: Expr ty -> Binding -> [Binding]
findDestructured expr bnd = map (\(v,_,_) -> v) $ findDestructuredWithExpr expr
    where
        -- | Returns the letted nth nodes and their continuations such that they can later on be
        --   removed. Assumes SSA form.
        --   Be careful with this function because the returned expressions maybe nested with each other!
        findDestructuredWithExpr :: Expr ty -> [(Binding, Expr ty, Expr ty)]
        findDestructuredWithExpr e = 
            map (\(_,v,l,c) -> (v,l,c)) $
            sortOn (\(i,_,_,_) -> i)
                [ (i,v,l,c) 
                | l@(Let v 
                    (PureFunction "ohua.lang/nth" _ `Apply` 
                        Lit (NumericLit i) `Apply` 
                        _ `Apply` 
                        Var bnd')
                    c)  
                    <- universe e
                , bnd == bnd']

replaceExpr :: (Expr ty, Expr ty) -> Expr ty -> Expr ty
replaceExpr (old,new) = transform f
    where 
        f expr | expr == old = new
        f expr = expr

pattern NthFunction :: Binding -> Expr ty
pattern NthFunction bnd <- PureFunction "ohua.lang/nth" _ `Apply` _ `Apply` _ `Apply` Var bnd

evictOrphanedDestructured :: Expr ty -> Expr ty
evictOrphanedDestructured e = 
    let allBnds = HS.fromList [v | Let v _ _ <- universe e]
    in transform (f allBnds) e
    where 
        f :: HS.HashSet Binding -> Expr ty -> Expr ty
        f bnds (Let _v (NthFunction bnd) cont) | not $ HS.member bnd bnds = cont
        f _ expr = expr

lambdaArgsAndBody :: Expr ty -> ([Binding], Expr ty)
lambdaArgsAndBody (Lambda arg l@(Lambda _ _)) =
    let (args, body) = lambdaArgsAndBody l
     in (arg : args, body)
lambdaArgsAndBody (Lambda arg body) = ([arg], body)
lambdaArgsAndBody e = ([], e)
