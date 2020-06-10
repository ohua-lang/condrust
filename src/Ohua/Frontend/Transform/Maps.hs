module Ohua.Frontend.Transform.Maps where

import Ohua.Prelude

import Ohua.Frontend.Lang
import Data.HashSet as HS 
import Data.Functor.Foldable (cata, embed)
import Control.Lens.Combinators (over)
import Control.Lens.Plated (plate)


-- With the new concept for SMap and state, this will not be necessary anymore.
convertMaps :: FunRef -> FunRef -> Expr -> Expr
convertMaps initFun updateFun = cata $ \case
    LetEF 
        (VarP v) (AppE (LitE (FunRefLit iFun)) []) 
        (StmtE (MapE lam d) cont) -- an imperative loop can only be a statement! 
        | iFun == initFun ->
        let convertLambda = cata $ \case
                -- state case: r.append(new_value)
                StmtEF (AppE (BindE (VarE vt) (LitE (FunRefLit uFun))) [value]) (LitE UnitLit)
                    | uFun == updateFun 
                    && vt == v ->
                    -- && countUsage lam v == 1 -> 
                    value
                -- FIXME We should not support a functional case here. Rather should
                --       the specific language integration transform this into the state notation.
                -- functional case: r = append(r, new_value)
                LetEF (VarP vt0) (AppE (LitE (FunRefLit uFun)) [VarE vt1, value]) (LitE UnitLit) 
                    | uFun == updateFun 
                    && vt0 == v
                    && vt1 == v ->
                    -- && countUsage lam v == 2 ->
                    value
                e -> embed e
        in LetE (VarP v) (flip MapE d $ convertLambda lam) cont
    e -> embed e
    -- TODO To properly implement this, we would have to scope the variables!
    -- where
    --     countUsage e v = length [vt | (VarE vt) <- universe e, -- usage
    --                                    vt == v,
    --                                    -- reassignments anywhere?
    --                                   (LetE pl _ _) <- universe e,
    --                                   pl == v,
    --                                   (LamE []) <- universe p] 
