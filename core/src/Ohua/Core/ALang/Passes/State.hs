{-|
Module      : $Header$
Description : Implementation of state transformations.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : EPL-1.0
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Ohua.Core.ALang.Passes.State where

import Ohua.Core.Prelude

import Ohua.Types.Vector hiding (map, zip, zip3, unzip, unzip3, filter, toList)
import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Util
import Ohua.Core.ALang.Passes.SSA
import qualified Ohua.Core.ALang.Refs as Refs

import Ohua.Core.ALang.PPrint ()

import qualified Data.HashSet as HS

import Control.Lens.Combinators (over)
import Control.Lens.Plated (plate)


-- TODO recursion support is still pending here!

-- | The SSA pass after transforming control state threads is vital.
--   It makes sure that all succeeding state threads after a control
--   state thread, take the state outputs of the control state threads.
preControlPasses :: MonadOhua m => Expr ty -> m (Expr ty)
preControlPasses =
  transformControlStateThreads >=>
  performSSA >=>
  transformFundamentalStateThreads >=>
  performSSA

-- FIXME there is code missing that turns the basic state threads into real state threads! this is the very first transformation that needs to run.
-- the transformation is: make state threads and then rebind the state -> alpha renaming on the rest of the term.

postControlPasses :: Expr ty -> Expr ty
postControlPasses = transformCtxtExits -- . traceShow "transforming ctxt exists!" -- TODO assert here that no more ctxt exits remain in the code

-- ToDo: Inputs are state + ctrl or nat and the output is the state
runSTCLangSMapFun :: Expr ty
runSTCLangSMapFun = Lit $ FunRefLit $ FunRef Refs.runSTCLangSMap Nothing $ FunType [TypeVar, TypeVar] TypeVar -- size and the state, i.e., there is one per state

-- FIXME this functions can not be untyped anymore!
-- ToDo: This is supposed to support state threading in branches and currently not used
runSTCLangIfFun :: Expr ty
runSTCLangIfFun = Lit $ FunRefLit $ FunRef Refs.runSTCLangIf Nothing $ FunType [] TypeVar

-- invariant: this type of node has at least one var as input (the computation result)
ctxtExit :: QualifiedBinding
ctxtExit = "ohua.lang/ctxtExit"


-- Question: Where is it used and what's the type supposed to be?
ctxtExitFunRef :: SNat ('Succ n) -> Expr ty
ctxtExitFunRef num = Lit $ FunRefLit $ FunRef ctxtExit Nothing $ FunType (toList $ replicateNE num TypeVar) TypeVar

-- | Transforms every stateful function into a fundamental state thread.
--   Corrects the reference to the state for the rest of the computation.
--   Assumption: expression is in ANF form (TODO enforce via the type system)
--
--  Transforms this:
-- @
--  let v = state.f () in c
-- @
--  into:
-- @
--  let (state', v) = state.f () in c[state'/state]
-- @
transformFundamentalStateThreads :: (MonadOhua m) => Expr ty -> m (Expr ty)
transformFundamentalStateThreads = transformM f
  where
    f e@(Let v app cont) = do
      s <- stBndForStatefulFun app
      let s' = usedIn cont =<< s
      maybe (return e) (g v app cont) s'
    f expr = return expr

    usedIn cont b@(bnd, _) =
      case [b | Var b <- universe cont, b == bnd] of
        [] -> Nothing
        _ -> Just b

    stBndForStatefulFun (StatefulFunction _ _ (Var tBnd@(TBind bnd ty))) = do Just . (\newBnd -> (tBnd, TBind newBnd ty)) <$> generateBindingWith bnd
    -- FIXME Once again, this stupid over-generalization of the language is a pain!
    stBndForStatefulFun e@(StatefulFunction {}) = error $ "state should have been var: " <> show e
    stBndForStatefulFun (e1 `Apply` _)            = stBndForStatefulFun e1
    stBndForStatefulFun _                         = return Nothing

    g r app cont (TBind stateIn sty, stateOut) = do
      -- Question: What's the 'x' type supposed to be? And why do the functions have such helpful and self-explaining names
      xBnd <- generateBinding
      let x = TBind xBnd TypeVar
      return $
        Let x app $
        destructure (Var x) [stateOut,r] $
        substitute stateIn (Var stateOut) cont

-- TODO do not introduce orphaned state, i.e., state that is never used again.
--      check before that which of the state is actually used after the control context it is used in.

-- | Adds a ctxtExit function that collects all states and the result of the context and passes it to the 'collect'/'select' function.
--   The transformation works on imperative code, .i.e.,
--   this:
-- @
--   ctxt \d ->
--          let x = before d in
--          let y1 = state1.f x in
--          let y2 = state2.f y1 in
--          let z = after y2 in
--          z
-- @
--   into that:
-- @
--   ctxt \d ->
--          let x = before d in
--          let y1 = state1.f x in
--          let y2 = state2.f y1 in
--          let z = after y2 in
--          let allOut = ctxtExit z state1 state2 in
--          allOut
-- @
-- Note though that the following case does NOT need to be contextified:
-- @
--   ctxt \state ->
--          let y = state.f () in
--          let z = after y in
--          z
-- @
-- Here 'state' is NOT a free variable!
--
-- Another important point concerns the destructuring of the result:
-- @
--   let ctxtOut = ctxt \d ->
--                         let x = before d in
--                         let y1 = state1.f x in
--                         let z = after y1 in
--                         let allOut = ctxtExit z state1 in
--                         allOut
--   let result = ohua.lang/nth 0 2 ctxtOut in
--   let state1 = ohua.lang/nth 1 2 ctxtOut in
--   cont
-- @
-- Note that we assign the returned state to the same variable!
-- As a result of this, a consecutive SSA pass can make sure that every
-- consecutive state thread in `cont` references the result of the ctxt
-- and *not* the initial state thread reference!
-- After SSA:
-- @
--   let ctxtOut = ctxt \d ->
--                         let x = before d in
--                         let y1 = state1.f x in
--                         let z = after y1 in
--                         let allOut = ctxtExit z state1 in
--                         allOut
--   let result = ohua.lang/nth 0 2 ctxtOut in
--   let state1' = ohua.lang/nth 1 2 ctxtOut in
--   [state |-> state']cont
-- @
transformControlStateThreads :: MonadGenBnd m => Expr ty -> m (Expr ty)
transformControlStateThreads = transformM f
  where
    f (Let v (fun@(PureFunction op _) `Apply` trueBranch `Apply` falseBranch) cont)
      | op == Refs.ifThenElse =
          let
            trueBranchStates = HS.fromList $ collectStates trueBranch
            falseBranchStates = HS.fromList $ collectStates falseBranch

            trueBranchStatesMissing = HS.difference falseBranchStates trueBranchStates
            falseBranchStatesMissing = HS.difference trueBranchStates falseBranchStates
            addMissing =
              HS.foldr
              (\missingState c ->
                 Let missingState
                 (pureFunction Refs.id Nothing (FunType [TypeVar] TypeVar) 
                   `Apply` Var missingState)
                 c)
            trueBranch' = applyToBody (`addMissing` trueBranchStatesMissing) trueBranch
            falseBranch' = applyToBody (`addMissing` falseBranchStatesMissing) falseBranch

            allStates = HS.toList $ HS.union trueBranchStates falseBranchStates
          in do
            trueBranch'' <- mkST (map Var allStates) trueBranch'
            falseBranch'' <- mkST (map Var allStates) falseBranch'
            -- Question: What's the 'ctxt_out' type supposed to be?
            ctxtOutBnd <- generateBindingWith "ctxt_out"
            let ctxtOut = TBind ctxtOutBnd TypeVar
            return $
              Let ctxtOut (fun `Apply` trueBranch'' `Apply` falseBranch'') $
              mkDestructured (v:allStates) ctxtOut
              cont
    f (Let v (fun@(PureFunction op _) `Apply` lam `Apply` ds) cont)
      | op == Refs.smap =
          let (args, expr) = lambdaArgsAndBody lam
              states = filter (not . (`HS.member` HS.fromList args)) $ collectStates expr
          in do
            expr' <- mkST (map Var states) expr
            -- Question: What's the 'ctxt_out' type supposed to be?
            ctxtOutBnd <- generateBindingWith "ctxt_out"
            let ctxtOut = TBind ctxtOutBnd TypeVar
                lam' = mkLambda args expr'
            return $
              Let ctxtOut (fun `Apply` lam' `Apply` ds) $
              mkDestructured (v:states) ctxtOut
              cont
    f expr = return expr


-- Assumptions: This function is applied to a normalized and SSAed expression.
--              This transformation executes after the control rewrites.
-- Note: The use of descend is entirely not necessary anymore once the "applicative normal form" is properly defined as a type. (see issue #8)

-- | The transformation turns the imperative into a functional state thread.
--   It transforms this:
-- @
--   let d = ctxtDataEntrance in
--   let x = before d in
--   let (state1',y1) = state1.f x in
--   let (state2',y2) = state2.f y1 in
--   let z = after 2 in
--   let allOut = ctxtExit z state1' state2' in
--   let (z', state1'', state2'') = ctxtResultExit allOut in
--   b
-- @
--   into that:
-- @
--   let d = ctxtDataEntrance in
--   let x = before d in
--   let (state1',y1) = state1.f x in
--   let (state2',y2) = state2.f y1 in
--   let z = after 2 in
--   let z' = ctxtResultExit z in
--   let state1'' = runSTCLang* state1' in
--   let state2'' = runSTCLang* state2' in
--   b
-- @
-- The goal of this transformation is to simplify fusion in the backend.
transformCtxtExits :: forall ty. Expr ty -> Expr ty
transformCtxtExits = evictOrphanedDestructured . f
    where
        f :: Expr ty -> Expr ty
        -- FIXME This does not work because the ctxtExit has a variable number of arguments! So for two arguments it actually is (Apply (Apply f x) y). Once more this is a problem of our weak definition!
--        f (Let v e@(PureFunction op _ `Apply` _) cont)
--            | op == ctxtExit =
        f l@(Let v e@(Apply _ _) cont) =
           case fromApplyToList' e of
             (FunRef op _ _, Nothing, compOut:stateArgs) | op == ctxtExit ->
               let g' = g v compOut stateArgs
                   cont' = g' cont
               in descend f cont'
             _ -> descend f l
        f e = descend f e

        g :: TypedBinding ty -> Expr ty -> [Expr ty] -> Expr ty -> Expr ty
        g compound compOut stateArgs l@(Let v e@(Apply _ _) cont) =
           case fromApplyToList' e of
             -- Must be a conditional (second ctxtExit)
             (FunRef op _ _, Nothing, compOut':stateArgs') | op == ctxtExit ->
               let h' = h compound compOut stateArgs v compOut' stateArgs'
                   cont' = h' cont
               in descend h' cont'
             -- collect case
             (fun@(FunRef op _ _), Nothing, [size, Var exitRes]) | op == Refs.collect && exitRes == compound ->
               let (compOut':stateOuts') = findDestructured cont v
                   stateExits ct =
                     foldr
                       (\(s',s) c ->
                          Let s' (runSTCLangSMapFun `Apply` size `Apply` s) c)
                       ct
                       $ zip stateOuts' stateArgs
               in
                 Let compOut' ((Lit $ FunRefLit fun) `Apply` size `Apply` compOut) $
                 stateExits
                 cont
             _ -> descend (g compound compOut stateArgs) l
        g co c s e = descend (g co c s) e

        h :: TypedBinding ty -> Expr ty -> [Expr ty]
          -> TypedBinding ty-> Expr ty -> [Expr ty]
          -> Expr ty
          -> Expr ty
        h compound compOut stateOuts _compound' compOut' stateOuts'
            (Let v (fun@(PureFunction op _) `Apply` cond `Apply` Var trueBranch `Apply` _falseBranch) cont)
            | op == Refs.select =
                let (tbCompOut, tbStateArgs, fbCompOut, fbStateArgs) =
                        if compound == trueBranch
                        then (compOut, stateOuts, compOut', stateOuts')
                        else (compOut', stateOuts', compOut, stateOuts)
                    (compOut'':stateOuts'') = findDestructured cont v
                    stateExits ct =
                        foldr
                            (\(s', (ts,fs)) c ->
                                Let s' (runSTCLangIfFun `Apply` cond `Apply` ts `Apply` fs) c)
                            ct $
                            zip stateOuts'' $
                            zip tbStateArgs fbStateArgs
                in stateExits $
                    Let compOut'' (fun `Apply` cond `Apply` tbCompOut `Apply` fbCompOut) $
                    descend f cont
        h co c s co' c' s' e = descend (h co c s co' c' s') e

        descend = over plate -- note composOp = descend = over plate -> https://www.stackage.org/haddock/lts-14.25/lens-4.17.1/Control-Lens-Plated.html#v:para (below)

applyToBody :: (Expr ty -> Expr ty) -> Expr ty -> Expr ty
applyToBody f (Lambda _ body) = applyToBody f body
applyToBody f e = f e

collectStates :: Expr ty -> [TypedBinding ty]
collectStates e = [ stb | (BindState (Var stb) _) <- universe e ]

mkST :: (MonadGenBnd m) => [Expr ty] -> Expr ty -> m (Expr ty)
mkST states = \case
    Let v e res -> Let v e <$> mkST states res
    e -> do
        allOut <- generateBindingWith "all_out"
        return $
            -- Question: What's this type supposed to be?
            Let (TBind allOut TypeVar)
                (mkApply (withSuccSing (Succ $ nlength states) ctxtExitFunRef) $ e : states)
                (Var (TBind allOut TypeVar))
