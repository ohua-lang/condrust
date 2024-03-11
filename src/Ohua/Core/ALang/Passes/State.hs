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

-- import Ohua.Commons.Types.Vector hiding (map, zip, zip3, unzip, unzip3, filter, toList)
import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Util
import Ohua.Core.ALang.Passes.SSA
import qualified Ohua.Core.InternalFunctions as IFuns

import Ohua.Core.ALang.PPrint ()

import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Control.Lens.Combinators (over)
import Control.Lens.Plated (plate)



-- TODO recursion support is still pending here!

-- | The SSA pass after transforming control state threads is vital.
--   It makes sure that all succeeding state threads after a control
--   state thread, take the state outputs of the control state threads.
preControlPasses :: MonadOhua m => Expr embExpr annot ty -> m (Expr embExpr annot ty)
preControlPasses =
  transformControlStateThreads >=>
  performSSA >=>
  transformFundamentalStateThreads >=>
  performSSA

-- FIXME there is code missing that turns the basic state threads into real state threads! this is the very first transformation that needs to run.
-- the transformation is: make state threads and then rebind the state -> alpha renaming on the rest of the term.

postControlPasses :: Expr embExpr annot ty -> Expr embExpr annot ty
postControlPasses = transformCtxtExits -- . traceShow "transforming ctxt exists!" -- TODO assert here that no more ctxt exits remain in the code


runSTCLangSMapFun :: OhuaType ty Resolved -> Expr embExpr annot ty
runSTCLangSMapFun stateTy = Lit $ FunRefLit $ FunRef IFuns.runSTCLangSMap $ FunType (Right $ IType TypeNat :| [stateTy]) stateTy -- size and the state, i.e., there is one per state


runSTCLangIfFun :: OhuaType ty Resolved -> Expr embExpr annot ty
runSTCLangIfFun stateTy = Lit $ FunRefLit $ FunRef IFuns.runSTCLangIf $ FunType (Right $ IType TypeBool :| [stateTy, stateTy]) stateTy

-- invariant: this type of node has at least one var as input (the computation result)
ctxtExit :: QualifiedBinding
ctxtExit = "ohua.lang/ctxtExit"

-- To type-encode the assumption of at least one input a Nonempty is sufficient. We don't need a 'Nat for this.
ctxtExitFunRef :: NE.NonEmpty (OhuaType ty Resolved)  -> OhuaType ty Resolved -> Expr embExpr annot ty
ctxtExitFunRef  inputTys retTy = Lit $ FunRefLit $ FunRef ctxtExit $ FunType (Right inputTys) retTy

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
transformFundamentalStateThreads :: (MonadOhua m) => Expr embExpr annot ty -> m (Expr embExpr annot ty)
transformFundamentalStateThreads = transformM f
  where
    f e@(Let v app cont) = do
      s <- stBndForStatefulFun app
      let s' = usedIn cont =<< s
      maybe (return e) (g v app cont) s'
    f expr = return expr

    usedIn cont b@(bnd, _) =
      case [b' | Var b' <- universe cont, b' == bnd] of
        [] -> Nothing
        _ -> Just b

    stBndForStatefulFun (StatefulFunction _ (Var tBnd@(TBind bnd ty))) = do Just . (\newBnd -> (tBnd, TBind newBnd ty)) <$> generateBindingWith bnd
    -- FIXME Once again, this stupid over-generalization of the language is a pain!
    stBndForStatefulFun e@(StatefulFunction {}) = error $ "state should have been var: " <> show e
    stBndForStatefulFun (e1 `Apply` _)            = stBndForStatefulFun e1
    stBndForStatefulFun _                         = return Nothing

    g result app cont (TBind stateIn sty, stateOut) = do
      xBnd <- generateBinding
      let x = TBind xBnd (TType (sty:|[ asType result]))
      return $
        Let x app $
        destructure (Var x) [stateOut, result] $
        substitute stateIn (Var stateOut) cont

-- ToDo: do not introduce orphaned state, i.e., state that is never used again.
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
transformControlStateThreads :: MonadGenBnd m => Expr embExpr annot ty -> m (Expr embExpr annot ty)
transformControlStateThreads = transformM f
  where
    f (Let v (fun@(PureFunction op) `Apply` trueBranch `Apply` falseBranch) cont)
      | op == IFuns.ifThenElse =
          let
            trueBranchStates = HS.fromList $ collectStates trueBranch
            falseBranchStates = HS.fromList $ collectStates falseBranch

            trueBranchStatesMissing = HS.difference falseBranchStates trueBranchStates
            falseBranchStatesMissing = HS.difference trueBranchStates falseBranchStates
            addMissing =
              HS.foldr
              (\missingState c ->
                 Let missingState
                 (pureFunction IFuns.id (FunType (Right $ asType missingState :| []) (asType missingState))
                   `Apply` Var missingState)
                 c)
            trueBranch' = applyToBody (`addMissing` trueBranchStatesMissing) trueBranch
            falseBranch' = applyToBody (`addMissing` falseBranchStatesMissing) falseBranch
            allStates = HS.toList $ HS.union trueBranchStates falseBranchStates
            stateThreadingReTy = TType (exprType trueBranch :| map asType allStates)
          in do
            trueBranch'' <- mkST (map Var allStates) trueBranch'
            falseBranch'' <- mkST (map Var allStates) falseBranch'
            --- ToDo: See description above, ctxt_out is (result, state')
            ctxtOutBnd <- generateBindingWith "ctxt_out"
            let ctxtOut = TBind ctxtOutBnd stateThreadingReTy 
            return $
              Let ctxtOut (fun `Apply` trueBranch'' `Apply` falseBranch'') $
              mkDestructured (v:allStates) ctxtOut
              cont
    f (Let v (fun@(PureFunction op) `Apply` lam `Apply` ds) cont)
      | op == IFuns.smap =
          let (args, expr) = lambdaArgsAndBody lam
              states = filter (not . (`HS.member` HS.fromList args)) $ collectStates expr
          in do
            expr' <- mkST (map Var states) expr
            -- ToDo: See description above, ctxt_out is (result, state')
            ctxtOutBnd <- generateBindingWith "ctxt_out"
            let resAndStatesTys = exprType lam :| map asType states
                ctxtOut = TBind ctxtOutBnd (TType resAndStatesTys)
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
transformCtxtExits :: forall embExpr annot ty. Expr embExpr annot ty -> Expr embExpr annot ty
transformCtxtExits = evictOrphanedDestructured . f
    where
        f :: Expr embExpr annot ty -> Expr embExpr annot ty
        -- FIXME This does not work because the ctxtExit has a variable number of arguments! So for two arguments it actually is (Apply (Apply f x) y). Once more this is a problem of our weak definition!
--        f (Let v e@(PureFunction op _ `Apply` _) cont)
--            | op == ctxtExit =
        f l@(Let v e@(Apply _ _) cont) =
           case fromApplyToList' e of
             (FunRef op _, Nothing, compOut:stateArgs) | op == ctxtExit ->
               let g' = g v compOut stateArgs
                   cont' = g' cont
               in descend f cont'
             _ -> descend f l
        f e = descend f e

        g :: TypedBinding ty -> Expr embExpr annot ty -> [Expr embExpr annot ty] -> Expr embExpr annot ty -> Expr embExpr annot ty
        g compound compOut stateArgs l@(Let v e@(Apply _ _) cont) =
           case fromApplyToList' e of
             -- Must be a conditional (second ctxtExit)
             (FunRef op _, Nothing, compOut':stateArgs') | op == ctxtExit ->
               let h' = h compound compOut stateArgs v compOut' stateArgs'
                   cont' = h' cont
               in descend h' cont'
             -- collect case
             (fun@(FunRef op _), Nothing, [size, Var exitRes]) | op == IFuns.collect && exitRes == compound ->
               let (compOut':stateOuts') = findDestructured cont v
                   stateExits ct =
                     foldr
                       (\(s',s) c ->
                          Let s' (runSTCLangSMapFun (exprType s) `Apply` size `Apply` s) c)
                       ct
                       $ zip stateOuts' stateArgs
               in
                 Let compOut' (Lit (FunRefLit fun) `Apply` size `Apply` compOut) $
                 stateExits
                 cont
             _ -> descend (g compound compOut stateArgs) l
        g co c s e = descend (g co c s) e

        h :: TypedBinding ty -> Expr embExpr annot ty -> [Expr embExpr annot ty]
          -> TypedBinding ty-> Expr embExpr annot ty -> [Expr embExpr annot ty]
          -> Expr embExpr annot ty
          -> Expr embExpr annot ty
        h compound compOut stateOuts _compound' compOut' stateOuts'
            (Let v (fun@(PureFunction op) `Apply` cond `Apply` Var trueBranch `Apply` _falseBranch) cont)
            | op == IFuns.select =
                let (tbCompOut, tbStateArgs, fbCompOut, fbStateArgs) =
                        if compound == trueBranch
                        then (compOut, stateOuts, compOut', stateOuts')
                        else (compOut', stateOuts', compOut, stateOuts)
                    (compOut'': stateOuts'') = findDestructured cont v
                    stateExits ct =
                        foldr
                            (\(s', (ts,fs)) c ->
                                Let s' (runSTCLangIfFun (exprType ts)`Apply` cond `Apply` ts `Apply` fs) c)
                            ct $
                            zip stateOuts'' $
                            zip tbStateArgs fbStateArgs
                in stateExits $
                    Let compOut'' (fun `Apply` cond `Apply` tbCompOut `Apply` fbCompOut) $
                    descend f cont
        h co c s co' c' s' e = descend (h co c s co' c' s') e

        descend = over plate -- note composOp = descend = over plate -> https://www.stackage.org/haddock/lts-14.25/lens-4.17.1/Control-Lens-Plated.html#v:para (below)

applyToBody :: (Expr embExpr annot ty -> Expr embExpr annot ty) -> Expr embExpr annot ty -> Expr embExpr annot ty
applyToBody f (Lambda _ body) = applyToBody f body
applyToBody f e = f e

collectStates :: Expr embExpr annot ty -> [TypedBinding ty]
collectStates e = [ stb | (BindState (Var stb) _) <- universe e ]

mkST :: (MonadGenBnd m) => [Expr embExpr annot ty] -> Expr embExpr annot ty -> m (Expr embExpr annot ty)
mkST states = \case
    Let v e res -> Let v e <$> mkST states res
    e -> do
        allOut <- generateBindingWith "all_out"
        let etypes =  exprType e :| map exprType states
        return $
            -- allOut contains the result an ALL states from the inner scope
            -- ctxtExitFun is a function that collects the result and all states and returns the tuple for 'allOut'
            Let (TBind allOut (TType etypes))
                (mkApply (ctxtExitFunRef etypes (TType etypes)) $ e : states)
                (Var (TBind allOut (TType etypes)))
