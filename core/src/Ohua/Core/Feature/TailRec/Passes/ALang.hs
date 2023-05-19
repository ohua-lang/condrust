{-|
Module      : $Header$
Description : Implementation for basic tail recrusion support.
Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
License     : EPL-1.0
Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

== Design:

The tail recursion implementation for the ohua-core compiler encompasses the following phases:


=== Phase 1: (Performed directly on the initial ALang form.)
Turn recursions into HOFs:

@
  let f' = \x1 ... xn -> ...
                           let y1 = ....
                           ...
                           let yn = ...
                           ...
                           if c then
                               result
                           else
                               recur y1 ... yn
  let f = Y f'
@

Lambda-inlinling will then just inline f' while still performing all other transformations
on it. A nice benefit: lowering for tail recursion is just an implementation of
a HigherOrderFunction lowering. As such though, we can not access the lambda, i.e., f.
So we need to do the lambda modifications on ALang before (which is nicer anyways).

We transform this into true tail recursion and hence nothing is performed on the branches.
As a result, we remove the whole if statement and just send the input to the conditional,
the output of the cycle (recursion) and the final output to the recurFun. It will use the `cond` to
understand which arcs to pull from!
Benefits: no array, left and right functions needed.

The lambda expression from phase 2 is lifted into a control context.
Normally there are two operators (ifFun/select or smapFun/collect). this time the operator
recurFun is both!

The resulting code for a call is then:

@
let result =
    let (recurCtrl,finalResult,recursionVars) = recurFun a1 ... an in
     let ctxt = ctrl recurCtrl b c d in
      let b0 = nth 0 ctxt in
       ...
        let x1 = nth 0 recursionVars in
         ...
          let y1 = ...
           ...
             if c then
                 result
             else
                 recurFun y1 ... yn
@

The `recurFun` calls now encapsulate the recursion. The `recurFun` call only returns when the recursion finished.
And then:

@
let result =
    let (recurCtrl,finalResult,recursionVars) = recurFun () () a1 ... an in
     let ctxt = ctrl recurCtrl b c d in
      let b0 = nth 0 ctxt in
       ...
        let x1 = nth 0 recursionVars in
         ...
          let y1 = ...
           ...
            recurFun c result y1 ... yn
@

Here, recurFun and recurFun are actually the same operator.
DFLowering must make that association.

Maybe it should rather be:

@
let result =
    let (recurCtrl,finalResult,recursionVars) = recurFun a1 ... an in
     let ctxt = ctrl recurCtrl b c d in
      let b0 = nth 0 ctxt in
       ...
        let x1 = nth 0 recursionVars in
         ...
          let y1 = ...
           ...
             if c then
                 fix result
             else
                 recurFun y1 ... yn
@

Using the `Y` combinator, this would be:

@

let g = \f x1 x2 x3 -> ...
                    let y1 = ...
                     ...
                      if c then
                          fix result
                      else
                          f y1 y2 y3 in

let result = Y g a b c in
  result
@

The above is an inlining of the function `g`, which is inarguably impossible
without additional semantics. That is, without knowing that the `recurFun` calls
are actually one. However, at this point, it is not lambda calculus anymore.

=== Phase 3: (Performed on the expression in ALang-normalized form (ANF)!)
RewriteAll the code (for a call) such that:

@
  let g = recur (\args -> let (x1 ... xn) = args
                              ...
                              let y1 = ....
                              ...
                              let yn = ...
                              ...
                              let e = if c then
                                        let z = right result
                                        in z
                                      else
                                        let ys = array y1 ... yn
                                        let z = left ys
                                        in z
                              in e
                )
                array a1 ... an
@

Note: y became recur
Either might be also implemented as:

@
  right a = (false, a)
  left a = (true, a)
  isRight = not . fst
  isLeft = fst
@

Phase 4: Handling free variables in the Lambda expression

The ALang phase handles free variables via lambda lifting:

@
  let g = recur (\b ->
                    let (args, freeArgs) = b
                    let (x1 ... xn) = args
                    let (a1 ... am) = freeArgs
                              ...
                              let y1 = ....
                              ...
                              let yn = ...
                              ...
                              let e = if c then
                                        let z = right result
                                        in z
                                      else
                                        let ys = array y1 ... yn
                                        let z = left ys
                                        in z
                              in e
                )
                array a1 ... an
                array arg1 ... argm
@

We again pack them in an array to make the recur operator easier to implement for a backend.
This way, it does not need to support variadic argument lists.

=== Phase 5: (DF Lowering for y.)
Adds the following operator as a context op:

@
  recur [a1 ... an] Either [y1 ... yn] result -> Either [y1 ... yn] result
@

The operator has two incoming arcs and two outgoing arcs:

  [1. incoming arc] @[a1 ... an]@
  [2. incoming arc] @Either [y1 ... yn] result@  <-- feedback edge: @e@
  [1. outgoing arc] @Either [a1 ... an] [y1 ... yn]@
  [2. outgoing arc] @result@

-}
{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Ohua.Core.Feature.TailRec.Passes.ALang where

import Ohua.Core.Prelude

import qualified Data.Text.Prettyprint.Doc as PP

import qualified Data.HashSet as HS
import Data.List.NonEmpty (fromList)

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.PPrint ()
import Ohua.Core.ALang.Passes.Control (liftIntoCtrlCtxt)
import qualified Ohua.Core.ALang.Refs as ALangRefs
import Ohua.Core.ALang.Util
    ( fromApplyToList
    , fromListToApply
    , lambdaArgsAndBody
    , mkDestructured
    )

--  ==== Implementation starts here

-- Currently not exposed by the frontend but only as the only part of recursion
-- at the backend.
recur :: QualifiedBinding
recur = ALangRefs.recur -- allows me to use it in binding position

-- This is a compiler-internal higher-order function.
recurHof :: QualifiedBinding
recurHof = "ohua.lang/recur_hof"

-- Question: What'r those types supposed to be? Should this rater be functions of VarTypes?
recurSf :: Expr ty
recurSf = pureFunction recur Nothing $ FunType [] TypeVar

recurHofSf :: Expr ty
recurHofSf = pureFunction recurHof Nothing $ FunType [] TypeVar


recurStartMarker :: QualifiedBinding
recurStartMarker = "ohua.lang.marker/recur_start"

recurEndMarker :: QualifiedBinding
recurEndMarker = "ohua.lang.marker/recur_end"

-- The Y combinator from Haskell Curry
y :: QualifiedBinding
y = "ohua.lang/Y"

ySf :: Expr ty
ySf = pureFunction y Nothing $ FunType [] TypeVar


recurFun :: QualifiedBinding
recurFun = ALangRefs.recurFunBnd

recurFunPureFunction :: Expr ty
recurFunPureFunction = pureFunction recurFun Nothing $ FunType [] TypeVar


idPureFunction :: Expr ty
idPureFunction = pureFunction "ohua.lang/id" Nothing (FunType [TypeVar] TypeVar)

-- Phase 1:
findTailRecs ::
       (Monad m, MonadGenBnd m, MonadError Error m)
    => Bool
    -> Expr ty
    -> m (Expr ty)
findTailRecs enabled e =
    snd <$> runReaderT (findRecCall e HS.empty) enabled

findRecCall ::
       (MonadGenBnd m, MonadError Error m)
    => Expr ty
    -> HS.HashSet (TypedBinding ty)
    -> ReaderT Bool m (HS.HashSet (TypedBinding ty), Expr ty)
findRecCall (Let bound expr inExpr) algosInScope
    -- for the assigment expr I add the reference and check the expression for references to the identifier
 = do
    (found, e) <- findRecCall expr $ HS.insert bound algosInScope
    -- proceed normally into the next expression
    (iFound, iExpr) <- findRecCall inExpr algosInScope
    -- did I detect a reference to this binding in the assignment expr?
    if HS.member bound found
        -- hoferize right away:
        then do
            let (TBind bnd ty) = bound
            bnd' <- generateBindingWith bnd
            return (iFound, Let (TBind bnd' ty) e $ Let bound (Apply ySf (Var (TBind bnd' ty))) iExpr)
        else return (HS.union found iFound, Let bound e iExpr)
-- findRecCall (Let bound expr inExpr) algosInScope = do
--     (iFound, iExpr) <- findRecCall inExpr algosInScope
--     return (iFound, Let bound expr iExpr)
findRecCall (Apply (Var tbnd) a) algosInScope
    | HS.member tbnd algosInScope
     -- no recursion here because if the expression is correct then these can be only nested APPLY statements
     = do
        _enabledTR <- ask
        unlessM ask $
            throwErrorDebugS
                "Detected recursion although tail recursion support is not enabled!"
        return (HS.insert tbnd HS.empty, Apply recurSf a)
            -- else error $ "Detected recursion (" ++ (show binding) ++ ") although tail recursion support is not enabled!"
findRecCall (Apply a b) algosInScope = do
    (aFound, aExpr) <- findRecCall a algosInScope
    (bFound, bExpr) <- findRecCall b algosInScope
    return (HS.union aFound bFound, Apply aExpr bExpr)
findRecCall (Lambda a e) algosInScope = do
    (eFound, eExpr) <- findRecCall e algosInScope
    return (eFound, Lambda a eExpr)
findRecCall other _ = return (HS.empty, other)


-- performed after normalization
verifyTailRecursion ::
       (MonadGenBnd m, MonadError Error m)
    => Expr ty
    -> m (Expr ty)
verifyTailRecursion expr
    | isCall y expr = performChecks (snd $ fromApplyToList expr) >> return expr
  where
    performChecks (Lambda _a e:_) = traverseToLastCall checkIf e
    performChecks e =
        throwErrorDebugS $ "Recursion is not inside a lambda but: " <> show e
    traverseToLastCall check (Let _v e ie)
        | isLastStmt ie = check e
    traverseToLastCall check (Let _v e ie) =
        failOnRecur e >> traverseToLastCall check ie
    traverseToLastCall _ e =
        throwErrorDebugS $ "Invariant broken! Found expression: " <> quickRender e
    -- failOnRecur (Let _ e ie) | isCall recur e || isCall recur ie = error "Recursion is not tail recursive!"
    failOnRecur (Let _ e ie) = failOnRecur e >> failOnRecur ie
    failOnRecur (Lambda _v e) = failOnRecur e -- TODO maybe throw a better error message when this happens
    failOnRecur (Apply (PureFunction _recur _) _) =
        error "Recursion is not tail recursive!"
    failOnRecur (Apply _ _) = return ()
    failOnRecur e = error $ "Invariant broken! Found pattern: " <> show e
    checkIf e
        | isCall "ohua.lang/if" e
      -- assumes well-structured if
         = do
            let (_:tBranch:fBranch:_) = snd $ fromApplyToList e
            let (Lambda _v et) = tBranch
            let (Lambda _v ef) = fBranch
            let lastFnOnBranch =
                    traverseToLastCall
                        (return .
                         (\(FunRef f _ _) -> f :: QualifiedBinding) .
                         fst . fromApplyToList)
            tFn <- lastFnOnBranch et
            fFn <- lastFnOnBranch ef
            when (tFn == recur) $ 
                when (fFn == recur) $
                    throwErrorDebugS
                        "Endless loop detected: Tail recursion does not have a non-recursive branch!"
            unless (fFn == recur) $
                throwErrorDebugS $
                "We currently do not support recursive calls that are located on" <>
                "nested conditional branches (#conditional branches > 1) or in" <>
                "Lambdas to other higher-order functions! Found: " <>
                show fFn <>
                " : " <>
                show tFn
    checkIf e =
        throwErrorDebugS $
        "Recursion is not tail recursive! Last stmt: " <> show (quickRender e)
    isLastStmt (Var _) = True
    isLastStmt _ = False
verifyTailRecursion e@(Let _ expr inExpr) =
    verifyTailRecursion expr >> verifyTailRecursion inExpr >> return e
verifyTailRecursion e@(Var _) = return e
verifyTailRecursion e =
    throwErrorDebugS $ "Invariant broken! Found stmt: " <> show e

-- Phase 3:

-- This is a reimplementation using `rewriteM` (ergo `plated`). The theory is
-- that this should do the recursion properly and be future proof.
--
-- Its important here that we pattern match on `Let` because `rewriteM` is a
-- bottom up traversal an hence `isCall` would match on partial applications on
-- the `Y` combinator. By pattern matching on `Let` here that can be avoided.
rewriteAll :: (MonadGenBnd m) => Expr ty -> m (Expr ty)
rewriteAll = rewriteM $ \case
    Let b e r | isCall y e -> (\e' -> Just $ Let b e' r) <$> rewriteCallExpr e
    _ -> pure Nothing

isCall :: QualifiedBinding -> Expr ty -> Bool
isCall f (Apply (PureFunction f' _) _) | f == f' = True
isCall f (Apply e@(Apply _ _) _) = isCall f e
isCall _ _ = False

rewriteCallExpr :: forall m ty.
       (MonadGenBnd m) => Expr ty -> m (Expr ty)
rewriteCallExpr e = do
    let (lam@(Lambda _ _):callArgs) = snd $ fromApplyToList e
    recurCtrlBnd <- generateBindingWith "ctrl"
    let recurCtrl = (TBind recurCtrlBnd controlSignalType)
    lam' <- liftIntoCtrlCtxt recurCtrl lam
    let (recurVars, expr) = lambdaArgsAndBody lam'

    let expr' = rewriteLastCond expr
  --   [ohualang|
  --     let (recurCtrl, b1 , ..., bn) = recurFun () () a1 ... an in
  --      let ctxt = ctrl recurCtrl b c d in
  --       let b0 = nth 0 ctxt in
  --        ...
  --         let x1 = nth 0 recursionVars in
  --          ...
  --           let y1 = ...
  --            ...
  --             let r = recurFun c result y1 ... yn in
  --               r
  -- this breaks haddock |]
  -- Reminder: I assume from the name that this is supposed to be 
    ctrlsBnd <- generateBindingWith "ctrls"
    let ctrls = TBind ctrlsBnd (TupleTy $ controlSignalType :| [controlSignalType])
        argTypes = case callArgs of
            [] -> error "I don't know whats wrong but args must not be empty here"
            _args ->  genFunType callArgs
    return $
        Let ctrls (fromListToApply (FunRef recurStartMarker Nothing $ argTypes) callArgs) $
        mkDestructured (recurCtrl : recurVars) ctrls expr'
  where
    rewriteLastCond :: Expr ty -> Expr ty
    rewriteLastCond (Let v ex o@(Var b))
        | v == b = Let v (rewriteCond ex) o
        | otherwise = error "Value returned from recursive function was not last value bound, this is not tail recursive!"
    rewriteLastCond (Let v ex ie) = Let v ex $ rewriteLastCond ie

    -- Question: What's the return type supposed to be?
    genFunType callArgs =  FunType (map exprType callArgs) TypeVar

    -- This whole rewriteCond and rewriteBranch algorithm is not correct. That
    -- is to say it only works in the specific case where a single `if` is the
    -- last expression in a recursive function. While the reason for this
    -- assumption is obvious we must consider that also nested `if`'s
    -- technically are valid tail recursive functions so long as there is at
    -- least one branch that recurses and one branch that does not. I feel
    -- implementing this correctly however is going to require some effort, thus
    -- I think we should do so later.
    rewriteCond :: Expr ty -> Expr ty
    rewriteCond fullExpr@(Apply (Apply (Apply (PureFunction f0 _) cond) (Lambda _ trueB)) (Lambda _ falseB)) | f0 == ALangRefs.ifThenElse =
        let trueB' = rewriteBranch trueB
            falseB' = rewriteBranch falseB
            (fixRef, recurVars) =
                case (trueB', falseB') of
                    (Left _f, Right _bnds) -> errorD $ flexText "I am sorry, but for now the recursion is required to be on the first (`then`) branch of the final condition. This is a bug of the implementation and will be fixed in the future. (Issue #36)\n\nYour code violating this invariant was\n" <> PP.indent 4 (PP.pretty fullExpr) -- (f, bnds)
                    (Right bnds, Left f) -> (f, bnds)
                    _ -> error "invariant broken"
            callArgs = cond : fixRef : recurVars
         in fromListToApply (FunRef recurEndMarker Nothing $ genFunType callArgs) callArgs
            
    rewriteCond _ =
        error
            "invariant broken: recursive function does not have the proper structure."
    rewriteBranch :: Expr ty -> Either (Expr ty) [Expr ty]
    -- normally this is "fix" instead of `id`
    rewriteBranch (Let _v (Apply (PureFunction "ohua.lang/id" _) result) _) = Left result
    rewriteBranch (Let _v ex _)
        | isCall recur ex = (Right . snd . fromApplyToList) ex
    rewriteBranch ex = error $ "invariant broken: " <> quickRender ex
