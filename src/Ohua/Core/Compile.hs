-- |
-- Module      : $Header$
-- Description : The compiler pipeline
-- Copyright   : (c) Sebastian Ertel and Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : sebastian.ertel@gmail.com, dev@justus.science
-- Stability   : experimental
-- Portability : portable
-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Ohua.Core.Compile where

import Ohua.Core.Prelude

import qualified Data.HashSet as HS

import Ohua.Core.ALang.Lang as ALang
import qualified Ohua.Core.ALang.Passes as APasses
import qualified Ohua.Core.ALang.Passes.SSA as SSA
import Ohua.Core.Feature.TailRec (loadTailRecPasses)
import qualified Ohua.Core.ALang.Passes.Verify as AVerify
import Ohua.Core.InternalFunctions as IFuns
import Ohua.Core.Compile.Configuration
import Ohua.Core.DFLang.PPrint ()
import qualified Ohua.Core.DFLang.Passes as DFPasses
import Ohua.Core.DFLang.Lang
import Ohua.Core.Stage
import Ohua.Core.Feature.TailRec.Passes.ALang (y)


forceLog :: (MonadLogger m, NFData a) => Text -> a -> m ()
forceLog msg a = a `deepseq` logDebugN msg

-- | The canonical order of transformations and lowerings performed in a full compilation.
pipeline :: (Show embExpr) => CustomPasses embExpr ty -> ALang.Expr embExpr ty -> OhuaM (NormalizedDFExpr embExpr ty)
pipeline CustomPasses {..}  e = do
    stage resolvedAlang e
    ssaE <- SSA.performSSA e
    stage ssaAlang ssaE
    normalizedE <- APasses.normalize =<< passBeforeNormalize ssaE
    stage normalizedAlang normalizedE
    whenDebug $ do
        APasses.checkProgramValidity normalizedE
        checkHigherOrderFunctionSupport normalizedE
        SSA.checkSSA normalizedE
    customAfterNorm <- passAfterNormalize normalizedE
    stage customAlangPasses customAfterNorm
    coreE <- APasses.runCorePasses =<< APasses.normalize customAfterNorm
    stage coreAlang coreE
    whenDebug $ do
        SSA.checkSSA coreE
        AVerify.checkInvariants coreE
    dfE <- DFPasses.lowerToDF =<< APasses.normalize coreE
    stage initialDflang dfE
    -- Ohua.Core.DFLang.Verify.verify dfE
    whenDebug $ DFPasses.checkSSA dfE
    coreDfE <- DFPasses.runCorePasses dfE
    stage coreDflang coreDfE
    dfAfterCustom <- passAfterDFLowering coreDfE
    stage customDflang dfAfterCustom
    whenDebug $ DFPasses.checkSSA dfAfterCustom
    dfFinal <- DFPasses.finalPasses dfAfterCustom
    stage finalDflang dfFinal
    whenDebug $ DFPasses.checkSSA dfFinal
    pure dfFinal

-- | Run the pipeline in an arbitrary monad that supports error reporting.
compile :: (ErrAndLogM m, Show embExpr) => Options -> CustomPasses embExpr ty -> ALang.Expr embExpr ty-> m (NormalizedDFExpr embExpr ty)
compile opts passes expr = do
    logFn <- askLoggerIO
    let passes' =
            flip loadTailRecPasses passes $
            view transformRecursiveFunctions opts
    either throwError pure =<<
        liftIO (runLoggingT (runFromExpr opts (pipeline passes') expr ) logFn)



hofNames :: HashSet QualifiedBinding
hofNames = HS.fromList [IFuns.smap, IFuns.ifThenElse, IFuns.seq, IFuns.recur, y]

-- FIXME I don't think this is needed anymore once issue #8 is resolved.
-- | Verify that only higher order functions have lambdas as arguments
checkHigherOrderFunctionSupport :: MonadOhua m => ALang.Expr embExpr ty -> m ()
checkHigherOrderFunctionSupport (ALang.Let _ e rest) = do
    void $ checkNestedExpr e
    checkHigherOrderFunctionSupport rest
  where
    checkNestedExpr (Apply f arg) = do
        supportsHOF <- checkNestedExpr f
        when (isLambda arg && not supportsHOF) $
            failWith $
            "Lambdas may only be input to higher order functions, not " <>
            show f
        pure True
    checkNestedExpr (PureFunction n) = pure $ HS.member n hofNames
    checkNestedExpr (ALang.Var _) = pure False
    checkNestedExpr (ALang.BindState e1 e2) = checkNestedExpr e1 >> checkNestedExpr e2
    checkNestedExpr a = failWith $ "Expected var or apply expr, got " <> show a
    isLambda (Lambda _ _) = True
    isLambda _ = False
checkHigherOrderFunctionSupport (ALang.Var _) = pure ()
checkHigherOrderFunctionSupport a =
    failWith $ "Expected let or var, got " <> show a
