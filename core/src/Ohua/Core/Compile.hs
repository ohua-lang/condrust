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

import Control.Lens (view)
import Ohua.Core.ALang.Lang as ALang
import Ohua.Core.ALang.Passes
import Ohua.Core.ALang.Passes.SSA
import Ohua.Core.Feature.TailRec (loadTailRecPasses)
import Ohua.Core.ALang.Passes.Verify
import Ohua.Core.ALang.Refs as Refs
import Ohua.Core.Compile.Configuration
import Ohua.Core.DFLang.PPrint ()
import Ohua.Core.DFLang.Passes
import Ohua.Core.DFLang.Lang
import Ohua.Core.Stage
import Ohua.Core.Feature.TailRec.Passes.ALang (y)


forceLog :: (MonadLogger m, NFData a) => Text -> a -> m ()
forceLog msg a = a `deepseq` logDebugN msg

-- | The canonical order of transformations and lowerings performed in a full compilation.
pipeline :: CustomPasses -> ALang.Expr ty -> OhuaM (NormalizedDFExpr ty)
pipeline CustomPasses {..} e = do
    stage resolvedAlang e
    ssaE <- performSSA e
    stage ssaAlang ssaE
    normalizedE <- normalize =<< passBeforeNormalize ssaE
    stage normalizedAlang normalizedE
    whenDebug $ do
        checkProgramValidity normalizedE
        checkHigherOrderFunctionSupport normalizedE
        Ohua.Core.ALang.Passes.SSA.checkSSA normalizedE
    customAfterNorm <- passAfterNormalize normalizedE
    stage customAlangPasses customAfterNorm
    coreE <- Ohua.Core.ALang.Passes.runCorePasses =<< normalize customAfterNorm
    stage coreAlang coreE
    whenDebug $ do
        Ohua.Core.ALang.Passes.SSA.checkSSA coreE
        Ohua.Core.ALang.Passes.Verify.checkInvariants coreE
    dfE <- lowerToDF =<< normalize coreE
    stage initialDflang dfE
    -- Ohua.Core.DFLang.Verify.verify dfE
    whenDebug $ Ohua.Core.DFLang.Passes.checkSSA dfE
    coreDfE <- Ohua.Core.DFLang.Passes.runCorePasses dfE
    stage coreDflang coreDfE
    dfAfterCustom <- passAfterDFLowering coreDfE
    stage customDflang dfAfterCustom
    whenDebug $ Ohua.Core.DFLang.Passes.checkSSA dfAfterCustom
    pure coreDfE

-- | Run the pipeline in an arbitrary monad that supports error reporting.
compile :: CompM m => Options -> CustomPasses -> ALang.Expr ty -> m (NormalizedDFExpr ty)
compile opts passes exprs = do
    logFn <- askLoggerIO
    let passes' =
            flip loadTailRecPasses passes $
            view transformRecursiveFunctions opts
    either throwError pure =<<
        liftIO (runLoggingT (runFromExpr opts (pipeline passes') exprs) logFn)

hofNames :: HashSet QualifiedBinding
hofNames = HS.fromList [Refs.smap, Refs.ifThenElse, Refs.seq, Refs.recur, y]

-- FIXME I don't think this is needed anymore once issue #8 is resolved.
-- | Verify that only higher order functions have lambdas as arguments
checkHigherOrderFunctionSupport :: MonadOhua m => ALang.Expr ty -> m ()
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
    checkNestedExpr (PureFunction n _) = pure $ HS.member n hofNames
    checkNestedExpr (ALang.Var _) = pure False
    checkNestedExpr a = failWith $ "Expected var or apply expr, got " <> show a
    isLambda (Lambda _ _) = True
    isLambda _ = False
checkHigherOrderFunctionSupport (ALang.Var _) = pure ()
checkHigherOrderFunctionSupport a =
    failWith $ "Expected let or var, got " <> show a
