-- |
-- Module      : $Header$
-- Description : Definition of an abstract expression language as the first IR for the Ohua compiler.
-- Copyright   : (c) Sebastian Ertel, Justus Adam 2017. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science, sebastian.ertel@gmail.com
-- Stability   : experimental
-- Portability : portable

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
--
-- Passes required to transform an expression in ALang into an expression in DFLang.
--
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.DFLang.Passes where


import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
--import           Control.Monad.Trans.Reader
import           Data.Either
import           Data.Foldable
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence        (Seq, (><), (|>))
import qualified Data.Sequence        as S
import           Lens.Micro
import           Ohua.ALang.Lang
import           Ohua.DFLang.TailRec  (recursionLowering)
import           Ohua.DFLang.HOF      as HOF
import           Ohua.DFLang.HOF.If
import           Ohua.DFLang.HOF.Seq
import           Ohua.DFLang.HOF.Smap
import           Ohua.DFLang.Lang     (DFExpr (..), DFFnRef (..), DFVar (..),
                                       LetExpr (..))
import           Ohua.DFLang.Util
import           Ohua.Monad
import           Ohua.Types
import           Ohua.Util


type Pass m = FnName -> FnId -> Assignment -> [Expression] -> m (Seq LetExpr)

data AlgoSpec m = AlgoSpec { formalInputVars :: [Binding],
                             dfExpr :: m DFExpr }

newtype LetRec m = LetRec { unLetRec :: HM.HashMap Binding (AlgoSpec m)}

type LetRecT m a = ReaderT (LetRec m) m a


-- | Check that a sequence of let expressions does not redefine bindings.
checkSSA :: (Foldable f, MonadError String m) => f LetExpr -> m ()
checkSSA = flip evalStateT mempty . mapM_ go
  where
    go le = do
        defined <- get
        let produced = flattenAssign (returnAssignment le)
            f a | HS.member a defined = Just a
            f _ = Nothing
        case msum $ map f produced of
            Just b  -> throwError $ "Rebinding of " ++ show b
            Nothing -> return ()
        modify (addAll produced)

    addAll = flip $ foldr' HS.insert


-- | Check that a DFExpression is in SSA form.
checkSSAExpr :: MonadError String m => DFExpr -> m ()
checkSSAExpr (DFExpr l _) = checkSSA l

-- | Transform an ALang expression into a DFExpression.
-- This assumes a certain structure in the expression.
-- This can be achieved with the 'normalize' and 'performSSA' functions and tested with
-- 'checkProgramValidity'.
lowerALang :: (MonadOhua m, MonadError String m) => Expression -> m DFExpr
lowerALang expr = do
    (var, exprs) <- runWriterT $ (flip runReaderT (LetRec mempty) . lowerToDF) expr
--    (var, exprs) <- runWriterT (go expr)
#ifdef DEBUG
    checkSSA exprs
#endif
    return $ DFExpr exprs var

lowerToDF :: (MonadOhua m, MonadError String m, MonadWriter (Seq LetExpr) m) => Expression -> LetRecT m Binding
lowerToDF  (Var (Local bnd)) = return bnd
lowerToDF  (Var _) = lift $ throwError "Non local return binding"
lowerToDF  (Let assign expr rest) = handleDefinitionalExpr assign expr continuation
  where
    continuation = lowerToDF rest
lowerToDF  _ = lift $ throwError "Expected `let` or binding"


handleDefinitionalExpr :: (MonadOhua m, MonadError String m, MonadWriter (Seq LetExpr) m) =>
                          Assignment -> Expression -> LetRecT m Binding -> LetRecT m Binding
handleDefinitionalExpr assign l@(Lambda arg _) cont = do
    -- TODO handle lambdas with multiple arguments
    let unAssign x = case x of { Direct b -> b; _ -> error "Invariant broken"}
    -- execute the rest of the traversal (the continuation) in the new LetRecT environment
    local (LetRec . HM.insert (unAssign assign) (AlgoSpec [unAssign arg] $ lowerLambdaExpr assign l) . unLetRec) cont
handleDefinitionalExpr assign l@(Apply _ _) cont = do
    let go (fn, fnId, args) = case assign of
                                  (Recursive binding) -> lift $ tell =<< (recursionLowering binding =<< lowerDefault fn fnId assign args)
                                  _ -> case HM.lookup fn hofNames of
                                          Just (WHOF (_ :: Proxy p)) -> lift $ lowerHOF (name :: TaggedFnName p) assign args
                                          Nothing                    -> lift $ tell =<< lowerDefault fn fnId assign args
    go =<< handleApplyExpr assign l
    cont
handleDefinitionalExpr _ e _ = lift $ throwError $ "Definitional expressions in a let can only be 'apply' or 'lambda' but got: " ++ show e

-- | Lower any not specially treated function type.
lowerDefault :: (MonadOhua m, MonadError String m) => Pass m
lowerDefault fn fnId assign args = mapM expectVar args <&> \args' -> [LetExpr fnId assign (EmbedSf fn) args' Nothing]

-- | Analyze an apply expression, extracting the inner stateful function and the nested arguments as a list.
-- Also generates a new function id for the inner function should it not have one yet.
handleApplyExpr :: (MonadOhua m, MonadError String m, MonadWriter (Seq LetExpr) m) =>
                   Assignment -> Expression -> LetRecT m (FnName, FnId, [Expression])
handleApplyExpr assign l@(Apply fn arg) = --go l [] <$> ask --(return . lift . go l []) =<< ask
--  let go :: Expression -> [Expression] -> m (FnName, FnId, [Expression])
--  where
  let
    certainly = fromMaybe $ error "Invariant broken"
    unwrapVar x = case x of { (Local b) -> b; _ -> error "Invariant broken"; }
    go (Var (Sf fn id))               args _        = (fn, , args) <$> maybe generateId return id
            -- reject algos for now
    go (Var v)                        args recAlgos =
          if HM.member (unwrapVar v) recAlgos then lowerRecAlgoCall (certainly $ HM.lookup (unwrapVar v) recAlgos) args assign
                                              else throwError $ "Expected Var Sf but got: Var " ++ show v -- FIXME there should be a special type of error here that takes the string and a value
    go (Apply fn arg@(Var resSymbol)) args recAlgos = go fn (arg:args) recAlgos
    go (Apply fn arg)                 args _        = throwError $ "Arg to apply should have been reduced to Var or EnvVar before df lowering. Found: " ++ show arg
    go x                              _    _        = throwError $ "Expected Apply or Var but got: " ++ show x in
    do
      recAlgos <- ask
      go l [] $ unLetRec recAlgos
handleApplyExpr _ (Var (Sf fn id))  = (fn, , []) <$> maybe generateId return id -- what is this?
handleApplyExpr _ g                 = lift $ throwError $ "Expected apply but got: " ++ show g

lowerRecAlgoCall :: (MonadOhua m, MonadError String m, MonadWriter (Seq LetExpr) m) =>
                            AlgoSpec m -> [Expression] -> Assignment -> LetRecT m (FnName, FnId, [Expression])
lowerRecAlgoCall algoFormals actuals callAssignment =
  let inputFormals = formalInputVars algoFormals
      mkIdFn id i o = lowerDefault "ohua.lang/id" id o [i] in
      do
        -- input side
        mapM_ (uncurry (\x y -> do
          id <- generateId
          tell =<< mkIdFn id x y)) $ zip actuals $ map Direct inputFormals

        -- recreate the body and 'tell' it to the writer
        ls <- lift $ dfExpr algoFormals
        tell $ letExprs ls

        -- output side
        id <- generateId

        return ("ohua.lang/id", id, [Var $ Local $ returnVar ls])


-- | Analyze a lambda expression. Since we perform lambda inlining, this can only be a letrec.
lowerLambdaExpr :: (MonadOhua m, MonadError String m) => Assignment -> Expression -> m DFExpr
lowerLambdaExpr (Recursive binding) expr = lowerALang expr
lowerLambdaExpr a _ = throwError $ "Expression was not inlined but assignment is not a 'letrec': " ++ show a

-- | Inspect an expression expecting something which can be captured in a DFVar otherwies throws appropriate errors.
expectVar :: MonadError String m => Expression -> m DFVar
expectVar (Var (Local bnd)) = pure $ DFVar bnd
expectVar (Var (Env i))     = pure $ DFEnvVar i
expectVar (Var _)           = throwError "Var must be local or env"
expectVar _                 = throwError "Argument must be var"

-- FIXME is this function still used? if not delete it!
-- finds all functions that use vars from the lexical context and adds the context source to them.
-- needs to do the following two things
--        1. contextify all functions that do not have a bound arg -> context arg
--        2. provide lexical scope access to these args
tieContext :: (Functor f, Foldable f) => Binding -> f LetExpr -> f LetExpr
tieContext ctxSource exprs = tieContext0 bounds ctxSource exprs
  where
    bounds = findBoundVars exprs


tieContext0 :: Functor f => HS.HashSet Binding -> Binding -> f LetExpr -> f LetExpr
tieContext0 bounds ctxSource = fmap go
  where
    go e | any isBoundArg (callArguments e) = e
    go e = e { contextArg = Just ctxSource }

    isBoundArg (DFVar v) = v `HS.member` bounds
    isBoundArg _         = False

lowerHOF :: forall f m . (MonadError String m, MonadOhua m, HigherOrderFunction f, MonadWriter (Seq LetExpr) m)
         => TaggedFnName f -> Assignment -> [Expression] -> m ()
lowerHOF _ assign args = do
    simpleArgs <- mapM handleArg args
    let newFnArgs = map (either Variable LamArg . fmap fst) simpleArgs
    f <- HOF.parseCallAndInitState newFnArgs :: m f
    flip evalStateT f $ do
        createContextEntry >>= tell
        let lambdas = rights simpleArgs
        for_ lambdas $ \(lam, body) -> do
            let boundVars = findBoundVars body `mappend` HS.fromList (flattenAssign $ beginAssignment lam)
            let freeVars = HS.toList $ findFreeVars0 boundVars body
            (scopers, renaming) <- scopeFreeVariables lam freeVars
            tell scopers
            scopeUnbound <- contextifyUnboundFunctions lam
            let tieContext1 = case scopeUnbound of
                    Just bnd -> tieContext0 boundVars bnd
                    Nothing  -> id
            tell $ tieContext1 (renameWith (HM.fromList renaming) body)
        createContextExit assign >>= tell
  where
    handleArg (Var (Local v)) = return $ Left $ DFVar v
    handleArg (Var (Env e)) = return $ Left $ DFEnvVar e
    -- FIXME I think this can not happen anymore.
    handleArg (Lambda assign body) = do
        DFExpr lets bnd <- lowerALang body
        return $ Right (Lam assign bnd, lets)
    handleArg _ = throwError "unexpected type of argument"


hofs :: [WHOF]
hofs =
    [ WHOF (Proxy :: Proxy IfFn)
    , WHOF (Proxy :: Proxy SmapFn)
    , WHOF (Proxy :: Proxy SeqFn)
    ]

hofNames :: HM.HashMap FnName WHOF
hofNames = HM.fromList $ map (extractName &&& id) hofs
  where extractName (WHOF (_ :: Proxy p)) = unTagFnName $ (name :: TaggedFnName p)
