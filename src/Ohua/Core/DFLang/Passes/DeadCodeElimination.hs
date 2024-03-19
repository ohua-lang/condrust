{-# LANGUAGE ScopedTypeVariables, NoOverloadedLists #-}

module Ohua.Core.DFLang.Passes.DeadCodeElimination where

import qualified Data.HashSet as HS
import Data.Text.Lazy.IO as T
import Data.Tuple.Extra (fst3)
import Ohua.Core.DFLang.Lang as L
import Ohua.Core.Prelude
import qualified Ohua.Commons.Types.Vector as V

eliminate :: (MonadOhua m) => NormalizedDFExpr embExpr annot ty -> m (NormalizedDFExpr embExpr annot ty)
eliminate expr = do
  expr' <- (eliminateExprs . eliminateOuts) expr
  (if L.countBindings expr == L.countBindings expr' then pure expr' else eliminate expr')

eliminateExprs :: forall m embExpr annot ty. (MonadOhua m) => NormalizedDFExpr embExpr annot ty -> m (NormalizedDFExpr embExpr annot ty)
eliminateExprs expr = do
  expr' <- eliminateDeadExprs expr
  (if L.length expr == L.length expr' then pure expr' else eliminateExprs expr')
  where
    eliminateDeadExprs = transformExprM f

    f :: NormalizedDFExpr embExpr annot ty -> m (NormalizedDFExpr embExpr annot ty)
    f (Let app@(PureDFFun _ out (FunRef fun _) _) cont) = do
      (if isUsed out then pure $ Let app cont else warn fun >> return cont)
    f e = pure e

    isUsed :: OutData a ty -> Bool
    isUsed out = any (`HS.member` (HS.fromList $ usedBindings expr)) $ toOutBnds out

    warn :: QualifiedBinding -> m ()
    warn (QualifiedBinding (NSRef ["ohua","lang"]) _) = return ()
    warn fun = warning fun

    warning :: QualifiedBinding -> m ()
    warning fun = liftIO $ T.putStrLn $ "[WARNING] The output of pure function '" <> show fun <> "' is not used. As such, it will be deleted. If the function contains side-effects then this function actually wants to be stateful!"

eliminateOuts :: forall embExpr annot ty. NormalizedDFExpr embExpr annot ty -> NormalizedDFExpr embExpr annot ty
eliminateOuts expr = mapFuns go expr
  where
    go :: forall (f::FunANF). DFApp f embExpr annot ty -> DFApp f embExpr annot ty
    go (RecurFun c ctrlOut outArgs initArgs inArgs cond result) = do
      let ctrlOut' = filterOutData =<< ctrlOut
      case V.zip3 outArgs initArgs inArgs of
        zipped ->
          case V.filter (isJust . filterOutData . fst3) zipped of
            V.MkEV filtered ->
              case V.unzip3 filtered of
                (outArgs', initArgs', inArgs') ->
                  RecurFun c ctrlOut' outArgs' initArgs' inArgs' cond result
    go (SMapFun (dOut, ctrlOut, sizeOut) dIn) =
      let dOut' = filterOutData =<< dOut
          ctrlOut' = filterOutData =<< ctrlOut
          sizeOut' = filterOutData =<< sizeOut
       in SMapFun (dOut', ctrlOut', sizeOut') dIn
    go (StateDFFun annots (stateOut, dataOut) fun sIn dIn) =
      StateDFFun
        annots
        (filterOutData =<< stateOut, filterOutData =<< dataOut)
        fun
        sIn
        dIn
    go e = e

    filterOutData :: forall (a::BindingType). OutData a ty -> Maybe (OutData a ty)
    filterOutData (Direct a) | not $ isBndUsed (unwrapTB a) = Nothing
    filterOutData a@Direct {} = Just a
    filterOutData (Destruct xs) =
      case mapMaybe filterOutData $ toList xs of
        [] -> Nothing
        (a : as) -> Just $ Destruct $ a :| as
    filterOutData (Dispatch bs) =
      case filter (isBndUsed . unwrapTB) $ toList bs of
        [] -> Nothing
        (a : as) -> Just $ Dispatch $ a :| as

    isBndUsed :: TypedBinding ty -> Bool
    isBndUsed bnd =
      let usedBnds = HS.fromList $ usedBindings expr
       in HS.member bnd usedBnds
