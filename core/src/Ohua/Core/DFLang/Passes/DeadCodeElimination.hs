{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Core.DFLang.Passes.DeadCodeElimination where

import Ohua.Core.Prelude
import Ohua.Core.DFLang.Lang as L
import Ohua.Core.DFLang.Refs as R
import qualified Ohua.Types.Vector as V

import qualified Data.List.NonEmpty as NE
import Data.Text.Lazy.IO as T
import Control.Monad.Extra

eliminate :: (MonadOhua m) => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
eliminate expr = do
  expr' <- (eliminateExprs >=> eliminateOuts) expr
  case L.countBindings expr == L.countBindings expr' of
    True -> pure expr'
    False -> do
      traceM "Doing another run on the eliminate transformation"
      eliminate expr'

eliminateExprs :: forall m ty.(MonadOhua m) => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
eliminateExprs expr = do
  expr' <- f expr
  case L.length expr == L.length expr' of
    True -> pure expr'
    False -> eliminateExprs expr'
  where
    f :: NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
    f (Let app cont) = case app of
      (PureDFFun out (FunRef fun _ _) _) ->
        case out `isUsedIn` expr of
          True -> Let app <$> f cont
          False -> warn fun >> (f cont)
      (StateDFFun (stateOut, dataOut) fun sIn dIn) ->
        Let
          (StateDFFun
           (dropOut =<< stateOut, dropOut =<< dataOut)
           fun sIn dIn) <$>
          f cont
      -- TODO(feliix42): Is recur elimination realistically possible?
      RecurFun {} -> Let app <$> f cont
    f v@(Var _) = do
      traceM $ toText $ "Terminating elimination on " ++ (show v)
      pure v

    dropOut :: OutData b -> Maybe (OutData b)
    dropOut out =
      case (out `isUsedIn` expr) of
        True -> Just out
        False -> Nothing

    out `isUsedIn` e = or $ map (`isBndUsed` e) $ outBnds out

    warn :: QualifiedBinding -> m ()
    warn fun@(QualifiedBinding (NSRef ns) _) =
      case ns == ["ohua","lang"] of
        True -> warning fun -- return ()
        False -> warning fun

    warning :: QualifiedBinding -> m ()
    warning fun = liftIO $ T.putStrLn $ "[WARNING] The output of pure function '" <> show fun <> "' is not used. As such, it will be deleted. If the function contains side-effects then this function actually wants to be stateful!"

eliminateOuts :: forall m ty.(MonadOhua m) => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
eliminateOuts expr = do
  traceM $ show expr
  transformExprM go expr
  where
    go (Let (RecurFun c ctrl outArgs initArgs inArgs cond result) cont) =
      case V.zip3 outArgs initArgs inArgs of
        zipped -> case V.filter filterDead zipped of
                    V.MkEV filtered -> case V.unzip3 filtered of
                      (outArgs', initArgs', inArgs') ->
                        pure $ Let (RecurFun c ctrl outArgs' initArgs' inArgs' cond result) cont
    go (Let app cont) = case app of
        (PureDFFun out fr@(FunRef fun _ _) inp)
          | R.smapFun == fun -> case out of
              -- this should match the initial form of an smap call. Hence, this houses the hack for issue #8
              -- FIXME(sertel): find a better way to recognize illegal channel combinations
              (Destruct xs@[_, _, _]) -> do
                traceM $ show xs
                traceM $ show $ NE.toList xs
                filtered <- mapM filterSmapOuts $ NE.toList xs
                traceM $ show filtered
                case filtered of
                  [[], [], []] ->
                    throwError $ "dead smap detected: " <> show app
                  [[], [], size] ->
                    throwError $ "detected smap that only dispatches its size. but for what?! either something needs to be contextified or data needs to be processed then. this is a bug. please report it."
                  [[], ctrl, []] ->
                    throwError "Unsupported. Please report. This requires a more explicit DFLang: Only ctrl used."
                  [[], ctrl, size] -> do
                    ctrl' <- createOutBnd ctrl
                    size' <- createOutBnd size
                    pure $ Let (PureDFFun (Destruct $ ctrl' :|[size']) fr inp) cont
                    -- throwError "Unsupported. Please report. This requires a more explicit DFLang: Only data output dropped."
                  [[ds], [], []] ->
                    pure $ Let (PureDFFun (Destruct $ (Direct ds) :|[]) fr inp) cont
                  [[ds], [], size] ->
                    throwError $ "TODO: Undefined behavior"
                  [[ds], ctrl, []] ->
                    throwError $ "Unsupported. Please report. This requires a more explicit DFLang: Size dropped. " <> show filtered
                    -- pure $ Let (PureDFFun (Destruct $ Direct ds :|[Direct ctrl]) fun inp) cont
                  [[ds], ctrl, size] -> do
                    ctrl' <- createOutBnd ctrl
                    size' <- createOutBnd size
                    pure $ Let (PureDFFun (Destruct $ (Direct ds) :|[ctrl', size']) fr inp) cont
                  x -> throwError $ "Encountered malformed smap output, most likely it has too many data outs: " <> show x
              -- TODO(feliix42): Handle already reduced smap expressions
              (Destruct xs) -> do
                -- traverse the out bindings, filter empty ones out and create new out bindings
                filtered <- mapM filterSmapOuts $ NE.toList xs
                binds' <- mapMaybeM createOutBndMaybe filtered --  $ mapM filterSmapOuts $ NE.toList xs
                -- binds' <- map (createOutBnd <=< (filter notNull) <=< filterSmapOuts) $ NE.toList xs
                pure $ Let (PureDFFun (Destruct $ NE.fromList binds') fr inp) cont
                --pure $ Let app cont
            -- case used of
            --   -- TODO(feliix42): wouldn't that need to be able to handle multiple ctrl outs?
            --   -- FIXME see issue #8. obviously, this is a big hack right now!
            --   -- ds  ctrl  size
            --   [(False,_),(False,_),(False,_)] ->
            --     throwError $ "dead smap detected: " <> show app
            --   [(True,ds), (False,_), (False,_)] ->
            --     pure $ Let (PureDFFun (Destruct $ Direct ds :|[]) fr inp) cont
            --   [(True,ds), (True,ctrl), (False,_)] ->
            --     throwError "Unsupported. Please report. This requires a more explicit DFLang: Size dropped."
            --     -- pure $ Let (PureDFFun (Destruct $ Direct ds :|[Direct ctrl]) fun inp) cont
            --   [(False,_), (True,_), (False,_)] ->
            --     throwError "Unsupported. Please report. This requires a more explicit DFLang: Only ctrl used."
            --   [(False,_), (True,ctrl), (True,size)] ->
            --     pure $ Let (PureDFFun (Destruct $ Direct ctrl :|[Direct size]) fr inp) cont
            --     -- throwError "Unsupported. Please report. This requires a more explicit DFLang: Only data output dropped."
            --   [(False,_), (False,_), (True,_)] -> throwError $ "detected smap that only dispatches its size. but for what?! either something needs to be contextified or data needs to be processed then. this is a bug. please report it."
            --   _ -> pure $ Let app cont
        _ -> let
          -- note that I do not only check inside the continuation but the whole expression
          -- because recurFun takes vars as arguments that are defined only later.
          used = map (\b -> (isBndUsed b expr, DataBinding b)) $ outBindings app
          in case filter (not . fst) used of
               []       -> pure $ Let app cont
               deadEnds -> throwError $ "Found dead ends for '" <> show app
                           <> "'.\nDead ends: " <> show deadEnds
    go e = pure e

    filterDead :: (OutData b, DFVar a ty, DFVar a ty) -> Bool
    filterDead = \case
                    ((Direct b), _, _) -> isBndUsed (unwrapABnd b) expr
                    (b, _, _) -> error $ toText $ "Unsupported OutData variant encountered in recurFun: " ++ (show b)

    filterSmapOuts :: OutData b -> m [ABinding b]
    filterSmapOuts (Direct a) = do
      traceM $ show $ unwrapABnd a
      case isBndUsed (unwrapABnd a) expr of
        True -> pure [a]
        False -> pure []
    filterSmapOuts (Destruct _) = throwError "Invariant broken: Cannot have layered destructuring pattern in smap"
    filterSmapOuts (Dispatch xs) = pure $ filter (\a -> isBndUsed (unwrapABnd a) expr) $ NE.toList xs

    createOutBnd :: [ABinding b] -> m (OutData b)
    createOutBnd [] = throwError $ "Invariant broken: Got tasked to construct OutData from empty list of bindings."
    createOutBnd [bnd] = pure $ Direct bnd
    createOutBnd lst = pure $ Dispatch $ NE.fromList lst

    createOutBndMaybe :: [ABinding b] -> m (Maybe (OutData b))
    createOutBndMaybe [] = pure Nothing
    createOutBndMaybe [bnd] = pure $ Just $ Direct bnd
    createOutBndMaybe lst = pure $ Just $ Dispatch $ NE.fromList lst


isBndUsed :: Binding -> NormalizedDFExpr ty -> Bool
isBndUsed bnd (Let app cont) = bnd `elem` (insDFApp app) || bnd `isBndUsed` cont
isBndUsed bnd (Var result) = bnd == result
