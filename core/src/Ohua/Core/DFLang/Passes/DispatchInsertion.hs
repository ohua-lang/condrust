module Ohua.Core.DFLang.Passes.DispatchInsertion where

import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NEE
import Ohua.Core.DFLang.Lang hiding (length)
import Ohua.Core.DFLang.Refs
import Ohua.Core.Prelude

insertDispatch :: forall ty m. MonadOhua m => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
insertDispatch (Let app cont) =
  case app of
    (PureDFFun outputs fr@(FunRef f _ _) inputs)
      | f == smapFun -> do
        (cont', outputs') <- foldMapOutData cont outputs
        cont'' <- insertDispatch cont'
        return $ Let (PureDFFun outputs' fr inputs) cont''
    _ -> Let app <$> insertDispatch cont
insertDispatch v = pure v

foldMapOutData ::
  forall ty b m.
  MonadOhua m =>
  NormalizedDFExpr ty ->
  OutData b ->
  m (NormalizedDFExpr ty, OutData b)
foldMapOutData expr (Direct bnd@(DataBinding _)) = do
  (cont', bnds') <- renameChannels (expr, []) (unwrapABnd bnd)
  case bnds' of
    [] -> throwError $ "Internal compiler error: Renaming a channel named " <> show bnd <> " yielded no channel name from " <> show expr
    [x] -> return (cont', (Direct x))
    _ -> return (cont', (Dispatch $ NE.fromList bnds'))
foldMapOutData expr ob@(Direct (StateBinding _)) = return (expr, ob)
foldMapOutData expr (Destruct bnds) = do
  (cont', bnds') <- foldrM g (expr, []) bnds
  return (cont', (Destruct $ NE.fromList bnds'))
  where
    g :: OutData b -> (NormalizedDFExpr ty, [OutData b]) -> m (NormalizedDFExpr ty, [OutData b])
    g bnd (exp, currentBinds) = do
      (expr', currentBinds') <- foldMapOutData exp bnd
      return (expr', (currentBinds' : currentBinds))
foldMapOutData expr (Dispatch bnds@((DataBinding _) :| _)) = do
  (cont', bnds') <- foldM f (expr, []) bnds
  return (cont', (Dispatch $ NE.fromList bnds'))
  where
    f :: (NormalizedDFExpr ty, [ABinding 'Data]) -> ABinding 'Data -> m (NormalizedDFExpr ty, [ABinding 'Data])
    f (exp, currentBinds) bnd = do
      (expr', currentBinds') <- renameChannels (exp, []) (unwrapABnd bnd)
      return (expr', (currentBinds ++ currentBinds'))
foldMapOutData expr ob@(Dispatch ((StateBinding _) :| _)) = return (expr, ob)

-- takes the smap body, the binding to the size channel, whether the binding has been seen before and the replaced bindings
renameChannels ::
  forall ty m.
  MonadOhua m =>
  (NormalizedDFExpr ty, [ABinding 'Data]) ->
  Binding ->
  m (NormalizedDFExpr ty, [ABinding 'Data])
renameChannels ((Let app@(PureDFFun out fn inp) cont), newBinds) bnd
  | elem bnd $ insDFApp app =
    -- rewrite
    do
      -- create a new binding
      newBnd <- DataBinding <$> generateBindingWith bnd
      let inp' = map (replaceInput bnd newBnd) inp
      let newBinds' = newBnd : newBinds
      (cont', newBinds'') <- renameChannels (cont, newBinds') bnd
      return (Let (PureDFFun out fn inp') cont', newBinds'')
  | otherwise = do
    -- no match, continue
    (cont', newBinds') <- renameChannels (cont, newBinds) bnd
    return (Let app cont', newBinds')
renameChannels ((Let app@(StateDFFun out fn boundState inp) cont), newBinds) bnd
  | (elem bnd $ extractBndsFromInputs [boundState]) &&
    (elem bnd $ extractBndsFromInputs $ NE.toList inp) =
      throwError "Invariant broken: Cannot use state variable as argument at the same time!"
  | elem bnd $ extractBndsFromInputs [boundState] =
    -- this is most delicate: The binding is used as state!
    do
      assertE (null newBinds) "Invariant broken: Cannot have a state used more than once (which would require a Dispatch)"
      assertE (ensureNoUse bnd cont) "Invariant broken: Cannot have a state used more than once (which would require a Dispatch)"
      return (Let app cont, [DataBinding bnd])
  | elem bnd $ extractBndsFromInputs $ NE.toList inp =
    do
      -- similar procedure as for a PureDFFun
      newBnd <- DataBinding <$> generateBindingWith bnd
      let inp' = map (replaceInput bnd newBnd) inp
      let newBinds' = newBnd : newBinds
      (cont', newBinds'') <- renameChannels (cont, newBinds') bnd
      return (Let (StateDFFun out fn boundState inp') cont', newBinds'')
  | otherwise = do
    -- no match, continue
    (cont', newBinds') <- renameChannels (cont, newBinds) bnd
    return (Let app cont', newBinds')
  where
    ensureNoUse :: Binding -> NormalizedDFExpr ty -> Bool
    ensureNoUse b (Var bnd) = bnd /= b
    ensureNoUse b (Let app cont) = case elem b $ insDFApp app of
      True -> False
      False -> ensureNoUse b cont
renameChannels ((Let app cont), newBinds) bnd = do
  (cont', newBinds') <- renameChannels (cont, newBinds) bnd
  return (Let app cont', newBinds')
renameChannels v@((Var _), _) _ = return v

replaceInput :: Binding -> ABinding 'Data -> DFVar 'Data a -> DFVar 'Data a
replaceInput old newBind var = case var of
  (DFVar t bnd)
    | unwrapABnd bnd == old -> (DFVar t newBind)
    | otherwise -> var
  (DFEnvVar _ _) -> var
