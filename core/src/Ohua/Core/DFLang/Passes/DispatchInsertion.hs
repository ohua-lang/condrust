module Ohua.Core.DFLang.Passes.DispatchInsertion where

import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty.Extra as NEE
import Ohua.Core.DFLang.Lang hiding (length)
import Ohua.Core.DFLang.Refs
import Ohua.Core.Prelude

insertDispatch :: forall ty m. MonadOhua m => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
insertDispatch (Let app cont) =
  case app of
    (PureDFFun outputs fn inputs)
      | fn == smapFun -> do
        (cont', outputs') <- foldMapOutData cont outputs
        cont'' <- insertDispatch cont'
        return $ Let (PureDFFun outputs' fn inputs) cont''
        -- let sizeChan = last $ outBnds outputs
        -- -- run the rename
        -- (cont', sizeChannels) <- renameChannels (cont, []) sizeChan
        -- -- if length of bindings is 1, don't do anything, else, change the outputs and return t
        -- if length sizeChannels == 1
        --   then Let app <$> insertDispatch cont
        --   else do
        --     outputs' <- replaceSizeWithDispatch outputs sizeChannels
        --     cont'' <- insertDispatch cont'
        --     return $ Let (PureDFFun outputs' fn inputs) cont''
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
  return (cont', (Dispatch $ NE.fromList bnds'))
foldMapOutData expr ob@(Direct (StateBinding _)) = return (expr, ob)
foldMapOutData expr (Destruct bnds) = do
  (cont', bnds') <- foldrM g (expr, []) bnds
  return (cont', (Destruct $ NE.fromList bnds'))
  where
    g :: OutData b -> (NormalizedDFExpr ty, [OutData b]) -> m (NormalizedDFExpr ty, [OutData b])
    g bnd (exp, currentBinds) = do
      (expr', currentBinds') <- foldMapOutData exp bnd
      return (expr', (currentBinds' : currentBinds))
foldMapOutData expr (Dispatch bnds@((DataBinding _):|_)) = do
  (cont', bnds') <- foldM f (expr, []) bnds
  return (cont', (Dispatch $ NE.fromList bnds'))
  where
    f :: (NormalizedDFExpr ty, [ABinding 'Data]) -> ABinding 'Data -> m (NormalizedDFExpr ty, [ABinding 'Data])
    f (exp, currentBinds) bnd = do
      (expr', currentBinds') <- renameChannels (exp, []) (unwrapABnd bnd)
      return (expr', (currentBinds ++ currentBinds'))
foldMapOutData expr ob@(Dispatch ((StateBinding _):|_)) = return (expr, ob)


-- takes the smap body, the binding to the size channel, whether the binding has been seen before and the replaced bindings
renameChannels ::
  forall ty m.
  MonadOhua m =>
  (NormalizedDFExpr ty, [ABinding 'Data]) ->
  Binding ->
  m (NormalizedDFExpr ty, [ABinding 'Data])
renameChannels (e@(Let app cont), newBinds) bnd =
  case app of
    (PureDFFun out fn inp)
      -- end condition
      | fn == collect -> pure (e, newBinds)
      -- TODO(feliix42): Do we need to catch multiple occurences of the same size channel here? Currently that can't happen, but who knows what will happen.
      | otherwise ->
        if elem bnd $ extractBndsFromInputs $ NE.toList inp
          then -- rewrite

            if length newBinds == 0
              then do
                (cont', newBinds') <- renameChannels (cont, [DataBinding bnd]) bnd
                return (Let app cont', newBinds')
              else do
                -- create a new binding
                newBnd <- DataBinding <$> generateBindingWith bnd
                let inp' = map (replaceInput bnd newBnd) inp
                let newBinds' = newBnd : newBinds
                (cont', newBinds'') <- renameChannels (cont, newBinds') bnd
                return (Let (PureDFFun out fn inp') cont', newBinds'')
          else do
            -- no match, continue
            (cont', newBinds') <- renameChannels (cont, newBinds) bnd
            return (Let app cont', newBinds')
    -- TODO(feliix42): Actually handle other function types if necessary
    _ -> do
      (cont', newBinds') <- renameChannels (cont, newBinds) bnd
      return (Let app cont', newBinds')
renameChannels ((Var _), _) _ = throwError $ "Invariant broken: Found an smap not delimited by a collect"

replaceInput :: Binding -> ABinding 'Data -> DFVar 'Data a -> DFVar 'Data a
replaceInput old newBind var = case var of
  (DFVar t bnd)
    | unwrapABnd bnd == old -> (DFVar t newBind)
    | otherwise -> var
  (DFEnvVar _ _) -> var

-- replaceSizeWithDispatch :: OutData b -> [ABinding 'Data] -> m (OutData b)
-- replaceSizeWithDispatch (Destruct binds) newBinds =
--   let lst' = NE.init binds
--    in pure $ Destruct $ NEE.snoc (NE.fromList lst') $ Dispatch $ NE.fromList newBinds
-- -- TODO(feliix42): @Sebastian how do you throw the error correctly here?
-- replaceSizeWithDispatch _ _ = throwError "Found unexpected OutData format in smap output."
