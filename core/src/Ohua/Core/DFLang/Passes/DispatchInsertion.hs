module Ohua.Core.DFLang.Passes.DispatchInsertion where

import Control.Monad.Extra (maybeM)
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty.Extra ((|>))
import qualified Ohua.Core.DFLang.Refs as DFLangRefs
import Ohua.Core.DFLang.Lang hiding (length)
import Ohua.Core.Prelude hiding (Nat)
import Ohua.Types.Vector 


data UsageSite = Pure| StateThread | SMap | If | Rec deriving Eq
data Record where
  Record :: NonEmpty UsageSite -> SNat ('Succ n) -> Record
type FreeVars = HM.HashMap Binding Record

fromApp :: DFApp a b -> UsageSite
fromApp PureDFFun{} = Pure
fromApp StateDFFun{} = StateThread
fromApp SMapFun{} = SMap
fromApp IfFun{} = If
fromApp RecurFun{} = Rec

addUsageSite :: UsageSite -> Record -> Record
addUsageSite a (Record u n) = Record (u |> a) (SSucc n)

updateLastUsageSite :: UsageSite -> Record -> Record
updateLastUsageSite u (Record us n) = Record us' n
  where
    us' = (NE.reverse . (u :|) . NE.tail . NE.reverse) us

isUsedAt :: UsageSite -> Record -> Bool
isUsedAt a (Record as _) = a `elem` as

instance Semigroup Record where
  (Record u1 n1) <> (Record u2 n2) = Record (u1 <> u2) (add n1 n2)


-- | This inserts dispatches only for control data.
--   Core delegates the decision of cloning or not cloning to the language integration.
-- TODO add proper documentation
insertDispatch :: forall m ty. MonadGenBnd m => MonadOhua m => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
insertDispatch e = do
  e' <- evalStateT (transformExprM go e) HM.empty
  traceM ("Dispatch done: " <> show e')
  return e'
  where
    go :: NormalizedDFExpr ty -> StateT FreeVars m (NormalizedDFExpr ty)
    go l@(Let app cont) = do
      l' <- case app of
        (SMapFun (dOut, ctrlOut, sizeOut) dIn) -> do
          (cont', dOut') <- maybeDispatch cont dOut
          (cont'', ctrlOut') <- maybeDispatch cont' ctrlOut
          (cont''', sizeOut') <- maybeDispatch cont'' sizeOut
          return $ Let (SMapFun (dOut', ctrlOut', sizeOut') dIn) cont'''
        (IfFun (trueOut, falseOut) dIn) -> do
          collect app
          (cont', trueOut') <- dispatchOutput cont trueOut
          (cont'', falseOut') <- dispatchOutput cont' falseOut
          return $ Let (IfFun (trueOut', falseOut') dIn) cont''
        (PureDFFun _ (FunRef f _ _) _) | f == DFLangRefs.ctrl -> collect app >> return l
        (PureDFFun o f i) -> do
          (cont', o') <- dispatchOutput cont o
          return $ Let (PureDFFun o' f i) cont'
        (StateDFFun (sOut, dOut) f sIn dIn) -> do
          (cont', dOut') <- maybeDispatch cont dOut
          (cont'', sOut') <- maybeDispatch cont' sOut
          return $ Let (StateDFFun (sOut', dOut') f sIn dIn) cont''
       -- TODO recurFun control output also needs to be dispatched!
        _ -> return l
      modify $ \condIns -> foldl (flip HM.delete) condIns $ outBindings app
      return l'
    go v = pure v

    collect app =
      let initial = Record (fromApp app :| []) $ SSucc SZero
      in   mapM_ (\bnd -> modify $ HM.insertWith (<>) bnd initial) $ insDFApp app

    maybeDispatch :: NormalizedDFExpr ty -> Maybe (OutData bndType) -> StateT FreeVars m (NormalizedDFExpr ty, Maybe (OutData bndType))
    maybeDispatch cont dOut =
      let f :: OutData bndType -> StateT FreeVars m (NormalizedDFExpr ty, Maybe (OutData bndType))
          f out = do
            (c', out') <- dispatchOutput cont out
            return (c', Just out')
       in maybeM (return (cont, dOut)) f $ return dOut
    --    onOut c o = case o of
    --      Nothing -> return (c, o)
    --      Just out -> do
    --        (c', out') <- foldMapOutData c out
    --        return (c', Just out')

    dispatchOutput :: NormalizedDFExpr ty -> OutData bndType -> StateT FreeVars m (NormalizedDFExpr ty, OutData bndType)
    dispatchOutput cont (Direct o) = do
      (cont', o') <- checkAndDispatch cont o
      return
        ( cont'
        , case o' of
            (o'' :| []) -> Direct o''
            _ -> Dispatch o'
        )
    dispatchOutput cont (Destruct (out :| outs)) = do
      (cont', out') <- dispatchOutput cont out
      (cont'', out'') <-
        foldM
          ( \(c, bs) b -> do
              (c', b') <- dispatchOutput c b
              return (c', bs |> b')
          )
          (cont', out' :| [])
          outs
      return (cont'', Destruct out'')
    dispatchOutput cont (Dispatch (out :| outs)) = do
      (cont', out') <- checkAndDispatch cont out
      (cont'', out'') <-
        foldM
          ( \(c, bs) b -> do
              (c', b') <- checkAndDispatch c b
              return (c', bs <> b')
          )
          (cont', out')
          outs
      return (cont'', Dispatch out'')

    checkAndDispatch :: NormalizedDFExpr ty -> ABinding bndType -> StateT FreeVars m (NormalizedDFExpr ty, NonEmpty (ABinding bndType))
    checkAndDispatch cont o = do
      condIns <- get
      case HM.lookup (unwrapABnd o) condIns of
        (Just (Record _ n)) -> do
          (cont', bnds) <- lift $ insertDispatch' cont o n
          return (cont', bnds)
        Nothing -> return (cont, o :| [])

    insertDispatch' :: NormalizedDFExpr ty -> ABinding bndType -> SNat ('Succ n) -> m (NormalizedDFExpr ty, NonEmpty (ABinding bndType))
    insertDispatch' cont b (SSucc n) = do
      let bnd = unwrapABnd b
      bnd' <- generateBindingWith bnd
      let cont' = substituteFirstOccurence (bnd, bnd') cont
      case n of
        SZero -> return (cont', renameABnd bnd' b :| [])
        k@SSucc{} -> (\(c, bnds) -> (c, renameABnd bnd' b <| bnds)) <$> insertDispatch' cont' b k

    substituteFirstOccurence = substitute FirstOccurrence

-- TODO rewrit using the fact that this is already a bottom-up traversal.
--foldMapOutData ::
--  forall ty b m.
--  MonadOhua m =>
--  NormalizedDFExpr ty ->
--  OutData b ->
--  m (NormalizedDFExpr ty, OutData b)
--foldMapOutData expr (Direct bnd@(DataBinding _)) = do
--  (cont', bnds') <- renameChannels (expr, []) (unwrapABnd bnd)
--  case bnds' of
--    [] -> return (cont', Direct bnd) -- The output is never used in the continuation. This will be caught by the dead code elimination
--    [x] -> return (cont', Direct x)
--    _ -> return (cont', Dispatch $ NE.fromList bnds')
--foldMapOutData expr ob@(Direct (StateBinding _)) = return (expr, ob)
----foldMapOutData expr (Destruct bnds) = do
----  (cont', bnds') <- foldrM g (expr, []) bnds
----  return (cont', Destruct $ NE.fromList bnds')
--foldMapOutData expr (Destruct (bnd :| bnds)) = do
--  (cont', bnd') <- foldMapOutData expr bnd
--  (cont', bnds') <- foldrM g (expr, []) bnds
--  return (cont', Destruct $ NE.fromList bnds')
--  where
--    g :: OutData b -> (NormalizedDFExpr ty, [OutData b]) -> m (NormalizedDFExpr ty, [OutData b])
--    g bnd (exp, currentBinds) = do
--      (expr', currentBinds') <- foldMapOutData exp bnd
--      return (expr', currentBinds' : currentBinds)
--foldMapOutData expr (Dispatch bnds@((DataBinding _) :| _)) = do
--  (cont', bnds') <- foldM f (expr, []) bnds
--  return (cont', Dispatch $ NE.fromList bnds')
--  where
--    f :: (NormalizedDFExpr ty, [ABinding 'Data]) -> ABinding 'Data -> m (NormalizedDFExpr ty, [ABinding 'Data])
--    f (exp, currentBinds) bnd = do
--      (expr', currentBinds') <- renameChannels (exp, []) (unwrapABnd bnd)
--      return (expr', currentBinds ++ currentBinds')
--foldMapOutData expr ob@(Dispatch ((StateBinding _) :| _)) = return (expr, ob)
--
---- takes the smap body, the binding to the size channel, whether the binding has been seen before and the replaced bindings
--renameChannels ::
--  forall ty m.
--  MonadOhua m =>
--  (NormalizedDFExpr ty, [ABinding 'Data]) ->
--  Binding ->
--  m (NormalizedDFExpr ty, [ABinding 'Data])
--renameChannels (Let app@(PureDFFun out fn inp) cont, newBinds) bnd
--  | elem bnd $ insDFApp app =
--    -- rewrite
--    do
--      -- create a new binding
--      newBnd <- DataBinding <$> generateBindingWith bnd
--      let inp' = map (replaceInput bnd newBnd) inp
--      let newBinds' = newBnd : newBinds
--      (cont', newBinds'') <- renameChannels (cont, newBinds') bnd
--      return (Let (PureDFFun out fn inp') cont', newBinds'')
--  | otherwise = do
--    -- no match, continue
--    (cont', newBinds') <- renameChannels (cont, newBinds) bnd
--    return (Let app cont', newBinds')
--renameChannels ((Let app@(StateDFFun out fn boundState inp) cont), newBinds) bnd
--  | (elem bnd $ extractBndsFromInputs [boundState])
--      && (elem bnd $ extractBndsFromInputs $ NE.toList inp) =
--    throwError "Invariant broken: Cannot use state variable as argument at the same time!"
--  | elem bnd $ extractBndsFromInputs [boundState] =
--    -- this is most delicate: The binding is used as state!
--    do
--      assertE (null newBinds) "Invariant broken: Cannot have a state used more than once (which would require a Dispatch)"
--      assertE (ensureNoUse bnd cont) "Invariant broken: Cannot have a state used more than once (which would require a Dispatch)"
--      return (Let app cont, [DataBinding bnd])
--  | elem bnd $ extractBndsFromInputs $ NE.toList inp =
--    do
--      -- similar procedure as for a PureDFFun
--      newBnd <- DataBinding <$> generateBindingWith bnd
--      let inp' = map (replaceInput bnd newBnd) inp
--      let newBinds' = newBnd : newBinds
--      (cont', newBinds'') <- renameChannels (cont, newBinds') bnd
--      return (Let (StateDFFun out fn boundState inp') cont', newBinds'')
--  | otherwise = do
--    -- no match, continue
--    (cont', newBinds') <- renameChannels (cont, newBinds) bnd
--    return (Let app cont', newBinds')
--  where
--    ensureNoUse :: Binding -> NormalizedDFExpr ty -> Bool
--    ensureNoUse b (Var bnd) = bnd /= b
--    ensureNoUse b (Let app cont) = case elem b $ insDFApp app of
--      True -> False
--      False -> ensureNoUse b cont
--renameChannels ((Let app cont), newBinds) bnd = do
--  (cont', newBinds') <- renameChannels (cont, newBinds) bnd
--  return (Let app cont', newBinds')
--renameChannels v@((Var _), _) _ = return v
--
--replaceInput :: Binding -> ABinding 'Data -> DFVar 'Data a -> DFVar 'Data a
--replaceInput old newBind var = case var of
--  (DFVar t bnd)
--    | unwrapABnd bnd == old -> (DFVar t newBind)
--    | otherwise -> var
--  (DFEnvVar _ _) -> var
