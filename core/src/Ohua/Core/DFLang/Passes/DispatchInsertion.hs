module Ohua.Core.DFLang.Passes.DispatchInsertion where

import Control.Monad.Extra (maybeM)
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty.Extra ((|>))


import qualified Ohua.Core.InternalFunctions as IFuns 
import Ohua.Core.DFLang.Lang hiding (length)
import Ohua.Core.Prelude hiding (Nat)
import Ohua.Types.Vector



data UsageSite = Pure | StateThread | SMap | If | Rec | Collect | Select | Ctrl deriving Eq
data Record where
  Record :: NonEmpty UsageSite -> SNat ('Succ n) -> Record
type FreeVars ty = HM.HashMap (TypedBinding ty) Record

fromApp :: DFApp fTy embExpr ty -> UsageSite
fromApp PureDFFun{} = Pure
fromApp StateDFFun{} = StateThread
fromApp SMapFun{} = SMap
fromApp IfFun{} = If
fromApp RecurFun{} = Rec
-- We'll need them later
fromApp CollectFun{} = Collect
fromApp SelectFun{} = Select
fromApp CtrlFun{} = Ctrl

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
insertDispatch :: forall m embExpr ty. MonadGenBnd m => MonadOhua m => NormalizedDFExpr embExpr ty -> m (NormalizedDFExpr embExpr ty)
insertDispatch e = evalStateT (transformExprM go e) HM.empty
  where
    go :: NormalizedDFExpr embExpr ty -> StateT (FreeVars ty) m (NormalizedDFExpr embExpr ty)
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
        (PureDFFun _ (FunRef f _) _)
          | f == IFuns.ctrl
            || f == IFuns.select
            || f == IFuns.collect
            -> collect app >> return l
        (PureDFFun o f i) -> do
          (cont', o') <- dispatchOutput cont o
          return $ Let (PureDFFun o' f i) cont'
        (StateDFFun (sOut, dOut) f sIn dIn) -> do
          (cont', dOut') <- maybeDispatch cont dOut
          (cont'', sOut') <- maybeDispatch cont' sOut
          return $ Let (StateDFFun (sOut', dOut') f sIn dIn) cont''
        (RecurFun fOut ctrlOut recArgsOut initIns recIns recCondIn fIn) -> do
          (cont', ctrlOut') <- maybeDispatch cont ctrlOut
          return $ Let (RecurFun fOut ctrlOut' recArgsOut initIns recIns recCondIn fIn) cont'
        _ -> return l
      modify $ \outB -> foldl (flip HM.delete) outB $ outBindings app
      return l'
    go v = pure v

    collect app =
      let initial = Record (fromApp app :| []) $ SSucc SZero
      in   mapM_ (\tbnd -> modify $ HM.insertWith (<>) tbnd initial) $ insDFApp app

    maybeDispatch :: NormalizedDFExpr embExpr ty -> Maybe (OutData bndType ty) -> StateT (FreeVars ty) m (NormalizedDFExpr embExpr ty, Maybe (OutData bndType ty))
    maybeDispatch cont dOut =
      let f :: OutData bndType ty -> StateT (FreeVars ty) m (NormalizedDFExpr embExpr ty, Maybe (OutData bndType ty))
          f out = do
            (c', out') <- dispatchOutput cont out
            return (c', Just out')
       in maybeM (return (cont, dOut)) f $ return dOut

    dispatchOutput :: NormalizedDFExpr embExpr ty -> OutData bndType ty -> StateT (FreeVars ty) m (NormalizedDFExpr embExpr ty, OutData bndType ty)
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

    checkAndDispatch :: NormalizedDFExpr embExpr ty -> ATypedBinding bndTy ty -> StateT (FreeVars ty) m (NormalizedDFExpr embExpr ty, NonEmpty (ATypedBinding bndTy ty))
    checkAndDispatch cont o = do
      condIns <- get
      case HM.lookup (unwrapTB o) condIns of
        (Just (Record _ n@(SSucc (SSucc _)))) -> do
          (cont', bnds) <- lift $ insertDispatch' cont o n
          return (cont', bnds)
        _ -> return (cont, o :| [])

    insertDispatch' :: NormalizedDFExpr embExpr ty -> ATypedBinding bndTy ty -> SNat ('Succ n) -> m (NormalizedDFExpr embExpr ty, NonEmpty (ATypedBinding bndTy ty))
    insertDispatch' cont b (SSucc n) = do
      let oldB@(TBind bnd ty) = unwrapTB b
      bnd' <- generateBindingWith bnd
      let newB = TBind bnd' ty
          cont' = substituteFirstOccurence (oldB,newB) cont
      case n of
        SZero -> return (cont', renameABnd newB b :| [])
        k@SSucc{} -> (\(c, bnds) -> (c, renameABnd newB b <| bnds)) <$> insertDispatch' cont' b k

    substituteFirstOccurence = substitute FirstOccurrence
