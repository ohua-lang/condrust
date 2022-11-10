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


data UsageSite = Pure| StateThread | SMap | If | Rec | Collect | Select | Ctrl deriving Eq
data Record where
  Record :: NonEmpty UsageSite -> SNat ('Succ n) -> Record
type FreeVars = HM.HashMap Binding Record

fromApp :: DFApp a b -> UsageSite
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
insertDispatch :: forall m ty. MonadGenBnd m => MonadOhua m => NormalizedDFExpr ty -> m (NormalizedDFExpr ty)
insertDispatch e = evalStateT (transformExprM go e) HM.empty
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
        (PureDFFun _ (FunRef f _ _) _)
          | f == DFLangRefs.ctrl
            || f == DFLangRefs.select
            || f == DFLangRefs.collect
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
      in   mapM_ (\bnd -> modify $ HM.insertWith (<>) bnd initial) $ insDFApp app

    maybeDispatch :: NormalizedDFExpr ty -> Maybe (OutData bndType) -> StateT FreeVars m (NormalizedDFExpr ty, Maybe (OutData bndType))
    maybeDispatch cont dOut =
      let f :: OutData bndType -> StateT FreeVars m (NormalizedDFExpr ty, Maybe (OutData bndType))
          f out = do
            (c', out') <- dispatchOutput cont out
            return (c', Just out')
       in maybeM (return (cont, dOut)) f $ return dOut

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
        (Just (Record _ n@(SSucc (SSucc _)))) -> do
          (cont', bnds) <- lift $ insertDispatch' cont o n
          return (cont', bnds)
        _ -> return (cont, o :| [])

    insertDispatch' :: NormalizedDFExpr ty -> ABinding bndType -> SNat ('Succ n) -> m (NormalizedDFExpr ty, NonEmpty (ABinding bndType))
    insertDispatch' cont b (SSucc n) = do
      let bnd = unwrapABnd b
      bnd' <- generateBindingWith bnd
      let cont' = substituteFirstOccurence (bnd, bnd') cont
      case n of
        SZero -> return (cont', renameABnd bnd' b :| [])
        k@SSucc{} -> (\(c, bnds) -> (c, renameABnd bnd' b <| bnds)) <$> insertDispatch' cont' b k

    substituteFirstOccurence = substitute FirstOccurrence
