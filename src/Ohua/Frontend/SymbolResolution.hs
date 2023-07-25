module Ohua.Frontend.SymbolResolution (resolveSymbols) where

import Ohua.UResPrelude
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.TypeSystem (Delta(..), Gamma(..))


{-
resolveSymbols :: ErrAndLogM m => Delta ty -> Namespace (Expr ty) anno ty -> m ()
resolveSymbols delta Namespace{algos} =
  forM_ algos
    $ \Algo{algoCode} -> resolveSymbolsInAlgo delta HM.empty algoCode

resolveSymbolsInAlgo :: ErrAndLogM m => Delta ty -> Gamma (VarType ty) ty -> Expr ty -> m ()
resolveSymbolsInAlgo delta gamma = \case
  LetE pat e1 e2 -> do
    resolveSymbolsInAlgo delta gamma e1
    let bnds = patBnd pat
    let gamma' = foldl (\g b -> HM.insert b TypeVar g) gamma bnds
    resolveSymbolsInAlgo delta gamma' e2
  AppE fun args ->
    forM_ ([fun] ++ args) $ resolveSymbolsInAlgo delta gamma
  LamE pat e ->
    let bnds = patBnd pat
        gamma' = foldl (\g b -> HM.insert b TypeVar g) gamma bnds
    in resolveSymbolsInAlgo delta gamma' ee
  BindE e1 e2 ->
    forM_ [e1 e2] $ resolveSymbolsInAlgo delta gamma
  IfE e1 e2 e3 ->
    forM_ [e1 e2 e3] $ resolveSymbolsInAlgo delta gamma
  WhileE e1 e2 ->
    forM_ [e1 e2] $ resolveSymbolsInAlgo delta gamma
  MapE e1 e2 ->
    forM_ [e1 e2] $ resolveSymbolsInAlgo delta gamma
  StmtE e1 e2 ->
    forM_ [e1 e2] $ resolveSymbolsInAlgo delta gamma
  TupE es ->
    forM_ es $ resolveSymbolsInAlgo delta gamma
  VarE bnd _ ->
    case HM.lookup bnd gamma of
      Just _ -> return ()
      Nothing -> -- TODO resolve here!
  LitE ->

data In :: forall n t. t -> Vector n t -> Type where
  Enter :: In t (t::ctxt)
  Preserve :: forall x. In t ctxt -> In t (x::ctxt)

data ResolvedExpr ty ::  Vector n t -> Type where
  VarE :: SBinding t -> In t ctxt -> ResolvedExpr ctxt ty
  LitE :: Res.Lit ty
-}

data SymResError
  = BndError Binding
  | QBndError QualifiedBinding
  | NoTypeFound QualifiedBinding
  | Ambiguity QualifiedBinding QualifiedBinding

resolve :: Delta ty -> [Imports] -> Maybe NSRef -> Binding -> Either (QualifiedBinding, FunType ty) SymResError
resolve delta imports (Just ns) bnd =
  let qb = resolveQBnd imports $ QualifiedBinding ns bnd
  in case HM.lookup qb delta of
       Just t -> Left (qb t)
       Nothing -> Right $ QBndError qb
resolve delta imports Nothing bnd =
  case resolveBnd imports bnd of
    Left qbs -> check qbs
    r -> r

check :: Delta ty -> NonEmpty QualifiedBinding -> Either (QualifiedBinding, FunType ty) SymResError
check delta (qb:|[]) =
  case HM.lookup qb delta of
    Just t -> Left (qb t)
    Nothing -> Right $ NoTypeFound qb
check delta (qb:|(qbs:qbss)) =
  case HM.lookup qb delta of
    Just t ->
      case check delta (qbs:|qbss) of
        Left (qb' _) -> Right $ Ambiguity qb qb'
        Right _ -> Left $ (qb,t)
    Nothing -> check delta (qbs:|qbss)

resolveQBnd :: [Imports] -> QualifiedBinding -> QualifiedBinding
resolveQBnd [] qb = qb
resolveQBnd ((Alias ns alias):is) (QualifiedBinding ns' b) | NSRef [alias] == ns' = QualifiedBinding ns b
resolveQBnd (_:is) qb = resolveQB is qb

resolveBnd :: [Imports] -> Binding -> Either (NonEmpty QualifiedBinding) SymResError
resolveBnd [] bnd = Right $ BndError bnd
resolveBnd ((Full ns bnd'):is) bnd | bnd' == bnd =
                                     case resolveBnd is bnd of
                                       Left other -> Left (qb <| other)
                                       Right _ -> Left (QualifiedBinding ns bnd :| [])
resolveBnd ((Glob ns):is) bnd =
  case resolveBnd is bnd of
    Left other -> Left (QualifiedBinding ns bnd <| other)
    Right _ -> Left (QualifiedBinding ns bnd :| [])
resolveBnd (_:is) bnd = resolveBnd is bnd
