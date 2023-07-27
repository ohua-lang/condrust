module Ohua.Frontend.SymbolResolution (resolveSymbols) where

import Ohua.UResPrelude
import Ohua.Frontend.Lang as FrLang
import qualified Ohua.Prelude as Res (FunType(..))
import qualified Data.HashMap.Lazy as HM

data SymResError
  = BndError Binding
  | QBndError QualifiedBinding
  | NoTypeFound QualifiedBinding
  | Ambiguity QualifiedBinding QualifiedBinding


{-
Environments:
-------------
Gamma ... associates local variables to a type
Delta ... associates function literals to a type
-}

type Gamma varTy ty = HM.HashMap Binding (varTy ty)
-- Question: Curent representation of FunType allows Unresolved. DO we want to forbid this here?
type Delta ty res = HM.HashMap QualifiedBinding (Res.FunType ty res)


resolve :: Delta ty res -> [Import] -> Maybe NSRef -> Binding -> Either (QualifiedBinding, FunType ty res ) SymResError
resolve delta imports (Just ns) bnd =
  let qb = resolveQBnd imports $ QualifiedBinding ns bnd
  in case HM.lookup qb delta of
       Just t -> Left (qb, t)
       Nothing -> Right $ QBndError qb
resolve delta imports Nothing bnd =
  case resolveBnd imports bnd of
    Left qbs -> check delta qbs
    Right r -> Right r

check :: Delta ty res -> NonEmpty QualifiedBinding -> Either (QualifiedBinding, FunType ty res) SymResError
check delta (qb:|[]) =
  case HM.lookup qb delta of
    Just t -> Left (qb t)
    Nothing -> Right $ NoTypeFound qb
check delta (qb:|(qbs:qbss)) =
  case HM.lookup qb delta of
    Just t ->
      case check delta (qbs:|qbss) of
            Left (qB, _) -> Right $ Ambiguity qb qB
            Right _ -> Left (qb,t)
    Nothing -> check delta (qbs:|qbss)

resolveQBnd :: [Import] -> QualifiedBinding -> QualifiedBinding
resolveQBnd [] qb = qb
resolveQBnd ((Alias ns alias):is) (QualifiedBinding ns' b) | NSRef [alias] == ns' = QualifiedBinding ns b
resolveQBnd (_:is) qb = resolveQB is qb

resolveBnd :: [Import] -> Binding -> Either (NonEmpty QualifiedBinding) SymResError
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
