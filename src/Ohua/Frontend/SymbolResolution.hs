module Ohua.Frontend.SymbolResolution (
  resolveSymbols
  , SymResError(..)
  , Delta
  , Gamma
  ) where

import Ohua.UResPrelude hiding (alias)
import qualified Ohua.Prelude as Res (FunType(..))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

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

type Gamma ty res = HM.HashMap Binding (OhuaType ty res)
-- Question: Curent representation of FunType allows Unresolved. DO we want to forbid this here?
type Delta ty res = HM.HashMap QualifiedBinding (Res.FunType ty res)


resolveSymbols :: Delta ty res -> [Import] -> Maybe NSRef -> Binding -> Either (QualifiedBinding, FunType ty res ) SymResError
resolveSymbols delta mod_imports (Just nspace) bnd =
  let qb = resolveQBnd mod_imports $ QualifiedBinding nspace bnd
  in case HM.lookup qb delta of
       Just t -> Left (qb, t)
       Nothing -> Right $ QBndError qb
resolveSymbols delta mod_imports Nothing bnd =
  case resolveBnd mod_imports bnd of
    Left qbs -> check delta qbs
    Right r -> Right r

check :: Delta ty res -> NonEmpty QualifiedBinding -> Either (QualifiedBinding, FunType ty res) SymResError
check delta (qb:|[]) =
  case HM.lookup qb delta of
    Just t -> Left (qb, t)
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
resolveQBnd ((Alias nspace alias): _is) (QualifiedBinding nspace' b) | NSRef [alias] == nspace' = QualifiedBinding nspace b
resolveQBnd (_:is) qb = resolveQBnd is qb

resolveBnd :: [Import] -> Binding -> Either (NonEmpty QualifiedBinding) SymResError
resolveBnd [] bnd = Right $ BndError bnd
resolveBnd ((Full nspace bnd') : is) bnd | bnd' == bnd =
                                     case resolveBnd is bnd of
                                       Left other -> Left ( QualifiedBinding nspace bnd NE.<| other)
                                       Right _ -> Left (QualifiedBinding nspace bnd :| [])
resolveBnd ((Glob nspace):is) bnd =
  case resolveBnd is bnd of
    Left other -> Left (QualifiedBinding nspace bnd NE.<| other)
    Right _ -> Left (QualifiedBinding nspace bnd :| [])
resolveBnd (_:is) bnd = resolveBnd is bnd
