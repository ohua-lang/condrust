{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ohua.Frontend.SymbolResolution (
  resolveSymbols
  , SymResError(..)
  , Delta
  , Gamma
  ) where

import Ohua.Prelude hiding (alias, last, head, tail)
import qualified Ohua.Prelude as Res (FunType(..))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.List (last, head, tail)

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
-- Question: Current representation of FunType allows Unresolved. DO we want to forbid this here?
type Delta ty res = HM.HashMap QualifiedBinding (Res.FunType ty res)


resolveSymbols :: Delta ty res -> [Import] -> Maybe NSRef -> Binding -> Either (QualifiedBinding, FunType ty res ) SymResError
resolveSymbols delta mod_imports (Just nspace) bnd =
  -- trace ("Will resolve " <> show bnd <> "with imports " <> show mod_imports <> " and namespace " <> show nspace)$
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
-- When we resolve a qualified Binding like Arc::clone, 
-- the following things might be in delta
--      Arc::clone
--      some::path::Arc::clone
--      some::path::ActualNameOfArc::clone (this would be the case if there was an import like use some::path::ActualNameOfArc as Arc)
-- what we'd expect/need from the imports in those cases are
--      [] -> in case of Arc::clone we should find nothing in the imports
--      [Glob (some::path) Arc : imps] -> in case of some::path::Arc::clone we expect the full import of the namespace Arc
--      [Alias (some::path::ActualNameOfArc) Arc : imps] -> in case of some::path::ActualNameOfArc::clone
-- we do not expect a full import because that would mean an `use some::path::Arc::clone` and a call `Arc::clone`
resolveQBnd [] qb = qb
resolveQBnd (Alias nspace alias: _imps) qb@(QualifiedBinding nspace' b) | NSRef [alias] == nspace' = QualifiedBinding nspace b
resolveQBnd (Glob (NSRef impSpaces) : _imps) qb@(QualifiedBinding (NSRef funSpaces) b) | last impSpaces  == head funSpaces = QualifiedBinding (NSRef $ impSpaces ++ tail funSpaces) b
resolveQBnd (_:imps) qb = resolveQBnd imps qb

resolveBnd :: [Import] -> Binding -> Either (NonEmpty QualifiedBinding) SymResError
resolveBnd [] bnd = Right $ BndError bnd
resolveBnd ((Full nspace bnd') : is) bnd | bnd' == bnd = 
                                     case resolveBnd is bnd of
                                       Left other ->  Left ( QualifiedBinding nspace bnd NE.<| other)
                                       Right _ -> Left (QualifiedBinding nspace bnd :| [])
resolveBnd ((Glob nspace):is) bnd =
  case resolveBnd is bnd of
    Left other -> Left (QualifiedBinding nspace bnd NE.<| other)
    Right _ -> Left (QualifiedBinding nspace bnd :| [])
resolveBnd (_:is) bnd = resolveBnd is bnd
