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
  | AmbiguousImports [QualifiedBinding]


{-
Environments:
-------------
Gamma ... associates local variables to a type
Delta ... associates function literals to a type
-}

type Gamma ty res = HM.HashMap Binding (OhuaType ty res)
-- Question: Current representation of FunType allows Unresolved. DO we want to forbid this here?
type Delta ty res = HM.HashMap QualifiedBinding (Res.FunType ty res)


resolveSymbols :: Delta ty res -> [Import] -> Maybe NSRef -> Binding -> Either (QualifiedBinding, FunType ty res) SymResError
resolveSymbols delta mod_imports (Just nspace) bnd =
  -- trace ("Will resolve " <> show bnd <> "with imports " <> show mod_imports <> " and namespace " <> show nspace)$
  let potential_qbs = resolveQBnd mod_imports $ QualifiedBinding nspace bnd
      potential_defs = foldr 
        (\bnd defs -> case HM.lookup bnd delta of
                        Just ty -> (bnd, ty) : defs
                        Nothing -> defs)
        []
        potential_qbs
  in case potential_defs of
       [] -> Right $ QBndError (QualifiedBinding nspace bnd)
       [(qb, t)] -> Left (qb, t)
       (def:defs) -> Right $ AmbiguousImports potential_qbs
       
resolveSymbols delta mod_imports Nothing bnd =
  -- Bindings wihtout a namespace can be local (functions) or, imported by Full, Global or Alias import just like 
  -- So we treat them just like bindings with namespaces and add the empty/local namespace as a potential resolution
  resolveSymbols delta mod_imports (Just (NSRef [])) bnd


resolveQBnd :: [Import] -> QualifiedBinding -> [QualifiedBinding]
-- When we resolve a qualified Binding like Arc::clone, 
-- the following things might be in delta
--      Arc::clone
--      some::path::Arc::clone
--      some::path::ActualNameOfArc::clone (this would be the case if there was an import like use some::path::ActualNameOfArc as Arc)
-- what we'd expect/need from the imports in those cases are
--      [] -> if Arc::clone is defined locally we should find nothing in the imports
--      [Full (some, path) Arc: imps] -> if we imported Arc as a namespace itself i.e. some::path::Arc
--      [Glob (some) path : imps ] -> if we had the import some::path::* where path contains Arc
--      [Alias (some::path::ActualNameOfArc) Arc : imps] -> in case of some::path::ActualNameOfArc::clone
-- => That means, for the global imports we cannot tell which one introduced Arc -> we return all of them and 
--    if the symbol lookup in delta yields multiple results the import is ambiguose.
-- ToDo?: We could add a check here, to not add potential global namespaces if there already was a fully specified (i.e. unequivocal) import
resolveQBnd [] qb = [qb]
resolveQBnd (Full (NSRef impSpaces) ns : _imps) qb@(QualifiedBinding (NSRef funSpaces@(fs:fss)) b)
  | ns == fs = [QualifiedBinding (NSRef $ impSpaces ++ funSpaces) b]
resolveQBnd (Alias nspace alias: _imps) qb@(QualifiedBinding nspace' b)
  | NSRef [alias] == nspace' = [QualifiedBinding nspace b]
resolveQBnd (Glob (NSRef impSpaces) : imps) qb@(QualifiedBinding (NSRef funSpaces) b)
  = QualifiedBinding (NSRef $ impSpaces ++ funSpaces) b : resolveQBnd imps qb
resolveQBnd (_:is) bnd = resolveQBnd is bnd

