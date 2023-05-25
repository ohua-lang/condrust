module Ohua.Core.ParseTools.Refs where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang

import Ohua.Prelude

-- ToDo: Replace all occurences with Alang equivalents and hide module from extern of core.
ohuaLangNS :: NSRef
ohuaLangNS = makeThrow ["ohua", "lang"]

mkQualVar :: NSRef -> Binding -> FunType ty -> Expr ty
mkQualVar nspace name0 = pureFunction (QualifiedBinding nspace name0) Nothing

mkOhuaLangRef :: Binding -> FunType ty -> Expr ty
mkOhuaLangRef = mkQualVar ohuaLangNS

ifBuiltin :: VarType ty -> Expr ty
ifBuiltin vTy = mkOhuaLangRef "if" (FunType [TypeBool ,vTy ,vTy ] vTy)

smapBuiltin :: Expr ty
-- HO for-loop: function -> collection to apply function to -> typeOfCollection ( return Type function)
smapBuiltin = mkOhuaLangRef "smap" (FunType [TypeVar, TypeVar] TypeVar )

seqBuiltin :: Expr ty
-- Operator (historisch) um eine Expression vor der anderen laufen zu lassen
-- ToDo: check if we still need that anywhere
seqBuiltin = mkOhuaLangRef "seq" (FunType [TypeVar, TypeVar] TypeVar )

funcTyConRef :: QualifiedBinding
funcTyConRef = QualifiedBinding ohuaLangNS "->"

funcTyConSBind :: SomeBinding
funcTyConSBind = Qual funcTyConRef

mkTuple :: QualifiedBinding
mkTuple = QualifiedBinding ohuaLangNS "(,)"
