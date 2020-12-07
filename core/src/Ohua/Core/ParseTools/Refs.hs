module Ohua.Core.ParseTools.Refs where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang

ohuaLangNS :: NSRef
ohuaLangNS = makeThrow ["ohua", "lang"]

mkQualVar :: NSRef -> Binding -> FunType ty -> Expr ty
mkQualVar nspace name0 = pureFunction (QualifiedBinding nspace name0) Nothing

mkOhuaLangRef :: Binding -> FunType ty -> Expr ty
mkOhuaLangRef = mkQualVar ohuaLangNS

ifBuiltin :: Expr ty
ifBuiltin = mkOhuaLangRef "if" (FunType [TypeVar,TypeVar,TypeVar])

smapBuiltin :: Expr ty
smapBuiltin = mkOhuaLangRef "smap" (FunType [TypeVar,TypeVar])

seqBuiltin :: Expr ty
seqBuiltin = mkOhuaLangRef "seq" (FunType [TypeVar,TypeVar])

funcTyConRef :: QualifiedBinding
funcTyConRef = QualifiedBinding ohuaLangNS "->"

funcTyConSBind :: SomeBinding
funcTyConSBind = Qual funcTyConRef

mkTuple :: QualifiedBinding
mkTuple = QualifiedBinding ohuaLangNS "(,)"
