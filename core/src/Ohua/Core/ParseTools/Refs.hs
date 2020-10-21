module Ohua.Core.ParseTools.Refs where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang

ohuaLangNS :: NSRef
ohuaLangNS = makeThrow ["ohua", "lang"]

mkQualVar :: NSRef -> Binding -> Expr ty
mkQualVar nspace name0 = pureFunction (QualifiedBinding nspace name0) Nothing

mkOhuaLangRef :: Binding -> Expr ty
mkOhuaLangRef = mkQualVar ohuaLangNS

ifBuiltin :: Expr ty
ifBuiltin = mkOhuaLangRef "if"

smapBuiltin :: Expr ty
smapBuiltin = mkOhuaLangRef "smap"

seqBuiltin :: Expr ty
seqBuiltin = mkOhuaLangRef "seq"

funcTyConRef :: QualifiedBinding
funcTyConRef = QualifiedBinding ohuaLangNS "->"

funcTyConSBind :: SomeBinding
funcTyConSBind = Qual funcTyConRef

mkTuple :: QualifiedBinding
mkTuple = QualifiedBinding ohuaLangNS "(,)"
