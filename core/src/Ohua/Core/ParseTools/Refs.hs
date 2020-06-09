module Ohua.Core.ParseTools.Refs where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang

ohuaLangNS :: NSRef
ohuaLangNS = makeThrow ["ohua", "lang"]

mkQualVar :: NSRef -> Binding -> Expr
mkQualVar nspace name0 = PureFunction (QualifiedBinding nspace name0) Nothing

mkOhuaLangRef :: Binding -> Expr
mkOhuaLangRef = mkQualVar ohuaLangNS

ifBuiltin :: Expr
ifBuiltin = mkOhuaLangRef "if"

smapBuiltin :: Expr
smapBuiltin = mkOhuaLangRef "smap"

seqBuiltin :: Expr
seqBuiltin = mkOhuaLangRef "seq"

funcTyConRef :: QualifiedBinding
funcTyConRef = QualifiedBinding ohuaLangNS "->"

funcTyConSBind :: SomeBinding
funcTyConSBind = Qual funcTyConRef

funcTyCon :: SomeTyVar
funcTyCon = TyCon funcTyConSBind

mkFunc :: DefaultTyExpr -> DefaultTyExpr -> DefaultTyExpr
mkFunc a b = TyRef funcTyCon `TyApp` a `TyApp` b

mkTuple :: QualifiedBinding
mkTuple = QualifiedBinding ohuaLangNS "(,)"
