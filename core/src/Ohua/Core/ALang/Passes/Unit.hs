module Ohua.Core.ALang.Passes.Unit where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.InternalFunctions (unitFun)


{-|
Performs the following transformation:
@
    let v = f () in e
@
becomes
@
    let v = Ohua.Core.lang/unitFn f () in e
@
where 
@
    unitFn :: (() -> a) -> () -> a
@
This way, we can now contextify the execution of f.
(Otherwise, this would not have been possible because in the imperative
host language, these functions do not take an argument.)
-}
mkUnitFunctionsExplicit :: Expr ty -> Expr ty
mkUnitFunctionsExplicit e =
    flip transform e $ \case
        Let v (Apply (Lit (FunRefLit fun@(FunRef _bnd _id fTy))) u@(Lit UnitLit)) ie ->
            Let
                v
                ((Lit $ FunRefLit $ FunRef unitFun Nothing $ FunType [TypeFunction fTy,  TypeUnit] (TypeFunction fTy) ) `Apply`
                 (Lit $ FunRefLit fun) `Apply`
                 u)
                ie
        other -> other
