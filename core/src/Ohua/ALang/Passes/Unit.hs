module Ohua.ALang.Passes.Unit where

import Ohua.Prelude

import Ohua.ALang.Lang


{-|
Performs the following transformation:
@
    let v = f () in e
@
becomes
@
    let v = ohua.lang/unitFn f () in e
@
where 
@
    unitFn :: (() -> a) -> () -> a
@
This way, we can now contextify the execution of f.
(Otherwise, this would not have been possible because in the imperative
host language, these functions do not take an argument.)
-}
mkUnitFunctionsExplicit :: Expression -> Expression
mkUnitFunctionsExplicit e =
    flip transform e $ \case
        Let v (Apply (Lit (FunRefLit (FunRef f _))) u@(Lit UnitLit)) ie ->
            Let
                v
                ((Lit $ FunRefLit $ FunRef "ohua.lang/unitFn" Nothing) `Apply`
                 (Lit $ FunRefLit $ FunRef f Nothing) `Apply`
                 u)
                ie
        other -> other
