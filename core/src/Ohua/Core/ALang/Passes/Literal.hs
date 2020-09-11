module Ohua.Core.ALang.Passes.Literal where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.ALang.Refs as R (id)

{-|
Performs the following transformation:
@
    let x = lit in e
@
becomes
@
    let x = Ohua.Core.lang/id lit in e
@
This satisfies the fact that DFLang does not have the concept of a literal, 
everything is a function.
Note: This transformation is mostly necessary in combination with a contextification.
For example:
@
    let someAlgo = \a -> Ohua.Core.lang/seq(f a, ()) in
        body
@
Here, f has side-effects to a and we sequence further execution.
-}
literalsToFunctions :: MonadOhua m => Expression -> m Expression
literalsToFunctions e =
    flip transformM e $ \case
        Let v u@(Lit lit) ie ->
            case lit of
                UnitLit      -> mkFun v u ie
                NumericLit _ -> mkFun v u ie
                EnvRefLit _  -> mkFun v u ie
                _other -> throwError $ "Compiler invariant broken. Trying to convert literal to function: " <> show lit 
        other -> return other
    where 
        mkFun v lit body = return $ 
            Let
                v
                ((Lit $ FunRefLit $ FunRef R.id Nothing) `Apply` lit)
                body
