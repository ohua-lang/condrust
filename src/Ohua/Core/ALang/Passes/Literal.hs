{-# LANGUAGE LambdaCase #-}
module Ohua.Core.ALang.Passes.Literal where

import Ohua.Core.Prelude

import Ohua.Core.ALang.Lang
import Ohua.Core.InternalFunctions as IFuns (id)

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
literalsToFunctions :: MonadOhua m => Expr embExpr annot ty -> m (Expr embExpr annot ty)
literalsToFunctions e =
    flip transformM e $ \case
        Let v (Lit lit) ie ->
            case lit of
                UnitLit      -> mkFun v lit ie
                NumericLit _ -> mkFun v lit ie
                StringLit _ -> mkFun v lit ie
                BoolLit _ -> mkFun v lit ie
                -- ToDo: Verify that we want to do this (in any case of hostcode embedding)
                HostLit _ _ -> mkFun v lit ie
                EnvRefLit _ _ -> mkFun v lit ie
                FunRefLit (FunRef qb _) -> throwError $ "Compiler invariant broken. Trying to convert function literal to function: " <> show qb
        other -> return other
    where
        mkFun v lit body = do
            let litType = getLitType lit
            return $
                Let v
                    (Apply [] 
                        (Lit (FunRefLit $ FunRef IFuns.id (FunType (Right $ litType :| []) litType )))
                        (Lit lit)
                    )
                    body
