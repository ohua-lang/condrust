{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Transform.Load ( load )
import Ohua.Frontend.Transform.Calls
    ( makeImplicitFunctionBindingsExplicit )
import Ohua.Frontend.Transform.Resolve ( resolveNS )
import Ohua.Frontend.Transform.Envs ( prepareRootAlgoVars )
import Ohua.Frontend.Transform.State ( check )


frontend :: forall m lang. (CompM m, Integration lang) 
        => lang -> CompilationScope -> FilePath -> m (NS lang, Namespace (FrLang.Expr (Type lang)) (AlgoSrc lang))
frontend lang compScope inFile = do
        (langNs, ns) <- load lang compScope inFile
        ns' <- resolveNS ns
        ns'' <- updateExprs ns' transform
        ns''' <- loadTypes lang langNs ns''
        return (langNs, ns''')
    where
        transform :: CompM m => FrLang.Expr ty -> m (FrLang.Expr ty)
        transform e = 
                (\a -> check a >> return a) =<< 
                ( prepareRootAlgoVars 
                . makeImplicitFunctionBindingsExplicit -- FIXME is this really necessary?!
                ) e