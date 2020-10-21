{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend where

import Ohua.Prelude

import Ohua.Frontend.Types
import Ohua.Frontend.Lang
import Ohua.Frontend.Transform.Load ( load )
import Ohua.Frontend.Transform.Calls
    ( makeImplicitFunctionBindingsExplicit )
import Ohua.Frontend.Transform.Resolve ( resolveNS )
import Ohua.Frontend.Transform.Envs ( prepareRootAlgoVars )
import Ohua.Frontend.Transform.State ( check )


frontend :: forall ty m lang. (CompM m, Integration lang) 
        => lang -> CompilationScope -> FilePath -> m (NS lang, Namespace (Expr ty))
frontend lang compScope inFile = do
        (langNs, ns) <- load lang compScope inFile
        ns' <- resolveNS ns
        ns'' <- updateExprs ns' transform
        ns''' <- loadTypes lang langNs ns''
        return (langNs, ns''')
    where
        transform :: CompM m => Expr ty -> m (Expr ty)
        transform e = 
                (\a -> check a >> return a) =<< 
                ( prepareRootAlgoVars 
                . makeImplicitFunctionBindingsExplicit -- FIXME is this really necessary?!
                ) e