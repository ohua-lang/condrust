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


frontend :: (CompM m, Integration lang) 
        => lang -> CompilationScope -> FilePath -> m (Lang lang, Namespace Expr)
frontend lang compScope inFile = do
        (lang', ns) <- load lang compScope inFile
        ns' <- resolveNS ns
        ns'' <- updateExprs ns' transform
        return (lang', ns'')
    where
        transform :: CompM m => Expr -> m Expr
        transform e = 
                (\a -> check a >> return a) =<< 
                ( prepareRootAlgoVars 
                . makeImplicitFunctionBindingsExplicit -- FIXME is this really necessary?!
                ) e