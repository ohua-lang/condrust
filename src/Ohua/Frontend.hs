{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Transform.Load ( load )
import Ohua.Frontend.Transform.Scopedness ( makeWellScoped )
import Ohua.Frontend.Transform.Resolve ( resolveNS )
import Ohua.Frontend.Transform.Envs ( prepareRootAlgoVars )
import Ohua.Frontend.Transform.State ( check )


frontend :: forall m lang. (CompM m, Integration lang) 
        => lang -> CompilationScope -> FilePath -> m (NS lang, Namespace (FrLang.Expr (Type lang)) (AlgoSrc lang))
frontend lang compScope inFile = do
        (langNs, (ns,reg)) <- load lang compScope inFile
        ns'    <- updateExprs ns transform
        ns''   <- resolveNS (ns',reg)
        ns''' <- updateExprs ns'' wellScopedness
        ns'''' <- loadTypes lang langNs ns'''
        return (langNs, ns'''')
    where
        transform :: CompM m => FrLang.Expr ty -> m (FrLang.Expr ty)
        transform e = prepareRootAlgoVars =<< wellScopedness e
        
        wellScopedness :: CompM m => FrLang.Expr ty -> m (FrLang.Expr ty)
        wellScopedness e = (\a -> check a >> return a) $ makeWellScoped e