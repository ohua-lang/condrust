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
import Ohua.Frontend.Transform.TailRec ( isRecAlgo )

import Control.Lens.Combinators ( over )


frontend :: forall m lang. (CompM m, Integration lang)
        => lang -> CompilationScope -> FilePath -> m (NS lang, Namespace (FrLang.Expr (Type lang)) (AlgoSrc lang))
frontend lang compScope inFile = do
        (langNs, (ns,reg)) <- load lang compScope inFile
        -- FIXME we should exclude recursive functions from stand-alone compilation. there is really no
        --       value in compiling them.
        ns'    <- updateExprs' ns transform
        ns''   <- resolveNS (ns',reg)
        ns'''  <- updateExprs' ns'' wellScopedness
        ns'''' <- loadTypes lang langNs ns'''
        return
          ( langNs
          -- we exclude recursive functions from stand-alone compilation.
          -- there is really no value in compiling them.
          , over algos (filter (not . isRecAlgo)) ns''''
          )
    where
        transform :: CompM m => Binding -> FrLang.Expr ty -> m (FrLang.Expr ty)
        transform b e = prepareRootAlgoVars =<< wellScopedness b e
        
        wellScopedness :: CompM m => Binding -> FrLang.Expr ty -> m (FrLang.Expr ty)
        wellScopedness b e = (\a -> check a >> return a) $ makeWellScoped b e
