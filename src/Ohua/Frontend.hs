{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Transform.Load ( loadAlgosAndImports  )
import Ohua.Frontend.Transform.Resolve ( resolveNS )
import Ohua.Frontend.Transform.Envs ( prepareRootAlgoVars )
import Ohua.Frontend.Transform.TailRec ( isRecAlgo )
import Ohua.Frontend.Transform.FinalLiterals ( noFinalLiterals )

import Control.Lens.Combinators ( over )


frontend :: forall m lang. (ErrAndLogM m, Integration lang)
         => lang
         -> CompilationScope
         -> FilePath
         -> m ((HostModule lang, Namespace (FrLang.RawExpr (Type lang)) (AlgoSrc lang)), HostModule lang)
frontend lang compScope inFile = do
        (langNs, ns, reg, placeholder) <- loadAlgosAndImports lang compScope inFile
        ns'                            <- updateExprs' ns trans
        ns''                           <- resolveNS (ns',reg)
        (delta, ns''')                 <- loadTypes lang langNs ns''
        let (_, ns'''') =
              ( langNs
              -- we exclude recursive functions from stand-alone compilation.
              -- there is really no value in compiling them.
              , over algos (filter (not . isRecAlgo)) ns'''
              )
        ns'''''                        <- updateExprs' ns'''' (toWellTyped delta)
        return (ns''''', placeholder)
    where
        trans :: ErrAndLogM m => FrLang.Expr ty -> m (FrLang.Expr ty)
        trans e = prepareRootAlgoVars <$>  noFinalLiterals
