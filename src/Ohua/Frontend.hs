{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Types
import qualified Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Transform.Load ( loadAlgosAndImports  )
import Ohua.Frontend.Transform.Resolve ( resolveNS )
-- import Ohua.Frontend.Transform.Envs ( prepareRootAlgoVars )
import Ohua.Frontend.Transform.TailRec ( isRecAlgo )
import Ohua.Frontend.Transform.FinalLiterals ( noFinalLiterals )
import Ohua.Frontend.Transform.UnitArgs ( noEmptyArgs )
import Ohua.Frontend.TypeSystem ( toWellTyped )

import Control.Lens.Combinators ( over )


frontend :: forall m lang. (ErrAndLogM m, Integration lang)
         => lang
         -> CompilationScope
         -> FilePath
         -> m ((HostModule lang, Namespace (FrLang.Expr (Type lang) Resolved) (AlgoSrc lang)), HostModule lang)
frontend lang compScope inFile = do
        (langNs, ns, reg, placeholder) <- loadAlgosAndImports lang compScope inFile
        ns'                            <- updateExprs ns trans
        ns''                           <- resolveNS (ns',reg)
        delta                          <- loadTypes lang langNs ns''

        -- we exclude recursive functions from stand-alone compilation.
        let ns'''                      =  over algos (filter (not . isRecAlgo)) ns''
        -- FIXME: Where are the imports in `toWellTyped  delta imports` are supposed to come from 
        ns''''                         <- updateExprs ns''' (toWellTyped delta [] )
        return ((langNs, ns''''), placeholder)
    where
        trans :: ErrAndLogM m => FrLang.Expr ty Unresolved -> m (FrLang.Expr ty Unresolved)
        trans e = noFinalLiterals e >> noEmptyArgs e -- prepareRootAlgoVars e
