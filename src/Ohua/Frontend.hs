{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Types
import qualified Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Transform.Load ( loadAlgosAndImports  )
import Ohua.Frontend.Transform.Resolve ( resolveNS )
import Ohua.Frontend.Transform.Envs ( prepareRootAlgoVars )
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
        nsTransf                       <- updateExprs ns trans
        nsReslvd                       <- resolveNS (nsTransf,reg)
        delta                          <- loadTypes lang langNs nsReslvd

        -- we exclude recursive functions from stand-alone compilation.
        let nsNoRecs                      =  over algos (filter (not . isRecAlgo)) nsReslvd
        nsWellTypd                        <- updateExprs nsNoRecs (toWellTyped delta (nsNoRecs^.imports))
        nsLetRoot                         <- updateExprs nsWellTypd prepareRootAlgoVars
        return ((langNs, nsLetRoot ), placeholder)
    where
        trans :: ErrAndLogM m => FrLang.Expr ty Unresolved -> m (FrLang.Expr ty Unresolved)
        trans e = noFinalLiterals e >> noEmptyArgs e
