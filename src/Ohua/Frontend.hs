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
        -- 1. Extract: (input file in host language,
        --              Integration.Namespace containing Integration.Algo's, Integration.Imports's and Integration.Global's of the input module,
        --              the 'register' a map {algo_name: lowered code} used for inlining the algorithms,
        --              placeholder (potentialy) containing host language code we "wrapped" out of the input code because we cannot compile it)
        (langNs, ns, reg, placeholder) <- loadAlgosAndImports lang compScope inFile
        -- Inline algo code from the register
        nsInlined                      <- resolveNS (ns,reg)
        -- make sure all functions have at least a Unit argument and no algo returns a literal 
        nsTransf                       <- updateExprs nsInlined trans
        -- Extract Delta i.e. the types of all functions used in the algos as {function_name:type}
        delta                          <- loadTypes lang langNs nsTransf
        -- Exclude recursive functions from stand-alone compilation.
        let nsNoRecs                      =  over algos (filter (not . isRecAlgo)) nsTransf
        -- FIXME: We ignore global definitions from the Namespace here
        -- Check typing of algorithms and propagate type information from Delta 
        nsWellTypd                        <- updateExprs nsNoRecs (toWellTyped delta (nsNoRecs^.imports))
        -- Transform algos with multiple arguments to nested one argument functions
        nsLetRoot                         <- updateExprs nsWellTypd prepareRootAlgoVars
        return ((langNs, nsLetRoot), placeholder)
    where
        trans :: ErrAndLogM m => FrLang.Expr ty Unresolved -> m (FrLang.Expr ty Unresolved)
        trans e = noFinalLiterals e >> noEmptyArgs e
