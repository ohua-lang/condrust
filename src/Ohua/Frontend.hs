{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Transform.Load ( loadAlgosAndImports  )
import Ohua.Frontend.Transform.Scopedness ( makeWellScoped )
import Ohua.Frontend.Transform.Resolve ( resolveNS )
import Ohua.Frontend.Transform.Envs ( prepareRootAlgoVars )
-- import Ohua.Frontend.Transform.State ( checkLinearUsage )
import Ohua.Frontend.Transform.TailRec ( isRecAlgo )
import Ohua.Frontend.Transform.FinalLiterals ( transformFinalLiterals )

import Control.Lens.Combinators ( over )


frontend :: forall m lang. (ErrAndLogM m, Integration lang)
         => lang
         -> CompilationScope
         -> FilePath
         -> m ((HostModule lang, Namespace (FrLang.Expr (Type lang)) (AlgoSrc lang) (HostType (Type lang))), HostModule lang)
frontend lang compScope inFile = do
        (langNs, ns, reg, placeholder) <- loadAlgosAndImports lang compScope inFile
        -- some transformation steps on the algorithm expressions 
        ns'    <- updateExprs' ns trans
        -- inline functions from scope
        ns''   <- resolveNS (ns',reg)
        -- apply transformation (again) to also catch inlined code
        ns'''  <- updateExprs' ns'' wellScopedness
        -- 
        ns'''' <- loadTypes lang langNs ns'''
        let finalNs@(_, _fNs) =
              ( langNs
              -- QUESTION: Why?
              -- we exclude recursive functions from stand-alone compilation.
              -- there is really no value in compiling them.
              , over algos (filter (not . isRecAlgo)) ns''''
              )
        -- _      <- updateExprs' fNs linearState
        return (finalNs, placeholder)
    where
        trans :: ErrAndLogM m => Binding -> FrLang.Expr ty -> m (FrLang.Expr ty)
        trans b e = prepareRootAlgoVars .  transformFinalLiterals =<< wellScopedness b e

        -- | Make sure that every variable is either let-bound in the scope before usage i.e. actually a local variable
        --   or considered a reference to a function imported or defined in scope
        -- QUESTION: Why do we do this? Shouldn't that be given by the parser/AST?        
        wellScopedness :: ErrAndLogM m => Binding -> FrLang.Expr ty -> m (FrLang.Expr ty)
        wellScopedness b e = pure $ makeWellScoped b e

        -- REMINDER: We need to add linear state use to the 'official assumptions' of our programming model (and write that down somewhere)
        -- REMINDER: This function is currently pointless. We keep it (commented out) as a placeholder, to remind us
        -- where the programming model should be checked 
        -- linearState :: ErrAndLogM m => Binding -> FrLang.Expr ty -> m ()
        -- linearState _ = (trace "Checking linear state usage") checkLinearUsage
