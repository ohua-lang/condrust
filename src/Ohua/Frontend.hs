{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Types
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Transform.Load ( load )
import Ohua.Frontend.Transform.Scopedness ( makeWellScoped )
import Ohua.Frontend.Transform.Resolve ( resolveNS )
import Ohua.Frontend.Transform.Envs ( prepareRootAlgoVars )
import Ohua.Frontend.Transform.State ( checkLinearUsage )
import Ohua.Frontend.Transform.TailRec ( isRecAlgo )
import Ohua.Frontend.Transform.FinalLiterals ( transformFinalLiterals )

import Control.Lens.Combinators ( over )

-- REMINDER: Definition of the frontend interface needs to be 
--           adapted when placeholder type changes

frontend :: forall m lang. (CompM m, Integration lang)
         => lang
         -> CompilationScope
         -> FilePath
         -> m ((NS lang, Namespace (FrLang.Expr (Type lang)) (AlgoSrc lang)), NS lang)
frontend lang compScope inFile = do
        (langNs, ns, reg, placeholder) <- load lang compScope inFile
        -- some transformation steps on the algorithm expressions 
        ns'    <- updateExprs' ns trans
        -- inline functions from scope
        ns''   <- resolveNS (ns',reg)
        -- apply transformation (again) to also catch inlined code
        ns'''  <- updateExprs' ns'' wellScopedness
        -- 
        ns'''' <- loadTypes lang langNs ns'''
        let finalNs@(_, fNs) =
              ( langNs
              -- QUESTION: Why?
              -- we exclude recursive functions from stand-alone compilation.
              -- there is really no value in compiling them.
              , over algos (filter (not . isRecAlgo)) ns''''
              )
        _      <- updateExprs' fNs linearState
        return (finalNs, placeholder)
    where
        trans :: CompM m => Binding -> FrLang.Expr ty -> m (FrLang.Expr ty)
        trans b e = (trace $ "after transform: " <> show e <> "\n") prepareRootAlgoVars . (trace $ "after wellscoping: " <> show e <> "\n") transformFinalLiterals =<<  (trace $ "before: " <> show e <> "\n") wellScopedness b e

        -- | Make sure that every variable is either let-bound in the scope before usage i.e. actually a local variable
        --   or considered a reference to a function imported or defined in scope
        
        -- QUESTION: Why do we do this? Shouldn't that be given by the parser/AST?        
        wellScopedness :: CompM m => Binding -> FrLang.Expr ty -> m (FrLang.Expr ty)
        wellScopedness b e = pure $ makeWellScoped b e
        
        -- REMINDER: We need to add linear state use to the 'official assumptions' of our programming model (and write that down somewhere)
        linearState :: CompM m => Binding -> FrLang.Expr ty -> m ()
        linearState _ = checkLinearUsage
