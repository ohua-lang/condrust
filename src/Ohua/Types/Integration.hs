{-# LANGUAGE TemplateHaskell #-}
module Ohua.Types.Integration where

import Universum

import Ohua.Types.Reference
import Control.Lens.TH


declareLenses[d|
     data Import 
          = Full -- fully qualified path. imports one binding.
               { ns :: NSRef
               , binding :: Binding
               }
          | Alias -- path with an alias. used in expression as prefix to import other bindings.
               { ns :: NSRef
               , alias :: Binding
               }
          | Glob -- path that imports all bindings in this path.
               { ns :: NSRef 
               }
          deriving (Show, Eq)
    |]

declareLenses[d|
     data Algo expr = Algo 
          { algoName :: Binding
          , algoCode :: expr
          } deriving (Show, Eq)
    |]

type Imports = [Import]
type Algos expr = [Algo expr]

declareLenses[d|
     data Namespace expr = Namespace 
          { nsName :: NSRef
          , imports :: Imports
          , algos :: Algos expr
          } deriving (Show, Eq)
    |]

updateExprs :: Monad m => Namespace expr1 -> (expr1 -> m expr2) -> m (Namespace expr2)
updateExprs namespace f = do
     algos' <- 
          forM (namespace^.algos) $ \algo -> do
               algoCode' <- f $ algo^.algoCode
               return $ over algoCode (const algoCode') algo
     return $ over algos (const algos') namespace

