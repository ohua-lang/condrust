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
     data Algo expr anno = Algo 
          { algoName :: Binding
          , algoCode :: expr
          , algoAnno :: anno
          } deriving (Show, Eq)
    |]

declareLenses[d|
     data Namespace expr anno = Namespace 
          { nsName :: NSRef
          , imports :: [Import]
          , algos :: [Algo expr anno]
          } deriving (Show, Eq)
    |]

updateExprs :: Monad m => Namespace expr1 anno -> (expr1 -> m expr2) -> m (Namespace expr2 anno)
updateExprs namespace f = updateExprs' namespace $ \_ e -> f e

updateExprs' :: Monad m => Namespace expr1 anno -> (Binding -> expr1 -> m expr2) -> m (Namespace expr2 anno)
updateExprs' namespace f = do
     algos' <- 
          forM (namespace^.algos) $ \algo -> do
               algoCode' <- f (algo^.algoName) (algo^.algoCode)
               return $ over algoCode (const algoCode') algo
     return $ over algos (const algos') namespace