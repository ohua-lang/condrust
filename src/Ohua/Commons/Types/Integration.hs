{-# LANGUAGE TemplateHaskell #-}
module Ohua.Commons.Types.Integration where

import Universum

import Ohua.Commons.Types.Bindings
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


-- ToDo: If we decide to do it this way, globals should also contain the type of global variables
--       For now I skip that because it would add another type parameter to all the other lenses and the Bindings should
--       suffice to identify globals in the backend.
newtype Global = Global Binding deriving (Show, Eq)


-- REMINDER: As long as not all expressions are typed and as long as therefore Ohua nodes are introduced with type
-- variables and as long as therefore I need to have a type propagation pass to type them....As long as this is the
-- case I need an extra field in the Algo definition to carry through the return type of the original code.  
-- Of course, we allready have this information present in the `algoInputCode`, but we can not use in the middle of the
-- compiler because a) there we don't know the concrete language we're compiling and hence not the patterns 
-- to match on to extract the type and b) we'd carry lots of stuff through the compile steps when all we need is one type.

--FIXME: Make ty more specific and establish dependency between ty and expr
declareLenses[d|
     data Algo expr inpt ty = Algo
          { algoName :: Binding
          , algoType :: ty
          , algoCode :: expr
          , algoInputCode :: inpt
          } deriving (Show, Eq)
    |]


-- | A Namespace consists of a reference, a list of Imports and a list of Algorithms extracted from a given 
--   input file. Each Algorithm in turn consists of the `algoCode`, the extracted/current compiler internal representation
--   of the algorithm and (maybe not perfectly named though) the `algoInputCode` carrying the original code of 
--   each compiled function.
declareLenses[d|
     data Namespace expr inpt ty = Namespace 
          { nsName :: NSRef
          , imports :: [Import]
          , globals :: [Global]
          , algos :: [Algo expr inpt ty]
          } deriving (Show, Eq)
    |]

updateExprs :: Monad m => Namespace expr1 inpt ty -> (expr1 -> m expr2) -> m (Namespace expr2 inpt ty)
updateExprs namespace f = updateExprs' namespace $ \_ e -> f e

updateExprs' :: Monad m => Namespace expr1 inpt ty -> (Binding -> expr1 -> m expr2) -> m (Namespace expr2 inpt ty)
updateExprs' namespace f = do
     algos' <-
          forM (namespace^.algos) $ \algo -> do
               algoCode' <- f (algo^.algoName) (algo^.algoCode)
               return $ over algoCode (const algoCode') algo
     return $ over algos (const algos') namespace


