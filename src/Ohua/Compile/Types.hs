{-|

Module      : $Header$
Description : Types for the compiler.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Ohua.Compile.Types where

import Ohua.Prelude

import Ohua.Types (Error)
import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Convert as FC
import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Convert as BC
import Control.Monad.Trans.Control (MonadBaseControl) -- TODO find out if this is really needed

import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Lazy.Char8 as L

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


type CompM m = 
     ( MonadIO m
     , MonadBaseControl IO m
     , MonadError Error m
     , MonadLogger m
     , MonadLoggerIO m
     )

type LanguageFileSuffix = Text
type CompilationScope = HM.HashMap NSRef LanguageFileSuffix

-- | This registers all algos used in a given namespace with their qualified names.
type NamespaceRegistry = HM.HashMap QualifiedBinding FrLang.Expr

-- type Conversions expr pat = 
--      ( FC.ConvertExpr expr -- frontend
--      , FC.ConvertPat pat -- frontend
--      , BC.ConvertInto expr -- backend
--      -- now we can easily add another one from ALang to expr to create new functions.
--      -- actually all we need is a way to lower ALang into TCLang.
--      )

-- class 
--      -- (Conversions expr pat) => 
--      Integration lang where
--      -- TODO This type class should link back to the conversion type classes 
--      --      that frontend and backend should be based upon.
--      --      Currently, we do not expose any functions that require them,
--      --      as such, I could not find a way to fit them.
--      --      I think the key is to define a type for ctxt:
--      -- type Ctxt expr pat :: *
--      -- data Ctxt :: * -> * -> *
--      --      This type faces the developer of an Integration but not the user!
--      frontend :: CompM m 
--                -- => Ctxt expr pat
--                -> FilePath
--                -> lang
--                -> m (lang, T.Namespace FrLang.Expr)

--      backend  :: CompM m 
--                => lang 
--                -> Algos TCLang.Expr 
--                -> m [L.ByteString]  -- FIXME the byte array is insufficient because it does not provide the names for the files.

-- TODO connect this type class with the type classes for converting into and from expressions
class Integration lang where
     frontend :: CompM m => FilePath -> lang -> m (lang, Namespace FrLang.Expr)
     backend  :: CompM m => Algos TCLang.TCExpr -> lang -> m (NonEmpty (FilePath, L.ByteString))