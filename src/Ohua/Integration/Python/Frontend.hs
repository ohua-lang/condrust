module Ohua.Integration.Python.Frontend where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Types
import Ohua.Frontend.Convert
import Ohua.Frontend.PPrint ()

import Ohua.Integration.Lang

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Error 
    type Type (Language 'Python) = Error 
    type AlgoSrc (Language 'Python) = Error 


--TODO: Make Language Python an instance of Frontend.Integration
{-
class Integration lang where
    type NS lang :: *
    type Type lang :: *
    type AlgoSrc lang :: *

    loadNs :: CompM m => 
        lang -> FilePath -> m (NS lang, Namespace (Expr (Type lang)) (AlgoSrc lang))
    loadTypes :: CompM m => 
        lang -> NS lang -> Namespace (Expr (Type lang)) (AlgoSrc lang) -> m (Namespace (Expr (Type lang)) (AlgoSrc lang))
-}