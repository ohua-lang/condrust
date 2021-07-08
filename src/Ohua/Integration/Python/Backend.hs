module Ohua.Integration.Python.Backend where

import Ohua.Prelude

import Ohua.Backend.Lang as TCLang
import Ohua.Backend.Types as B

import Ohua.Integration.Lang hiding (Lang)
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.Types
import qualified Language.Python.Common as PyCom
--import qualified 


instance Integration (Language 'Python) where
    type NS (Language 'Python) = Error 
    type Type (Language 'Python) = Error 
    type AlgoSrc (Language 'Python) = Error 

    type Expr (Language 'Python) = Error 
    type Task (Language 'Python) = Error 

    

-- TODO: Make Language Python an instance of Backend.Integration
{-
class Integration lang where
    type NS lang :: *
    type Type lang :: *
    type AlgoSrc lang :: *

    type Expr lang :: *
    type Task lang :: *

    convertExpr :: (Architecture arch, Lang arch ~ lang) => arch -> TaskExpr (Type lang) -> Expr lang

    -- TODO I believe now that this function does not belong into the interface anymore!
    lower :: 
        ( CompM m
        , Architecture arch
        , Lang arch ~ lang
        , ty ~ (Type lang))
        => NS lang
        -> arch
        -> Namespace (Program (Channel ty) (Com 'Recv ty) (TaskExpr ty) ty) (AlgoSrc lang)
        -> m (Namespace (Program (Channel ty) (Com 'Recv ty) (Task lang) ty) (AlgoSrc lang))
-}