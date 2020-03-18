{-|

Module      : $Header$
Description : This backend just dumps the results to a JSON file.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Ohua.Compile.CodeGen.JSONObject where

import Ohua.Prelude

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser)

import Ohua.Serialize.JSON
import Ohua.Compile.CodeGen.Iface
import Ohua.Frontend.NS (FunAnn)
import Ohua.Types (TyExpr, SomeBinding)
import Ohua.Parser.Common hiding (Parser)
import Ohua.DFGraph (OutGraph)

instance ToJSONKey Binding
instance FromJSONKey Binding

instance ToJSON Fun where
    toJSON = undefined 

    toEncoding (Fun gr annos name) =
        pairs ("graph" .= gr
                <> "annotations" .= annos
                <> "name" .= name)

-- I just could not solve this ambiguity on the type variable 'a'.
-- It might be possible that the GHC really really needs a concrete type at this moment.
-- instance FromJSON Fun where
--     -- parseJSON :: Value -> Parser Fun
--     parseJSON = go
--         where 
--             go :: forall a. BackendSupport a => Value -> Parser Fun
--             go = withObject "Fun" $ \v -> fun -- :: BackendSupport a => OutGraph -> Annotated a (FunAnn (TyExpr SomeBinding)) -> Binding -> Fun)
--                 <$> v .: "graph" -- :: Parser OutGraph)
--                 <*> v `annos` "annotations" -- :: BackendSupport a => Parser (Annotated a (FunAnn (TyExpr SomeBinding))))
--                 <*> v .: "name" -- :: Parser Binding)
--                     where
--                         fun :: OutGraph -> Annotated a (FunAnn (TyExpr SomeBinding)) -> Binding -> Fun
--                         fun = Fun
--                         annos :: Object -> Text -> Parser (Annotated a (FunAnn (TyExpr SomeBinding)))
--                         annos = (.:)

$(deriveToJSON defaultOptions ''CodeGenData)
-- $(deriveJSON defaultOptions ''CodeGenData)

generate :: CodeGen
generate = pure . encode
