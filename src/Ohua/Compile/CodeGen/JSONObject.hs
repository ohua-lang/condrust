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
module Ohua.Compile.CodeGen.JSONObject where

import Ohua.Prelude

import Data.Aeson

import Ohua.Serialize.JSON
import Ohua.Compile.CodeGen.Iface
import Ohua.Frontend.NS (FunAnn)
import Ohua.Types (TyExpr, SomeBinding)

instance ToJSONKey Binding
instance FromJSONKey Binding

instance ToJSON SomeBinding where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SomeBinding where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON (TyExpr SomeBinding) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (TyExpr SomeBinding) where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON (FunAnn (TyExpr SomeBinding)) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (FunAnn (TyExpr SomeBinding)) where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON Fun where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Fun where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON CodeGenData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CodeGenData where
    parseJSON = genericParseJSON defaultOptions

generate :: CodeGen
generate = pure . encode
