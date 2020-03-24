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
                <> "funDef" .= annos
                <> "name" .= name)

$(deriveToJSON defaultOptions ''CodeGenData)
-- $(deriveJSON defaultOptions ''CodeGenData)

generate :: CodeGen
generate = pure . encode
