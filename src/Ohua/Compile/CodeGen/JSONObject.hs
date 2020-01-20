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

import Ohua.Serialize.JSON ()
import Ohua.CodeGen.Iface

instance ToJSON Fun where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Fun where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON CodeGenData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CodeGenData where
    parseJSON = genericParseJSON defaultOptions

generate :: Text -> CodeGen
generate = pure . encode
