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
generate fileName cgdf =
    pure (unwrap $ fileName, encode cgdf)
