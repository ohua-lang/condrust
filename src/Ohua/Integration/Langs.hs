{-|

Module      : $Header$
Description : Specifies the supported languages.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}

module Ohua.Integration.Langs where

import Ohua.Prelude
import Ohua.Parser.Common as P
import Ohua.Compat.Go.Parser(parseGo)

definedLangs :: [(Text, Text, P.Parser)]
definedLangs =
    ( ".go"
    , "Go frontend for the algorithm language",
    parseGo) :
    []

getParser :: Text -> P.Parser
getParser ext
    | Just a <- find ((== ext) . view _1) definedLangs = a ^. _3
    | otherwise =
        error $ "No parser defined for files with extension '" <> ext <> "'"
