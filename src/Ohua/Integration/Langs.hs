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

import Ohua.Compile.Types
import Ohua.Integration.Rust
-- import Ohua.Integration.Go

data Integrated where
    Integrated :: Integration lang => lang -> Integrated


definedIntegrations :: [(Text, Text, Integrated)]
definedIntegrations =
    [
    --  ( ".go"
    --  , "Go integration"
    --  , Go
    --  )
     ( ".rs"
     , "Rust integration"
     , Integrated Rust
     )
    ]

getIntegration :: Text -> Integrated
getIntegration ext
    | Just a <- find ((== ext) . view _1) definedIntegrations = a ^. _3
    | otherwise =
        error $ "No language integration defined for files with extension '" <> ext <> "'"
