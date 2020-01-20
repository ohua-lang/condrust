{-|

Module      : $Header$
Description : Configuration of the Ohua compiler.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

This module loads a yaml configuration that guides the compilation.

-}
module Ohua.Compile.Config where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

-- TODO: this is essentially part of the build system!
