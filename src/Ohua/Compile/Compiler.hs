{-|

Module      : $Header$
Description : The Ohua compiler.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

-}
module Ohua.Compile.Compiler where

import Ohua.Prelude

import Ohua.Frontend as Fr (frontend)
import Ohua.Frontend.Types (CompilationScope)
import Ohua.Core.Types.Environment (Options)
import Ohua.Core.Compile.Configuration as CoreConfig
import Ohua.Core.Compile as Core (compile)
import Ohua.Core.Unit (cleanUnits)
import Ohua.Backend as B (backend)
import Ohua.Compile.Lower.FrLang (toAlang)
import Ohua.Compile.Lower.DFLang (toTCLang)

import Ohua.Compile.Config
import Ohua.Integration

import Control.Lens
import System.FilePath


compile :: CompM m 
    => FilePath -> CompilationScope -> Options -> FilePath -> m ()
compile inFile compScope coreOpts outDir = do
    -- composition:
    let lang = getIntegration (toText $ takeExtension inFile)

    -- frontend
    (ctxt, ns) <- Fr.frontend lang compScope inFile
    -- middle end
    ns' <- updateExprs ns $ toAlang >=> core >=> toTCLang
    -- backend 
    B.backend outDir ns' lang
    where
        core = Core.compile
                coreOpts
                (def {CoreConfig.passAfterDFLowering = cleanUnits})


