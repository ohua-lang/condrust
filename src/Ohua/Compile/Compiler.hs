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

import System.FilePath

compile :: CompM m 
    => FilePath -> CompilationScope -> Options -> FilePath -> m ()
compile inFile compScope coreOpts outDir = do
    -- composition:
    let lang = getIntegration (toText $ takeExtension inFile)

    -- frontend
    (ctxt, ns) <- Fr.frontend lang compScope inFile
    -- middle end
    compiledAlgos <- 
        mapM 
            (\algo -> 
                (algo,) <$> 
                core $
                toAlang -- lower into ALang
                (algo^.algoCode))
            $ ns^.algos
    -- backend 
    B.backend compiledAlgos lang
    where
        core expr = do
            alangExpr <- runGenBndT mempty expr
            Core.compile
                coreOpts
                (def {CoreConfig.passAfterDFLowering = cleanUnits})
                alangExpr


