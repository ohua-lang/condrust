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
import qualified Ohua.Core.Compile.Configuration as CConfig
import Ohua.Backend as B (backend)
import Ohua.Compile.Lower.FrLang (toAlang)
import Ohua.Compile.Lower.DFLang (toTCLang)

import Ohua.Integration
import Ohua.Integration.Lang
import Ohua.Integration.Architecture

import System.FilePath


compile :: CompM m
    => FilePath -> CompilationScope -> Options -> FilePath -> m ()
compile inFile compScope coreOpts outDir =
    runIntegration
        (toText $ takeExtension inFile)
        "sm" -- TODO integrate backend architecture option into config
        (compilation inFile compScope coreOpts outDir)

compilation :: forall (lang::Lang) (arch::Arch) m.
    (CompM m, FullIntegration lang arch)
    =>
    FilePath -> CompilationScope -> Options -> FilePath
    -> Maybe CConfig.CustomPasses
    -> Language lang -> Architectures arch -> m ()
compilation inFile compScope coreOpts outDir optimizations integration arch = do
    -- frontend
    (ctxt, n) <- Fr.frontend integration compScope inFile
    -- middle end
    n' <- updateExprs n $ toAlang >=> core >=> toTCLang
    -- backend
    B.backend outDir n' ctxt arch

    where
        core = Core.compile coreOpts coreOptimizations
        coreOptimizations =
          case optimizations of
            Nothing -> def
            Just (CConfig.CustomPasses pbn pan pad) ->
              def
              { passBeforeNormalize = passBeforeNormalize def >=> pbn
              , passAfterNormalize = passAfterNormalize def >=> pan
              , passAfterDFLowering = passAfterDFLowering def >=> pad
              }
