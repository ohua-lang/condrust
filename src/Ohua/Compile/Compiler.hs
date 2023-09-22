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
module Ohua.Compile.Compiler
  ( compile
  , langInfo
  ) where

import Ohua.Prelude

import qualified  Ohua.Frontend as Fr (frontend)
import Ohua.Frontend.Types (CompilationScope)
import Ohua.Core.Types.Environment as CoreEnv
import Ohua.Core.Compile.Configuration as CoreConfig
import qualified Ohua.Core.Compile as Core (compile)
import qualified Ohua.Core.Compile.Configuration as CConfig
import qualified Ohua.Backend as B (backend)
import qualified Ohua.Backend.Config as BConfig (Options)
import qualified Ohua.Backend.Types as BT
import Ohua.Compile.Lower.FrLang (toAlang)
import Ohua.Compile.Lower.DFLang (toTCLang)

import Ohua.Integration (FullIntegration, runIntegration, langInfo)
import Ohua.Integration.Config as IConfig
import Ohua.Integration.Lang (Lang, Language)
import Ohua.Integration.Architecture (Arch, Architectures)

import System.FilePath

compile :: ErrAndLogM m
        => FilePath           -- ^ Input: path to the file to be compiled
        -> CompilationScope   -- ^ Frontend config: scope of the current compilation
        -> CoreEnv.Options    -- ^ Core config
        -> BConfig.Options    -- ^ Backend config
        -> IConfig.Config     -- ^ Integration configuration
        -> FilePath           -- ^ Output: path to the file to be generated
        -> m ()
compile inFile compScope coreOpts beConf integConf outDir =
    runIntegration
        (toText $ takeExtension inFile)
        integConf
        (compilation inFile compScope coreOpts beConf outDir)

compilation :: forall (lang::Lang) (arch::Arch) m.
    (ErrAndLogM m, FullIntegration lang arch)
    => FilePath
    -> CompilationScope
    -> CoreEnv.Options
    -> BConfig.Options

    -> FilePath
    -> Maybe (CConfig.CustomPasses (BT.Type (Language lang)))
    -> Language lang
    -> Architectures arch
    -> m ()
compilation inFile compScope coreOpts beConf outDir optimizations integration arch = do
    -- frontend: extract all algorithms (function definitions) from the given scope and
    --           transform them into the frontend language
    -- REMINDER: I need to keep brackets around (ctxt, n) here because frontend returns an object
    ((ctxt, extracted_algos), enc_module) <- Fr.frontend integration compScope inFile
    -- middle end:
    extracted_algos_as_ALang <- updateExprs extracted_algos toAlang
    extracted_algos_as_DFLang <- updateExprs extracted_algos_as_ALang core
    extracted_algos_final <- updateExprs extracted_algos_as_DFLang toTCLang
    -- backend
    B.backend outDir beConf extracted_algos_final ctxt arch enc_module

    where
        -- core ::forall (lang::Lang) (arch::Arch) m ty. (ErrAndLogM m, FullIntegration lang arch) => ty -> ALang.Expr ty -> m (DFLang.NormalizedDFExpr ty)
        core = Core.compile coreOpts coreOptimizations
        coreOptimizations =
          case optimizations of
            Nothing -> def
            Just (CConfig.CustomPasses pbn pan pad) ->
              def
              { passBeforeNormalize = passBeforeNormalize def >=> pbn
              -- FIXME I switch this here because I want to run before the TailRec
              --       rewrites but after normalize. Normally TailRec would not be
              --       hooked in like this but would be a first-class part of core.
              --       Hence, I would not have to do this.
              --       This must be part of sertel/ohua-core#7.
              , passAfterNormalize = pan >=> passAfterNormalize def
              , passAfterDFLowering = passAfterDFLowering def >=> pad
              }
