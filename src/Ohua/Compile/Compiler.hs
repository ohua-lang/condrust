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
import Ohua.Core.Types.Environment as CoreEnv
import Ohua.Core.Compile.Configuration as CoreConfig
import Ohua.Core.Compile as Core (compile)
import qualified Ohua.Core.Compile.Configuration as CConfig
import Ohua.Backend as B (backend)
import Ohua.Compile.Lower.FrLang (toAlang)
import Ohua.Compile.Lower.DFLang (toTCLang)

import Ohua.Integration
import Ohua.Integration.Config as IConfig hiding (Options(..))
import Ohua.Integration.Lang
import Ohua.Integration.Architecture

import System.FilePath


compile :: CompM m
        => FilePath           -- ^ Input: path to the file to be compiled
        -> CompilationScope   -- ^ Frontend config: scope of the current compilation
        -> CoreEnv.Options    -- ^ Core config
        -> IConfig.Config       -- ^ Integration configuration
        -> FilePath           -- ^ Output: path to the file to be generated
        -> m ()
compile inFile compScope coreOpts integConf outDir =
    runIntegration
        (toText $ takeExtension inFile)
        integConf
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
        core = Core.compile coreOpts def

