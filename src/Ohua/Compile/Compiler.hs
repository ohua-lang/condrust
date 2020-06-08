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

import Control.Monad.RWS (tell, evalRWS)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Functor.Foldable hiding (fold)
import qualified Data.HashMap.Strict as HM
import Data.List (partition)
import Data.List.NonEmpty as NE (fromList)
import qualified Data.HashSet as Set
import qualified Data.Text as T
import Control.Lens (each, view, (%~), (^?), ix)

import Ohua.ALang.Lang as ALang
import qualified Ohua.Frontend.Lang as FrLang
import Ohua.Frontend as Fr (frontend)
import Ohua.Frontend.Lower (toAlang)

-- FIXME the namespaces are broken here! this should be: Ohua.Core.Compile
import qualified Ohua.Compile as OhuaCore (compile)
import qualified Ohua.Compile.Configuration as OhuaCoreConfig (passAfterDFLowering)
import Ohua.Compile.Config
import Ohua.Unit (cleanUnits)
import Ohua.Integration.Langs
import Ohua.Compile.Types


compile :: 
    ( CompM m
    , MonadReader (StageHandling, Bool) m ) 
    => FilePath -> CompilationScope -> FilePath -> m ()
compile inFile compScope outDir = do
    -- composition:
    let lang = getIntegration (toText $ takeExtension inFile)

    -- frontend
    (ctxt, ns) <- Fr.frontend lang compScope inFile
    -- middle end
    compiledAlgos <- 
        mapM 
            (\algo -> 
                (algo,) <$> 
                ohuaCoreCompilation $
                toAlang -- lower into ALang
                (algo^.algoCode))
            $ ns^.algos
    -- backend 
    B.backend compiledAlgos lang
    where
        core stageHandlings tailRecSupport expr = do
            (stageHandlings, tailRecSupport) <- ask
            alangExpr <- runGenBndT mempty expr
            OhuaCore.compile
                (def 
                    & stageHandling .~ stageHandlings
                    & transformRecursiveFunctions .~ tailRecSupport)
                (def {OhuaCoreConfig.passAfterDFLowering = cleanUnits})
                alangExpr


