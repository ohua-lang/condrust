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
{-# LANGUAGE CPP, ConstraintKinds #-}
module Ohua.Compile.Compiler where

import Ohua.Prelude

import Control.Concurrent.Async.Lifted
import Control.Monad.RWS (tell, evalRWS)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Functor.Foldable hiding (fold)
import qualified Data.HashMap.Strict as HM
import Data.List (partition)
import Data.List.NonEmpty as NE (fromList)
import qualified Data.HashSet as Set
import qualified Data.Text as T
import System.Directory
import System.FilePath as Path ((<.>), takeExtension)
import Control.Lens (each, view, (%~), (^?), ix)

import Ohua.ALang.Lang as ALang
import qualified Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Lower (toAlang)
import Ohua.DFGraph (OutGraph)
-- FIXME the namespaces are broken here! this should be: Ohua.Core.Compile
import qualified Ohua.Compile as OhuaCore (compile)
import qualified Ohua.Compile.Configuration as OhuaCoreConfig (passAfterDFLowering)
import Ohua.Compile.Config
import Ohua.Compile.Transform.Resolve (resolveNS)
import Ohua.Compile.Transform.Load (load)
import Ohua.Unit (cleanUnits)
import Ohua.Integration.Langs
import Ohua.Compile.Types


ohuacCompilation :: (CompM m, Integration lang) => lang -> CompilationScope -> FilePath -> m (Namespace FrLang.Expr)
ohuacCompilation lang compScope inFile = resolveNS =<< load lang compScope inFile

ohuaCoreCompilation :: CompM m => StageHandling -> Bool -> FrLang.Expr -> m OutGraph
ohuaCoreCompilation stageHandlings tailRecSupport expr = do
    expr' <- prepareRootAlgoVars expr
    let expr'' = transformFinalUnit expr'
    alangExpr <- runGenBndT mempty $ toAlang expr''
    OhuaCore.compile
        (def 
            & stageHandling .~ stageHandlings
            & transformRecursiveFunctions .~ tailRecSupport)
        (def {OhuaCoreConfig.passAfterDFLowering = cleanUnits})
        alangExpr

-- TODO move these transformations into their own module
prepareRootAlgoVars :: CompM m => FrLang.Expr -> m FrLang.Expr
prepareRootAlgoVars (FrLang.LamE vars body) =  go 0 vars body
  where
    go i (x:xs) rest =
        go (i+1) xs $ FrLang.LetE x (FrLang.LitE $ EnvRefLit $ makeThrow i) rest
    go _ [] rest = return rest
prepareRootAlgoVars _ = throwError "compiler invariant broken"

transformFinalUnit :: FrLang.Expr -> FrLang.Expr
transformFinalUnit = cata $ \case
    FrLang.StmtEF e u@(FrLang.LitE UnitLit) -> FrLang.SeqE e u
    e -> embed e

compile :: 
    ( CompM m
    , MonadReader (StageHandling, Bool) m ) 
    => FilePath -> CompilationScope -> FilePath -> m ()
compile inFile compScope outDir = do
    (sh, tailRec) <- ask
    -- composition:
    let lang = getIntegration (toText $ takeExtension inFile)

    -- frontend
    (ctxt, ns) <- ohuacCompilation lang compScope inFile
    -- middle end
    compiledAlgos <- 
        mapM 
            (\algo -> 
                (algo,) <$> ohuaCoreCompilation sh tailRec  (algo^.algoCode))
            $ ns^.algos
    -- backend 
    writeFiles =<< backend lang compiledAlgos
    where
        writeFiles = mapM_ 
                        (\(file, code) -> do 
                            let fullPath = outDir <> file
                            liftIO $ L.writeFile fullPath code
                            logInfoN $ "Code written to '" <> T.pack fullPath <> "'"
                        )

