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
import qualified Data.HashSet as Set
import qualified Data.Text as T
import System.Directory
import System.FilePath as Path ((<.>), takeExtension)
import Control.Lens (each, view, (%~), (^?), ix)

import Ohua.ALang.Lang as ALang
import qualified Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.NS as NS hiding (Imports)
import Ohua.DFGraph (OutGraph)
import Ohua.Compile.Config
import Ohua.Unit
import Ohua.ALang.PPrint
import Ohua.Serialize.JSON ()

import qualified Ohua.Compat.Go.Parser(parseGo)

import Ohua.Parser.Common as P

import Ohua.Compile.Types

ohuacCompilation ::  
    ( MonadError Error m
    , MonadLoggerIO m)
    => CompilationScope -> FrLang.Expr -> m FrLang.Expr
ohuacCompilation = resolveNS =<< load

ohuaCoreCompilation :: 
    ( MonadError Error m
    , MonadLoggerIO m) 
    => StageHandling -> Bool -> FrLang.Expr -> m OutGraph
ohuaCoreCompilation stageHandlings tailRecSupport = do
    -- this transforms a Frontend expression into an ALang expression
    -- FIXME the interface here is broken. either I give an FrLang expression to core and
    --       itself translates it into ALang or FrLang is part of ohuac.
    --       the first version is definitely the better choice.
    alangExpr <- runGenBndT mempty ((decls . traverse) Fr.toAlang expr)
    OhuaCore.compile
        (def 
            & stageHandling .~ stageHandlings
            & transformRecursiveFunctions .~ tailRecSupport)
        (def {passAfterDFLowering = cleanUnits})
        alangExpr

compileExpr :: 
    ( MonadError Error m
    , MonadLoggerIO m
    , MonadReader (StageHandling, Bool, CodeGenSelection) m ) 
    => CompilationScope -> FrLang.Expr -> m OutGraph
compileExpr compScope expr = do
    (sh, tailRec, _) <- ask
    ohuaCoreCompilation sh tailRec =<< ohuacCompilation expr

package ::
    ( MonadError Error m
    , MonadLoggerIO m
    , MonadReader (StageHandling, Bool, CodeGenSelection) m ) 
    => NSRef -> Imports -> [(Algo, OutGraph)] -> m L.ByteString
package nsName sfImports algos = do
    codegen <- selectionToGen . (^._3) <$> ask
    codegen
        CodeGenData
            { namespace = nsName
            , sfDependencies = Set.fromList sfImports
            , funs = flip map algos $\(algo, gr) -> 
                Fun 
                    { graph = gr
                    , annotations = algo ^. algoTyAnn
                    , name = algo ^. algoName
                    } 
            }

compile :: 
    ( MonadError Error m
    , MonadLoggerIO m
    , MonadReader (StageHandling, Bool, CodeGenSelection) m ) 
    => FileRef -> CompilationScope -> FileRef -> m ()
compile inFile compScope outFile = do
    ns <- readAndParse inFile
    compiledAlgos <-
        mapM 
            (\algo -> (algo, compileExpr compScope $ algo ^. algoCode)) 
            ns ^. algos 
    let sfImports = 
            flip Set.difference compScope
            $ Set.fromList
            $ join
            $ map (\imp -> map (QualifiedBinding imp^.nsRef) imp^.bindings) ns^.imports 
    packaged <- package ns^.name sfImports compiledAlgos
    L.writeFile packaged
    logInfoN $ "Code written to '" <> outFile <> "'"

