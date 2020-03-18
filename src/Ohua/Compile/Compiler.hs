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
import qualified Ohua.Frontend.NS as NS hiding (Imports)
import Ohua.DFGraph (OutGraph)
-- FIXME the namespaces are broken here! this should be: Ohua.Core.Compile
import qualified Ohua.Compile as OhuaCore (compile)
import qualified Ohua.Compile.Configuration as OhuaCoreConfig (passAfterDFLowering)
import Ohua.Compile.Config
import Ohua.Compile.Transform.Resolve (resolveNS)
import Ohua.Compile.Transform.Load (load)
import Ohua.Compile.CodeGen.Iface (CodeGenData(..), Fun(..))
import Ohua.Unit (cleanUnits)
import Ohua.Serialize.JSON ()

import Ohua.Parser.Common as P

import Ohua.Compile.Types

ohuacCompilation :: CompM m => CompilationScope -> FilePath -> m P.Namespace
ohuacCompilation compScope inFile = resolveNS =<< load compScope inFile

ohuaCoreCompilation :: CompM m => StageHandling -> Bool -> FrLang.Expr -> m OutGraph
ohuaCoreCompilation stageHandlings tailRecSupport expr = do
    expr' <- prepareRootAlgoVars expr
    let expr'' = transformFinalUnit expr'
    -- this transforms a Frontend expression into an ALang expression
    -- FIXME the interface here is broken. either I give an FrLang expression to core and
    --       itself translates it into ALang or FrLang is part of ohuac.
    --       see ohua-core issue #3
    alangExpr <- runGenBndT mempty $ FrLang.toAlang expr''
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

package ::
    ( CompM m
    , MonadReader (StageHandling, Bool, CodeGenSelection) m ) 
    => Maybe NSRef -> Set.HashSet QualifiedBinding -> [(Algo, OutGraph)] -> m L.ByteString
package ns sfImports compiledAlgos = do
    codegen <- selectionToGen . (^._3) <$> ask
    codegen
        CodeGenData
            { namespace = ns
            , sfDependencies = sfImports
            , funs = flip map compiledAlgos (\((Algo n t _), gr) -> 
                Fun 
                    { graph = gr
                    , annotations = t
                    , name = n
                    })
            }

compileModule ::
    ( CompM m
    , MonadReader (StageHandling, Bool, CodeGenSelection) m) 
    => FilePath -> CompilationScope -> m L.ByteString
compileModule inFile compScope = do
    (sh, tailRec, _) <- ask
    ns <- ohuacCompilation compScope inFile
    compiledAlgos <- 
        mapM 
            (\algo -> 
                (algo,) <$> (ohuaCoreCompilation sh tailRec $ algo^.algoCode))
            $ ns^.algos
    package (ns^.nsName) (sfImports ns) compiledAlgos
    where
        sfImports :: P.Namespace -> Set.HashSet QualifiedBinding 
        sfImports ns = 
            let sfs = flip Set.difference (Set.fromList $ HM.keys compScope)
                    $ Set.fromList
                    $ join
                    $ map 
                        (\imp -> 
                            -- TODO this conversion is just painful.
                            --      it feels to me like there are too many types for the same thing:
                            --      QualifiedBinding vs NSRef vs Import vs ...
                            let bnds = unwrap $ imp^.nsRef
                            in map 
                                (\bnd -> makeThrow $ bnds ++ [bnd]) 
                                $ imp^.bindings) 
                        $ ns^.imports 
            in flip Set.map sfs 
                $ \nsReference -> 
                    let l = NE.fromList $ unwrap nsReference
                        newNSRef = makeThrow $ init l
                        bnd = last l
                    in QualifiedBinding newNSRef bnd


compile :: 
    ( CompM m
    , MonadReader (StageHandling, Bool, CodeGenSelection) m ) 
    => FilePath -> CompilationScope -> FilePath -> m ()
compile inFile compScope outFile = do
    packaged <- compileModule inFile compScope
    liftIO $ L.writeFile outFile packaged
    logInfoN $ "Code written to '" <> T.pack outFile <> "'"

