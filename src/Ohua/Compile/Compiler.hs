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

import Ohua.ALang.Lang
import qualified Ohua.Frontend.Lang as Fr
import Ohua.Frontend.NS as NS hiding (Imports)
import Ohua.Unit
import Ohua.ALang.PPrint
import Ohua.Serialize.JSON ()

import qualified Ohua.Compat.Go.Parser(parseGo)

import Ohua.Parser.Common as P

import Ohua.Compile.Types

-- | Converts a parse representation of a namespace and resolves it,
--   i.e., it ... TODO
loadDepsAndResolve ::
       CompM m => NSTracker -> Namespace -> m ResolvedNamespace
loadDepsAndResolve tracker rawMod =
    join $
    -- FIXME the below needs to be adapted to work on the new namespace data structure.
    --       We need a step here that differentiates sf imports from algo imports!
    --       Once we have that, we can create a core-representation of the namespace.
    resolveNS <$> gatherDeps tracker (map fst $ rawMod ^. algoImports) <*>
    -- this transforms a Frontend expression into an ALang expression
    runGenBndT mempty ((decls . traverse) Fr.toAlang rawMod)

-- registerAndLoad :: CompM m => ModTracker -> NSRef -> m ResolvedNamespace
-- registerAndLoad tracker reference =
--     registerAnd tracker reference (loadModule tracker reference)

-- FIXME What is this MVar stuff in the below code? Why is this necessary at all?!
--       Seems like, he wanted to get around another state monad.
-- insertDirectly ::
--        MonadIO m
--     => ModTracker
--     -> ResolvedNamespace
--     -> m (MVar ResolvedNamespace)
-- insertDirectly tracker ns = do
--     nsRef <- newMVar ns
--     modifyIORef' tracker $ HM.insert (ns ^. name) nsRef
--     pure nsRef

-- registerAnd ::
--        (Eq ref, Hashable ref, MonadIO m)
--     => IORef (HashMap ref (MVar load))
--     -> ref
--     -> m load
--     -> m load
-- registerAnd tracker reference ac = do
--     newModRef <- liftIO newEmptyMVar
--     actualRef <-
--         liftIO $
--         atomicModifyIORef' tracker $ \trackerMap ->
--             case trackerMap ^? ix reference of
--                 Just mvar -> (trackerMap, Left mvar)
--                 Nothing ->
--                     (HM.insert reference newModRef trackerMap, Right newModRef)
--     case actualRef of
--         Left toWait -> liftIO $ readMVar toWait
--         Right build -> do
--             compiled <- ac
--             liftIO $ putMVar build compiled
--             pure compiled


-- gatherSFDeps :: Expression -> Set.HashSet QualifiedBinding
-- gatherSFDeps e = Set.fromList [ref | Lit (FunRefLit (FunRef ref _)) <- universe e]
-- gatherSFDeps = cata $ \case
--   VarF (Sf ref _) -> Set.singleton ref
--   other -> fold other

-- FIXME I have no clue what all this sorting is for.
-- topSortMods :: [Namespace a] -> [Namespace a]
-- topSortMods = topSortWith (^. name) (map fst . view algoImports)

-- topSortWith :: (Hashable b, Eq b) => (a -> b) -> (a -> [b]) -> [a] -> [a]
-- topSortWith getIdent getDeps mods' =
--     concat @[] $ ana (uncurry go) (mempty, mods')
--   where
--     go satisfied avail
--         | null newSat =
--             if null newAvail
--                 then Nil
--                 else error "Unsortable! (Probably due to a cycle)"
--         | otherwise =
--             Cons newSat $
--             (Set.union (Set.fromList (map getIdent newSat)) satisfied, newAvail)
--       where
--         (newSat, newAvail) =
--             partition (all (`Set.member` satisfied) . getDeps) avail


-- topSortDecls :: (Binding -> Bool) -> [(Binding, Expr)] -> [(Binding, Expr)]
-- topSortDecls f declarations = map fst $ topSortWith (fst . fst) snd declsWDeps
--   where
--     localAlgos = Set.fromList $ map fst declarations
--     getDeps (item, e) =
--         Set.toList $
--         Set.delete item $
--         snd $ evalRWS (go e) mempty ()
--       where
--         go =
--             cata $ \e ->
--                 let recursed = sequence_ e
--                     registered bnd = local (Set.insert bnd) recursed
--                  in case e of
--                         LetF bnd val body -> registered bnd
--                         VarF bnd
--                             | f bnd ->
--                                 unlessM (asks $ Set.member bnd) $
--                                 when (bnd `Set.member` localAlgos) $ tell [bnd]
--                         LambdaF bnd body -> registered bnd
--                         _ -> recursed
--     declsWDeps = zip declarations $ map getDeps declarations

-- FIXME This seems to try to understand something about environment refs. 
--       But these should really not be present at all anymore!
-- mainToEnv :: Expression -> (Int, Expression)
-- mainToEnv = go 0 identity
--   where
--     go !i f (Lambda assignment body) =
--         go (i + 1) (f . Let assignment (Lit $ EnvRefLit $ makeThrow i)) body
--     go !i f rest = (i, f rest)

-- FIXME I do not understand what this is for. Seems to be something very
--       Rust specific. If so then it belongs into the parser.
-- typeFormatterHelper :: Text -> TyExpr SomeBinding -> TyExpr SomeBinding -> Text
-- typeFormatterHelper moduleSeparator tupleConstructor = go []
--   where
--     formatRef sb = T.intercalate moduleSeparator $ map unwrap bnds
--       where
--         bnds =
--             case sb of
--                 Unqual b -> [b]
--                 Qual (QualifiedBinding ns bnd) -> unwrap ns ++ [bnd]
--     go l e
--         | e == tupleConstructor = "(" <> arglist <> ")"
--         | otherwise =
--             case e of
--                 TyRef ref ->
--                     formatRef ref <>
--                     if null l
--                         then ""
--                         else "<" <> arglist <> ">"
--                 TyApp t1 t2 -> go (go [] t2 : l) t1
--       where
--         arglist = T.intercalate "," l

-- #if WITH_CLIKE_PARSER
-- formatRustType :: TyExpr SomeBinding -> Text
-- formatRustType = typeFormatterHelper "::" CTy.tupleConstructor
-- #endif
-- type LangFormatter = TyExpr SomeBinding -> Text

-- langs :: [(Text, LangFormatter)]
-- langs =
-- -- #if WITH_CLIKE_PARSER
-- --   ("rust", formatRustType) :
-- -- #endif
--   []

ohuacCompilation :: Expression -> Expression
ohuacCompilation expr =
    -- What is the problem in the below code??? Why is this case statement needed?
    -- Seems like a trivial transformation.
    let expr =
            case expr'
                    -- FIXME this is technically not correct for edge cases
                    of
                Lambda "_" body -> body
                e -> e
        sfDeps = gatherSFDeps expr -- FIXME probably not needed anymore
        (mainArity, completeExpr) = mainToEnv expr in 
    completeExpr

ohuaCoreCompilation :: 
    ( MonadError Error m
    , MonadLoggerIO m
    , MonadReader (StageHandling, Bool) m ) 
    => StageHandling -> Bool -> Expression -> m OutGraph
ohuaCoreCompilation stageHandlings tailRecSupport =
    OhuaCore.compile
        (def 
            & stageHandling .~ stageHandlings
            & transformRecursiveFunctions .~ tailRecSupport)
        (def {passAfterDFLowering = cleanUnits})

compileExpr :: 
    ( MonadError Error m
    , MonadLoggerIO m
    , MonadReader (StageHandling, Bool) m ) 
    => Imports -> Expression -> m OutGraph
compileExpr sh tailRec expr = 
    loadDepsAndResolve 
    ohuaCoreCompilation sh tailRec =<< ohuacCompilation expr

package ::
    ( MonadError Error m
    , MonadLoggerIO m
    , MonadReader (StageHandling, Bool, CodeGenSelection) m ) 
    => NSRef -> Imports -> [(Algo, OutGraph)] -> m L.ByteString
package nsName sfImports algos = do
    codegen <- selectionToGen <$> _3 ^. ask
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
    => FileRef -> FileRef -> m ()
compile inFile outFile = do
    ns <- readAndParse inFile
    algoImports = filter () -- TODO
    compiledAlgos <-
        mapM 
            (\algo -> (algo, compileExpr algoImports $ algo ^. algoCode)) 
            ns ^. algos 
    packaged <- package compiledAlgos
    L.writeFile packaged
    logInfoN $ "Code written to '" <> outFile <> "'"

