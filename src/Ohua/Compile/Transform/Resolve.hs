{-|

Module      : $Header$
Description : Transformation for splicing in algos for algo references.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

This module is a transformation that turns a function reference into an expression.
The resulting expression does not contain any (function reference) anymore.
Function references in this transformation are just references to variable that refer to
a function. That is, this transformation does not deal with namespaces or such.

-}

module Ohua.Compile.Transform.Resolve where


-- TODO: It feels to me like this function needs a rewrite. I'm quite sure there was no testing at
--       all to make sure it works for algorithms from other namespaces.
--       More urgently, we need to define what a namespace reference that a parser returns should look like.
--       For example in Go:
--       import some/other/ns
--                ^
--                |
--                 - What is this?
--       ns.foo()
--        ^
--        |
--         - Or this?
--
--       Or in Rust:
--       use some::other::ns{foo};
--      
--       foo(); <-- This is not a local algo!
--
-- For this transformation, the expression only has the following form:
-- > some/other/ns:foo()
--   |               |
--    ---------------
--           |
--            -> This is a function literal.
-- The algo is registered with the key 
-- > some/other/ns:foo
resolveNS ::
       forall m. (MonadError Error m)
    => IFaceDefs
    -> ResolvedNamespace
    -> m ResolvedNamespace
resolveNS ifacem ns = do
    resDecls <-
        flip runReaderT (mempty, ifacem) $
        go0
            (topSortDecls (`Set.member` locallyDefinedAlgos) $
             HM.toList (ns ^. decls))
    pure $ ns & decls .~ HM.fromList resDecls
  where
    go0 [] = pure []
    go0 ((varName, expr):xs) = do
        done <- go varName expr
        local (second $ HM.insert (QualifiedBinding (ns ^. name) varName) done) $
            ((varName, done) :) <$> (go0 xs)
    go :: Binding -> Expr -> ReaderT (Set.HashSet Binding, IFaceDefs) m Expression
    go self = cata (handleTerm self)
    handleTerm self e =
        case e of
            -- FIXME Is this right at all?
            --       This is a catamorphism, so I believe it moves bottom up.
            --       Collecting bindings bottom up and then using them to check seems totally wrong to me.
            --       For the new parser, I believe this whole stuff is not necessary anymore.
            --       
            LetF bnd _ _ -> registered bnd
            VarF bnd
                | Just alangVersion <- primitives ^? ix bnd -> pure alangVersion
                | otherwise -> do
                    isLocal <- asks (Set.member bnd . fst)
                    let isSelf = bnd == self
                    -- This tries to understand whether a reference inside the expression is essentially
                    -- a reference to an algo or to a function.
                    case (HM.lookup bnd algoRefers, HM.lookup bnd sfRefers) of
                        _
                            | isLocal || isSelf -> pure $ Var bnd
                        (Just algo, Just sf) ->
                            throwError $
                            "Ambiguous ocurrence of unqualified binding " <>
                            show bnd <>
                            ". Could refer to algo " <>
                            show algo <>
                            " or sf " <>
                            show sf
                        -- Here, the loaded algorithm gets hooked in! This is all the magic of this whole function.
                        -- It simply turns this variable definition into a let that looks like so:
                        -- `let bnd = loadedAlgo in bnd`.
                        -- It allows you to use this algo somewhere in the code wherever you like.
                        -- In fact, there is probably a transformation in core that turns this into just `loadedAlgo`.
                        -- Why no immediately do so?
                        (Just algoname, _) ->
                            flip (Let bnd) (Var bnd) <$>
                            (maybe
                                 (throwError $
                                  "Algo not loaded " <> show algoname)
                                 pure =<<
                             asks (HM.lookup algoname . snd))
                        -- If it is a (stateful) function then it turns this var into a literal with a function reference.
                        -- This assumes that the parser interpreted a reference as a variable rather than a function reference.
                        -- Can this still happen in the new parser design?
                        (_, Just sf) ->
                            pure $ Lit $ FunRefLit $ FunRef sf Nothing
                        _ -> throwError $ "Binding not in scope " <> show bnd
            -- This branch is just error reporting.
            LitF (FunRefLit (FunRef qb _)) -> do
                algo <- asks (HM.lookup qb . snd)
                case algo of
                    Just anAlgo -> pure anAlgo
                    _
                        | isImported qb ->
                            pure $ Lit $ FunRefLit $ FunRef qb Nothing
                        | otherwise ->
                            throwError $
                            "No matching algo or stateful function available or namespace not loaded for " <>
                            show qb
            LambdaF bnd _ -> registered bnd
            _ -> recursed
      where
        recursed = embed <$> sequence e
        registered bnd = registerBnd bnd recursed
    isImported qb =
        (qb ^. namespace) `Set.member` Set.fromList (map fst sfImports1)
    sfImports1 = stdNamespace : ns ^. sfImports
    registerBnd = local . first . Set.insert
    locallyDefinedAlgos = Set.fromList $ HM.keys (ns ^. decls)
    algoRefers =
        mkReferMap $ (ns ^. name, HM.keys (ns ^. decls)) : ns ^. algoImports
    sfRefers = mkReferMap sfImports1
    mkReferMap importList =
        HM.fromListWith
            reportCollidingRef
            [ (shortname, QualifiedBinding sourceNS shortname)
            | (sourceNS, referList) <- importList
            , shortname <- referList
            ]
    reportCollidingRef a b =
        error $
        "Colliding refer for '" <> show a <> "' and '" <> show b <> "' in " <>
        show (ns ^. name)
