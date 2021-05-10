module Ohua.Compile.Lower.FrLang where

import Ohua.Prelude

import Control.Category ((>>>))
import Control.Lens.Plated (Plated, cosmos, gplate, plate, universeOn)
import Data.Functor.Foldable (cata)
import qualified Data.HashSet as HS
import GHC.Exts

import Ohua.Frontend.Lang
import Ohua.Frontend.PPrint ()
import Ohua.Core.ALang.Lang hiding (Expr, ExprF)
import qualified Ohua.Core.ALang.Lang as AL
import qualified Ohua.Core.ALang.Refs as ARefs
import Ohua.Core.ParseTools.Refs (ifBuiltin, mkTuple, smapBuiltin, seqBuiltin)


-- | Not sure this traversal is necessary, but it makes every smap argument into
-- a lambda
--
-- I am leaving it here in case we need it later.
_ensureLambdaInSmap :: (Monad m, MonadGenBnd m) => Expr ty -> m (Expr ty)
_ensureLambdaInSmap =
    rewriteM $ \case
        MapE (LamE _ _) _ -> pure Nothing
        MapE other coll -> do
            bnd <- generateBinding
            pure $ Just $ MapE (LamE [VarP bnd] $ AppE other [VarE bnd]) coll
        _ -> pure Nothing

-- | Ensures every lambda takes at most one argument.
mkLamSingleArgument :: Expr ty -> Expr ty
mkLamSingleArgument =
    rewrite $ \case
        LamE (x1:x2:xs) b -> Just $ LamE [x1] $ LamE (x2 : xs) b
        _ -> Nothing

removeDestructuring :: MonadGenBnd m => Expr ty -> m (Expr ty)
removeDestructuring =
    rewriteM $ \case
        LetE (TupP pats) e1 e2 -> do
            valBnd <- generateBinding
            pure $ Just $ LetE (VarP valBnd) e1 $ unstructure valBnd pats e2
        LamE [TupP pats] e -> do
            valBnd <- generateBinding
            pure $ Just $ LamE [VarP valBnd] $ unstructure valBnd pats e
        _ -> pure Nothing

giveEmptyLambdaUnitArgument :: Expr ty -> Expr ty
giveEmptyLambdaUnitArgument =
    rewrite $ \case
        LamE [] e -> Just $ LamE [UnitP] e
        _ -> Nothing

nthFun :: Expr ty
nthFun = LitE $ FunRefLit $ FunRef ARefs.nth Nothing $ FunType [TypeVar,TypeVar,TypeVar]

unstructure :: Binding -> [Pat] -> Expr ty -> Expr ty
unstructure valBnd pats = go (toInteger $ length pats) pats
  where
    go numPats =
        foldl (.) id .
        map
            (\(idx, pat) ->
                 LetE pat $
                 AppE
                     nthFun
                     [ LitE (NumericLit idx)
                     , LitE (NumericLit numPats)
                     , VarE valBnd
                     ]) .
        zip [0 ..]

trans :: Expr ty -> AL.Expr ty
trans =
    cata $ \case
        VarEF b -> Var b
        LitEF l -> Lit l
        LetEF p e1 e2 -> Let (patToBnd p) e1 e2
        AppEF e1 e2
            | null e2 -> e1 `Apply` Lit UnitLit
            | otherwise -> foldl Apply e1 e2
        LamEF p e ->
            case p of
                [] -> e
                [p0] -> Lambda (patToBnd p0) e
                _ ->
                    error $
                    "Invariant broken: Found multi apply or destructure lambda: " <>
                    show p
        IfEF cont then_ else_ ->
            ifBuiltin `Apply` cont `Apply` Lambda "_" then_ `Apply`
            Lambda "_" else_
        MapEF function coll -> smapBuiltin `Apply` function `Apply` coll
        BindEF ref e -> BindState ref e
        StmtEF e1 cont -> Let "_" e1 cont
        SeqEF source target -> seqBuiltin `Apply` source `Apply` Lambda "_" target
        TupEF parts -> foldl Apply (pureFunction mkTuple Nothing $ FunType [TypeVar,TypeVar]) parts
  where
    patToBnd =
        \case
            VarP v -> v
            UnitP -> "_"
            p -> error $ "Invariant broken, invalid pattern: " <> show p

toAlang' :: CompM m => HS.HashSet Binding -> Expr ty -> m (AL.Expr ty)
toAlang' taken expr = runGenBndT taken $ transform expr
    where 
        transform =
            giveEmptyLambdaUnitArgument >>>
            mkLamSingleArgument >>> 
            removeDestructuring >=> pure . trans

toAlang :: CompM m => Expr ty -> m (AL.Expr ty)
toAlang expr = toAlang' (definedBindings expr) expr

definedBindings :: Expr ty -> HS.HashSet Binding
definedBindings olang =
    HS.fromList $
    [v | VarE v <- universe olang] <>
    [v | VarP v <- universeOn (cosmos . patterns) olang]
