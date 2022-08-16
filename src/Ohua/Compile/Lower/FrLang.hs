module Ohua.Compile.Lower.FrLang where

import Ohua.Prelude

import Control.Category ((>>>))
import Control.Lens.Plated (Plated, cosmos, gplate, plate, universeOn)
import Data.Functor.Foldable (cata)
import qualified Data.HashSet as HS
import GHC.Exts

import Ohua.Frontend.Lang as FR
import Ohua.Frontend.PPrint ()
import Ohua.Core.ALang.Lang hiding (Expr, ExprF)
import qualified Ohua.Core.ALang.Lang as AL
import qualified Ohua.Core.ALang.Refs as ARefs
import Ohua.Core.ParseTools.Refs (ifBuiltin, mkTuple, smapBuiltin, seqBuiltin)


-- | Not sure this traversal is necessary, but it turns every smap argument into
-- a lambda
--
-- I am leaving it here in case we need it later.
_ensureLambdaInSmap :: (Monad m, MonadGenBnd m) => FR.Expr ty -> m (FR.Expr ty)
_ensureLambdaInSmap =
    rewriteM $ \case
        MapE (FR.LamE _ _) _ -> pure Nothing
        MapE other coll -> do
            bnd <- generateBinding
            pure $ Just $ MapE (LamE [VarP bnd] $ AppE other [VarE bnd]) coll
        _ -> pure Nothing

-- | Ensures every lambda takes at most one argument.
mkLamSingleArgument :: FR.Expr ty -> FR.Expr ty
mkLamSingleArgument =
    rewrite $ \case
        LamE (x1:x2:xs) b -> Just $ LamE [x1] $ LamE (x2 : xs) b
        _ -> Nothing

removeDestructuring :: MonadGenBnd m => FR.Expr ty -> m (FR.Expr ty)
removeDestructuring =
    rewriteM $ \case
        LetE (TupP pats) e1 e2 -> do
            valBnd <- generateBinding
            pure $ Just $ LetE (VarP valBnd) e1 $ unstructure valBnd pats e2
        LamE [TupP pats] e -> do
            valBnd <- generateBinding
            pure $ Just $ LamE [VarP valBnd] $ unstructure valBnd pats e
        _ -> pure Nothing

giveEmptyLambdaUnitArgument :: FR.Expr ty -> FR.Expr ty
giveEmptyLambdaUnitArgument =
    rewrite $ \case
        LamE [] e -> Just $ LamE [UnitP] e
        _ -> Nothing

-- | The idea here is, that we transform a while loop to a recursive call
--   As the inner part of the while loop is a local scope and there is no 'return' 
--   for local variables, the arguments as well as the return of the recursive call are 
--   (stateful) objects from the outer context, that are manipulated/used in the loop.
--   Assuming we had assignments in the supported subset, we'd also need to scan for variables that
--   are assigned in the scope e.g.
{- fun(...){
    ...
    while state1.next(){
        let e = state1.get();
        state.do_stuff(e)
    }
   }

   should become:

    fun(...){
    ...
    let (state1, state2) = recursive_while(state1, state2) 
   }
-} 
-- QUESTION: Do we need checks for validity here that arrise purely from the conversion? 
--           We do not care (at all) for invalid Code of the input language, and should treat 
--           more general problems that affect also recursions downstream.

whileToRecursion :: MonadGenBnd m =>  FR.Expr ty -> m (FR.Expr ty)
whileToRecursion = 
    rewriteM $ \case
        WhileE cond body -> Just <$> do
            loopName <- generateLoopName body
            let (branchingFunction, stateVars) = generateIfSplit loopName cond body
            let whileLoopFunction = (trace $ "WHILE TRANSFORM: "<> show stateVars) 
                                     LamE (map VarP stateVars) $ StmtE body branchingFunction 
            return $ LetE (VarP loopName) whileLoopFunction branchingFunction
        _ -> return Nothing

        where
            generateLoopName :: MonadGenBnd m => FR.Expr ty -> m Binding
            generateLoopName _b = generateBindingWith "while_loop_function"

            generateIfSplit :: Binding -> FR.Expr ty -> FR.Expr ty -> (FR.Expr ty, [Binding])
            generateIfSplit loopName cond body = 
                -- We need to pass statefull variables from one recursion to the next.
                -- This may include a stateful varaible used in the condition e.g. while iter.has_next()
                let usedStates = extractUsedStates body
                    returnTuple = 
                        -- ToDo: Tuple kommt nicht mit nur einem Element klar -> 1. adapt with check -> Fix later
                        TupE (map VarE usedStates)
                    recCall = 
                        LetE "bla" returnTuple $
                        IfE cond (AppE (VarE loopName) (map VarE usedStates)) (VarE "bla")
                    -- recCall = IfE cond (AppE (VarE loopName) usedStates) usedStates
                in (recCall, usedStates)
                
            extractUsedStates ::  FR.Expr ty -> [Binding]
            -- ToDo: 1. BindE expr expr might 'bind' other expression as well. For now we want to error in that case 
            extractUsedStates body = map 
                (\e -> case e of 
                    VarE bnd -> bnd
                    expr -> error $ "We currently only support stateful functions directly called on objects. Please rewrite for " <> show expr) 
                [expr | BindE expr _funE <- universe body]


 {- Original While Transformation in the frontend. I suspect I can/should replace the empty
    args with the actual states beeing processed in the loop.

    let loopLambdaRef = "while_loop_body"
    let recur =
          IfE
            cond'
            (AppE (VarE loopLambdaRef)  [])
            $ LitE UnitLit
    return $
      -- FIXME proper name generation needed here!
      LetE
        (VarP loopLambdaRef)
        (LamE [] $ StmtE block' recur)
        recur
    -}


nthFun :: FR.Expr ty
nthFun = LitE $ FunRefLit $ FunRef ARefs.nth Nothing $ FunType $ Right $ TypeVar :| [TypeVar,TypeVar]

unstructure :: Binding -> [FR.Pat] -> FR.Expr ty -> FR.Expr ty
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

trans :: FR.Expr ty -> AL.Expr ty
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
        SeqEF source target -> seqBuiltin `Apply` source `Apply` target
        -- QUESTION: Where should this come from.
        -- ISSUE: TypeVar may cause problems for Backends where 'type variables' are just not an option
        TupEF parts -> foldl Apply (pureFunction mkTuple Nothing $ FunType $ Right $ TypeVar :| [TypeVar]) parts
        WhileEF cond _body ->  error "While loop has not been replaced. Please file a bug"
  where
    patToBnd =
        \case
            VarP v -> v
            UnitP -> "_"
            p -> error $ 
             "Compiler screwed up. At this point any patterns" <>
             "(function arguments or let bound variables) should be single vars or wild card but " <> show p <>
             "is not. Please file a bug."

toAlang' :: CompM m => HS.HashSet Binding -> Expr ty -> m (AL.Expr ty)
toAlang' taken expr = runGenBndT taken $ transform expr
    where 
        transform =
            whileToRecursion >=>
            giveEmptyLambdaUnitArgument >>>
            mkLamSingleArgument >>> 
            removeDestructuring >=> pure . trans

toAlang :: CompM m => FR.Expr ty -> m (AL.Expr ty)
toAlang expr = (trace $ "Converting to Alang, Bindings are" <> show (definedBindings expr)) toAlang' (definedBindings expr) expr

definedBindings :: FR.Expr ty -> HS.HashSet Binding
definedBindings olang =
    HS.fromList $
    [v | VarE v <- universe olang] <>
    [v | VarP v <- universeOn (cosmos . patterns) olang]
