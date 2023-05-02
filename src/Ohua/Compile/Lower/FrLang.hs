module Ohua.Compile.Lower.FrLang where

import Ohua.Prelude
import GHC.Exts

import Control.Category ((>>>))
import Control.Lens.Plated (Plated, cosmos, gplate, plate, universeOn)
import Data.Functor.Foldable (cata)
import qualified Data.HashSet as HS
import GHC.Exts

import Ohua.Frontend.Lang as FR
import Ohua.Frontend.PPrint ()
import Ohua.Core.ALang.Lang hiding (Expr, ExprF)
import qualified Ohua.Core.ALang.Lang as ALang
import qualified Ohua.Core.ALang.Refs as ARefs
import Ohua.Core.ParseTools.Refs (ifBuiltin, mkTuple, smapBuiltin, seqBuiltin)
import qualified Data.List.NonEmpty as NE


-- | Not sure this traversal is necessary, but it turns every smap argument into
-- a lambda
--
-- I am leaving it here in case we need it later.
-- And I commented it out because we need to adapt t typed Var's 
{- _ensureLambdaInSmap :: (Monad m, MonadGenBnd m) => FR.Expr ty -> m (FR.Expr ty)
_ensureLambdaInSmap =
    rewriteM $ \case
        MapE (FR.LamE _ _) _ -> pure Nothing
        MapE other coll -> do
            bnd <- generateBinding
            pure $ Just $ MapE (LamE [VarP bnd ty] $ AppE other [VarE bnd ty]) coll
        _ -> pure Nothing
-}

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
            let tupleTy = (tupTypeFrom pats)
            pure $ Just $ LetE (VarP valBnd tupleTy) e1 $ unstructure (valBnd, tupleTy) pats e2
        LamE [TupP pats] e -> do
            valBnd <- generateBinding
            let tupleTy = (tupTypeFrom pats)
            pure $ Just $ LamE [VarP valBnd tupleTy] $ unstructure (valBnd, tupleTy) pats e
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
whileToRecursion = return 
{-
    rewriteM $ \case
        WhileE cond body -> Just <$> do
            loopName <- generateLoopName body
            let (branchingFunction, stateVars) = generateIfSplit loopName cond body
            -- ToDO: Use actual looptype depending on if we state-thread or not
            let loopType = TypeVar
            let whileLoopFunction = -- (trace $ "WHILE TRANSFORM: "<> show stateVars) 
                                     LamE (map (uncurry VarP) stateVars) $ StmtE body branchingFunction 
            return $ LetE (VarP loopName loopType) whileLoopFunction branchingFunction
        _ -> return Nothing

        where
            generateLoopName :: MonadGenBnd m => FR.Expr ty -> m Binding
            generateLoopName _b = generateBindingWith "while_loop_function"

            generateIfSplit :: Binding -> FR.Expr ty -> FR.Expr ty -> (FR.Expr ty, [(Binding, ArgType ty)])
            generateIfSplit loopName cond body = 
                -- We need to pass statefull variables from one recursion to the next.
                -- This may include a stateful variable used in the condition e.g. while iter.has_next()
                let usedStates = extractUsedStates body
                    returnTuple = 
                        -- FIXME: I have to make the case distinction below, because TuPE fails with less than 2 arguments
                        case usedStates of 
                            [] -> error $ "There seems to be no state change in "<> show body <> ". This loop may be pointless."
                            [(bnd, ty)] -> VarE bnd ty
                            sVars -> fromList (map (uncurry VarE) sVars)
                    
                    recCall = 
                        -- ToDo: Fix typing, loops have either a function type if we thread states, or type () -> () if we just
                        --       enclose the environment implicitely  
                        let loopTy = TypeVar
                            loopName = "loop_function"
                        in 
                            LetE (VarP loopName loopTy) returnTuple $
                            IfE cond (AppE (VarE "loop_function" loopTy) (map (uncurry VarE) usedStates)) (VarE "loop_function" loopTy)

                in (recCall, usedStates)
                
            extractUsedStates ::  FR.Expr ty -> [(Binding, ArgType ty)]
            -- ToDo: 1. BindE expr expr might 'bind' other expression as well. For now we want to error in that case 
            extractUsedStates body = map 
                (\e -> case e of 
                    VarE bnd ty -> (bnd, ty)
                    expr -> error $ "We currently only support stateful functions directly called on objects. Please rewrite for " <> show expr) 
                [expr | BindE expr _funE <- universe body]
-}

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

unstructure :: (Binding, ArgType ty) -> NonEmpty (FR.Pat ty) -> FR.Expr ty -> FR.Expr ty
unstructure (valBnd, valTy) pats = go (toInteger $ length pats) (NE.toList pats)
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
                     , VarE valBnd valTy
                     ]) .
        zip [0 ..]

trans :: FR.Expr ty -> ALang.Expr ty
trans =
    cata $ \case
        VarEF b ty -> Var $ TBind b ty
        LitEF l -> Lit l
        LetEF p e1 e2 -> Let (patToTBind p) e1 e2
        AppEF e1 e2
            | null e2 -> e1 `Apply` Lit UnitLit
            | otherwise -> foldl Apply e1 e2
        LamEF p e ->
            case p of
                [] -> e
                [p0] -> Lambda (patToTBind p0) e
                _ ->
                    error $
                    "Invariant broken: Found multi apply or destructure lambda: " <>
                    show p
        IfEF cont then_ else_ ->
            ifBuiltin `Apply` cont `Apply` Lambda "_" then_ `Apply`
            Lambda "_" else_
        MapEF function coll -> smapBuiltin `Apply` function `Apply` coll
        BindEF ref e -> BindState ref e
        StmtEF e1 cont -> Let (TBind "_" TypeVar) e1 cont
        SeqEF source target -> seqBuiltin `Apply` source `Apply` target
        TupEF fty parts -> foldl Apply (pureFunction mkTuple Nothing fty) parts
        WhileEF cond _body ->  error "While loop has not been replaced. Please file a bug"
  where
    patToTBind =
        \case
            VarP v ty -> TBind v ty
            -- Reminder: The underscore is used to represent things that will be
            -- ignored later i.e. not send, meaning we will not need to type channels for it
            -- FIXME: This shouldn't be stringly typed
            UnitP -> TBind "_" TypeVar
            p -> error $ 
             "Compiler screwed up. At this point any patterns" <>
             "(function arguments or let bound variables) should be single vars or wild card but " <> show p <>
             "is not. Please file a bug."

toAlang' :: CompM m => HS.HashSet Binding -> Expr ty -> m (ALang.Expr ty)
toAlang' taken expr = runGenBndT taken $ transform expr
    where 
        transform =
            whileToRecursion >=>
            giveEmptyLambdaUnitArgument >>>
            mkLamSingleArgument >>> 
            removeDestructuring >=> pure . trans

toAlang :: CompM m => FR.Expr ty -> m (ALang.Expr ty)
toAlang expr =  toAlang' (definedBindings expr) expr

definedBindings :: FR.Expr ty -> HS.HashSet Binding
definedBindings olang =
    HS.fromList $
    [v | VarE v _ty <- universe olang] <>
    [v | VarP v _ty <- universeOn (cosmos . patterns) olang]

tupTypeFrom:: NonEmpty (Pat ty) -> (ArgType ty)
tupTypeFrom pats = TupleTy $ NE.map getPType pats 
    where 
        getPType (VarP _b ty) = ty
        -- ToDO: Or would we add a unit type here? 
        getPType (UnitP) = error $ "Encountered a unit inside a tuple pattern, like \"let (a, (), b) = ...\". Please file a bug"
        -- Actually we could probably support it. Also we should have cought that case before
        getPType (TupP _ ) = error $ "Encountered a nested tuple pattern, like \"let (a, (b, c)) = ...\". This is currently not supported"