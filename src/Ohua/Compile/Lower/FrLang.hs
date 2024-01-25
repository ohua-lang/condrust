module Ohua.Compile.Lower.FrLang where

import Ohua.Prelude

import Control.Category ((>>>))
import qualified Data.HashSet as HS

import Ohua.Frontend.Lang as FR 
import Ohua.Frontend.PPrint ()
import Ohua.Core.ALang.Lang hiding (Expr, ExprF)
import qualified Ohua.Core.ALang.Lang as ALang
import qualified Ohua.Core.InternalFunctions as IFuns
import qualified Data.List.NonEmpty as NE


toAlang :: ErrAndLogM m => FR.FuncExpr ty -> m (ALang.Expr ty)
toAlang expr =  toAlang' (definedBindings expr)  expr

toAlang' :: ErrAndLogM m => HS.HashSet Binding -> FR.FuncExpr ty -> m (ALang.Expr ty)
toAlang' taken expr = runGenBndT taken $ transfrm expr
    where
        transfrm =
            whileToRecursion >=>
            mkLamSingleArgument . unwrapAlgo >>>
            removeDestructuring >=> pure . trans

-- | In the frontend we represent algos as Lamda expressions to capture the input and return types
--   However ALang still expects them to be Let expressions (which is the "second layer" of each algo)
--   so we just unwrap them here until we get consistency among the representations
unwrapAlgo :: FR.FuncExpr ty -> FR.FuncExpr ty
unwrapAlgo (LamE pats innerAlgo) = addAssignments (NE.toList pats) innerAlgo
   where 
    addAssignments ((VarP x xty): xs) body =
        addAssignments xs $ LetE (VarP x xty) (LitE $ EnvRefLit x xty) body
    addAssignments [] body = body
    -- ToDo/Representation: We need to encode in the type system, that it's impossible to have nested patterns at this point 
    addAssignments _nested _body = error "Error: Algo arguments contain tuple pattern. Those should have been resolved already. Please file a bug"
-- This case shouldn't happen, but this is an intermediate solution so we let it error in the code we want to keep/refactor
unwrapAlgo e = e


-- | Whenever the result of a function call is a tuple, add assignemnts for each part of the tuple to have each function call a single
--   output i.e.
--   Let (a, b) = fun() becomes 
--   Let newName = fun()
--       in Let a = nth(newName, 0) 
--       in Let b = nth(newName, 1)
removeDestructuring :: MonadGenBnd m => FR.FuncExpr ty -> m (FR.FuncExpr ty)
removeDestructuring = 
    FR.preWalkM $ \case
        LetE (TupP pats) e1 e2 -> do
            valBnd <- generateBinding
            let tType = (tupTypeFrom pats)
            pure $ LetE (VarP valBnd tType) e1 $ unstructure (valBnd, tType) pats e2
        LamE (TupP pats :| []) e -> do
            valBnd <- generateBinding
            let tType = (tupTypeFrom pats)
            pure $ LamE (VarP valBnd tType :| []) $ unstructure (valBnd, tType) pats e
        e -> pure e


unstructure :: (Binding, ResolvedType ty) -> NonEmpty (FR.ResolvedPat ty) -> FR.FuncExpr ty -> FR.FuncExpr ty
unstructure (valBnd, valTy) pats = go (toInteger $ length pats) (NE.toList pats)
  where
    go numPats =
        foldl (.) id .
        map
            (\(idx, pat) ->
                 LetE pat $
                 AppE
                     (nthFun valTy (FR.patType pat))
                     (LitE (NumericLit idx) :|
                     [ LitE (NumericLit numPats)
                     , VarE valBnd valTy
                     ])) .
        zip [0 ..]

-- ToDo: Replace by Alang definition
nthFun :: ResolvedType ty -> ResolvedType ty -> FR.FuncExpr ty
nthFun collTy elemTy = LitE $ FunRefLit $ FunRef IFuns.nth $ FunType (Right $ IType TypeNat :| [IType TypeNat, collTy]) elemTy

seqFunSf ::  ResolvedType ty -> ResolvedType ty -> FR.FuncExpr ty
seqFunSf t1 t2 = LitE $ FunRefLit $ FunRef IFuns.seqFun $ FunType (Right $ t1 :| [t2]) t2


-- | The function actualy lowering Frontend IR to ALang
trans :: FR.FuncExpr ty -> ALang.Expr ty
trans =
    \case
        VarE b ty -> Var $ TBind b ty
        LitE l -> Lit l
        LetE p e1 e2@LitE{} -> 
            let litReplaced = constLit  e1 e2 
            in trans litReplaced
        LetE p e1 e2 -> Let (patToTBind p) (trans e1) (trans e2)

        AppE e1 args -> foldl Apply (trans e1) (map trans args)
        LamE p e -> case p of
                (p0:|[]) -> Lambda (patToTBind p0) (trans e)
                _ -> error $
                    "Invariant broken: Found multi apply or destructure lambda: " <>
                    show p
        IfE cont then_ else_ ->
            ALang.ifBuiltin
                (ALang.exprType  (trans then_))
                `Apply` (trans cont)
                    `Apply` Lambda (TBind "_" $ IType TypeUnit) (trans then_)
                    `Apply` Lambda (TBind "_" $ IType TypeUnit) (trans else_)
        MapE function coll ->
            let function' = trans function
                coll' = trans coll
            in  case ALang.funType function' of
                Just fTy -> (ALang.smapBuiltin (FType fTy) (ALang.exprType coll') (IType $ TypeList (IType TypeUnit))) `Apply` function' `Apply` coll'
                Nothing -> error $ "Function type is not available for expression:\n "<> show function <> "\n Please report this error."

        StateFunE stateE (MethodRes mName fty) args -> 
            let method' = BindState (trans stateE) (Lit $ FunRefLit (FunRef mName fty) )
            in foldl Apply method' (map trans args)

        StmtE e1 e2@LitE{} -> 
            let litReplaced = constLit e1 e2 
            in trans litReplaced

        StmtE e1 cont -> Let (TBind "_" $ IType TypeUnit) (trans e1) (trans cont)
        TupE exprs -> 
            let alExprs = NE.map trans exprs
                exprTys = NE.map ALang.exprType alExprs
            in foldl 
                    Apply 
                    (pureFunction IFuns.mkTuple (FunType (Right exprTys) (TType exprTys))) 
                    alExprs
                 
                
        WhileE _cond _body ->  error "While loop has not been replaced. Please file a bug"
  where
    patToTBind =
        \case
            VarP v ty -> TBind v ty
            p -> error $
             "Invariant broken: At this point any patterns" <>
             "(function arguments or let bound variables) should be single vars but " <> show p <>
             "is not. Please file a bug."

    constLit:: FR.FuncExpr ty -> FR.FuncExpr ty -> FR.FuncExpr ty
    constLit eS eT =
        let pType = FR.exprType eS
            tType = FR.exprType eT 
        in  LetE (VarP "x_lit" pType) eS $
            AppE (seqFunSf pType tType)  ((VarE "x_lit" pType) :| [eT])


tupTypeFrom :: NonEmpty (ResolvedPat ty) -> (ResolvedType ty)
tupTypeFrom pats = TType $ NE.map getPType pats
    where
        getPType (VarP _b ty) = ty
        -- Actually we could probably support it. Also we should have cought that case before
        -- FIXME correct!
        -- steps:
        -- 1) adapt TType (in Resolved.Types)
        -- 2) adapt TupP (in WellTyped)
        getPType (TupP _ ) = error $ "Encountered a nested tuple pattern, like \"let (a, (b, c)) = ...\". This is currently not supported"


definedBindings :: FR.FuncExpr ty -> HS.HashSet Binding
definedBindings olang =
    HS.fromList $
    [v | VarE v _ty <- FR.flattenR olang] <>
    [v | VarP v _ty <- universePats olang]

-- | Not sure this traversal is necessary, but it turns every smap argument into
-- a lambda
--
-- I am leaving it here in case we need it later.
-- And I commented it out because we need to adapt t typed Var's 
{- _ensureLambdaInSmap :: (Monad m, MonadGenBnd m) => FR.Expr ty 'Resolved -> m (FR.Expr ty 'Resolved)
_ensureLambdaInSmap =
    rewriteM $ \case
        MapE (FR.LamE _ _) _ -> pure Nothing
        MapE other coll -> do
            bnd <- generateBinding
            pure $ Just $ MapE (LamE [VarP bnd ty] $ AppE other [VarE bnd ty]) coll
        _ -> pure Nothing
-}

-- | Ensures every lambda takes at most one argument.
mkLamSingleArgument :: FR.FuncExpr ty -> FR.FuncExpr ty
mkLamSingleArgument =
    FR.preWalkE $ \case
        LamE (x1:|(x2:xs)) b -> LamE (x1 :| []) $ LamE (x2 :| xs) b
        e -> e

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

whileToRecursion :: MonadGenBnd m =>  FR.FuncExpr ty -> m (FR.FuncExpr ty)
whileToRecursion = return 
{-
    rewriteM $ \case
        WhileE cond body -> Just <$> do
            loopName <- generateLoopName body
            let (branchingFunction, stateVars) = generateIfSplit loopName cond body
            -- ToDO: Use actual looptype depending on if we state-thread or not
            let loopType = 'TypeVar'
            let whileLoopFunction = -- (trace $ "WHILE TRANSFORM: "<> show stateVars) 
                                     LamE (map (uncurry VarP) stateVars) $ StmtE body branchingFunction 
            return $ LetE (VarP loopName loopType) whileLoopFunction branchingFunction
        _ -> return Nothing

        where
            generateLoopName :: MonadGenBnd m => FR.Expr ty 'Resolved -> m Binding
            generateLoopName _b = generateBindingWith "while_loop_function"

            generateIfSplit :: Binding -> FR.Expr ty 'Resolved -> FR.Expr ty 'Resolved -> (FR.Expr ty 'Resolved, [(Binding, OhuaType ty 'Resolved)])
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
                        let loopTy = 'TypeVar'
                            loopName = "loop_function"
                        in 
                            LetE (VarP loopName loopTy) returnTuple $
                            IfE cond (AppE (VarE "loop_function" loopTy) (map (uncurry VarE) usedStates)) (VarE "loop_function" loopTy)

                in (recCall, usedStates)
                
            extractUsedStates ::  FR.Expr ty 'Resolved -> [(Binding, OhuaType ty 'Resolved)]
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



