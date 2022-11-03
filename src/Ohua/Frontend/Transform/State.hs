module Ohua.Frontend.Transform.State  where 

import Ohua.Prelude hiding (Nat)
import Ohua.Types.Vector

import Ohua.Frontend.Lang
import Ohua.Frontend.PPrint
import Data.HashMap.Lazy as HM

-- | This essentially checks for linear state usage and is part of
--   the according type system.
type Action = ( Nat  -- reading
              , Nat  -- writing
              )

data Ctxt =
  Ctxt
  (HM.HashMap Binding Action) -- ctxt
  (HM.HashMap Binding Action) -- local ctxt

-- REMINDER: This function was supposed to check programming model compliance.
-- However there was no test case that triggert relevant errors so it is commented out to
-- avoid confusion.

{-

checkLinearUsage :: CompM m => Expr ty -> m ()
checkLinearUsage expr = f (Ctxt HM.empty HM.empty) expr >> return ()
  where
    f ctxt e@(BindE (VarE bnd) b) = do
      (Ctxt ctxt' local') <- f ctxt b
      case HM.lookup bnd local' of
         (Just (Succ _,_)) ->
                        error $ "State variable " <> show (unwrap bnd) <>
                        " used for reading *before* writing in expression:\n" <> prettyExpr e <>
                        "\n In the program:\n" <> prettyExpr expr
         (Just (Zero,y)) -> return $ Ctxt ctxt' $ HM.insert bnd (Zero,Succ y) local'
         Nothing ->
           case HM.lookup bnd ctxt' of
             (Just (Succ _,_)) ->
                            error $ "State variable " <> show (unwrap bnd) <>
                            " used for reading *and* writing in expression:\n" <> prettyExpr e <>
                            "\n In the program:\n" <> prettyExpr expr
             (Just (_,Succ _)) ->
                            error $ "State variable " <> show (unwrap bnd) <>
                            " is used a second time (inside this context) in expression:\n" <> prettyExpr e <>
                            "\n In the program:\n" <> prettyExpr expr
             (Just (Zero,Zero)) -> return $ Ctxt (HM.insert bnd (Zero,Succ Zero) ctxt') local'
             Nothing -> error $ "invariant broken: expression is not well-scoped!\n" <>
                        prettyExpr e <> "\n Unbound var: " <> show bnd <>
                        "\n Expression:\n" <> prettyExpr expr
    f (Ctxt ctxt loc) e@(VarE bnd) =
      case HM.lookup bnd loc of
        -- Note that reading *after* writing is actually ok, because each
        -- state thread creates a new state ref that is used in the continuation.
        (Just (x,y)) -> return $ Ctxt ctxt $ HM.insert bnd (Succ x,y) loc
        Nothing ->
          case HM.lookup bnd ctxt of
            (Just (_,Succ _)) -> error $ "State variable " <> show (unwrap bnd) <>
                            " used for reading *and* writing in expression:\n" <> prettyExpr e <>
                            "\n In the program:\n" <> prettyExpr expr
            (Just (x,Zero)) -> return $ Ctxt (HM.insert bnd (Succ x,Zero) ctxt) loc
            Nothing -> error $ "invariant broken: expression is not well-scoped!\n" <>
                       prettyExpr e <> "\n Unbound var: " <> show bnd <>
                       "\n Expression:\n" <> prettyExpr expr
    f ctxt (AppE a bs) = f ctxt a >>= (\ctxt' -> foldM f ctxt' bs)
    f ctxt (LetE p a b) =
      -- I have to add it here into the checkLinearUsage for a because p may be the identifier
      -- for a recursive function.
       f (addToCtxt ctxt p) a >>= (`f` b)
    f ctxt (LamE ps b) = f (addToCtxt ctxt $ TupP ps) b
    f ctxt (MapE b d) = do
      ctxt' <- f ctxt d
      ctxt'' <- f (liftLocalIntoCtxt ctxt') b
      return ctxt' -- ???
    f ctxt (IfE a b c) = do
      ctxt' <- f ctxt a
      ctxt'' <- f ctxt' b
      ctxt''' <- f ctxt' c
      return ctxt' -- ???
    f ctxt _ = return ctxt

    addToCtxt (Ctxt ctxt loc) p =
      Ctxt ctxt $ foldl (\local' b -> HM.insert b (Zero,Zero) local') loc [bnd | VarP bnd <- universe p]

    liftLocalIntoCtxt (Ctxt ctxt loc) =
      Ctxt
      (foldl (\c b -> HM.insert b (Zero,Zero) c) ctxt $ HM.keys loc)
      HM.empty

-}