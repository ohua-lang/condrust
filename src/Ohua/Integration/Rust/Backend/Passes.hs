module Ohua.Integration.Rust.Backend.Passes where

import Ohua.Prelude
import Ohua.Integration.Rust.Backend.Subset

import qualified Data.HashSet as HS

-- | Rust wants all __mutable__ variables to be tagged.
--   This transformation finds them and does so.
propagateMut :: Block -> Block
propagateMut block =
  evalState (goBlock block) HS.empty
  where
    go e@(MethodCall (Var bnd) _ _) = do
      modify $ HS.insert bnd
      return e
    go e@(Assign (Var bnd) _) = do
      modify $ HS.insert bnd
      return e
    go (BlockExpr block) = BlockExpr <$> goBlock block
    go (If e0 block e1) = do
      block' <- goBlock block
      return $ If e0 block' e1
    go (Loop block) = Loop <$> goBlock block
    go (ForLoop p r block) = ForLoop p r <$> goBlock block
    go (While e block) = While e <$> goBlock block
    go e = pure e

    goBlock (Block stmts) =
      let (stmts', states) =
            runState (reverse <$> mapM goStmt (reverse stmts)) HS.empty
      in do
        modify $ HS.union states
        return $ Block stmts'

    goStmt (Local p e) = do
      p' <- transformM goPat p
      return $ Local p' e
    goStmt (Semi e) = Semi <$> transformM go e
    goStmt (NoSemi e) = NoSemi <$> transformM go e

    goPat (IdentP (IdentPat mode bnd)) = do
      s <- get
      mode' <- case HS.member bnd s of
                 True -> do
                   put $ HS.delete bnd s
                   return Mutable
                 False -> return mode
      return $ IdentP $ IdentPat mode' bnd
    goPat e = pure e
