module Ohua.Integration.Rust.Backend.Passes where

import Ohua.Prelude
import Ohua.Integration.Rust.Backend.Subset

import qualified Data.HashSet as HS

-- | Rust wants all __mutable__ variables to be tagged.
--   This transformation finds them and does so.
propagateMut :: Expr -> Expr
propagateMut = (`evalState` HS.empty)  . transformM go
  where
    go e@(MethodCall (Var bnd) _ _) = do
      modify $ HS.insert bnd
      return e
    go e@(Assign (Var bnd) _) = do
      modify $ HS.insert bnd
      return e
    go (Local p e) = do
      p' <- transformM goPat p
      return $ Local p' e
    go e = pure e

    goPat (IdentP (IdentPat mode bnd t)) = do
      s <- get
      mode' <- case HS.member bnd s of
                 True -> do
                   put $ HS.delete bnd s
                   return Mutable
                 False -> return mode
      return $ IdentP $ IdentPat mode' bnd t
    goPat e = pure e
