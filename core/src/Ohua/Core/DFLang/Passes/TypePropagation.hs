module Ohua.Core.DFLang.Passes.TypePropagation where

import Data.HashMap.Lazy as HM
import Ohua.Core.DFLang.Lang
import qualified Ohua.Core.DFLang.Refs as Refs
import Ohua.Core.Prelude

data Exists ty = forall semTy.Exists (DFVar semTy ty)

-- TODO collect variables from all calls (pure and stateful)
-- TODO handle smapFun, recurFun, select, ifFun, ....
propagateTypes :: NormalizedDFExpr ty -> NormalizedDFExpr ty
propagateTypes e = evalState (transformExprM go e) HM.empty
  where
    go ::
      NormalizedDFExpr ty ->
      State (HM.HashMap Binding (Exists ty)) (NormalizedDFExpr ty)
    -- (Sebastian to himself): Implement these damn types already!!!
    go (Let (PureDFFun out@(Direct outBnd) f@(FunRef fun _ _) [ctrlSig, DFVar ty dataInpBnd]) ct)
      | fun == Refs.ctrl = do
        vars <- get
        let dataInp' = case HM.lookup (unwrapABnd outBnd) vars of
              Just (Exists (DFVar ty' _)) -> DFVar ty' dataInpBnd
              _ -> DFVar ty dataInpBnd
        -- add it to the used vars
        modify (HM.insert (unwrapABnd dataInpBnd) $ Exists dataInp')
        return $ Let (PureDFFun out f [ctrlSig, dataInp']) ct
    go e' = return e'
