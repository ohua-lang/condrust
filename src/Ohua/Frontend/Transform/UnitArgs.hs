module Ohua.Frontend.Transform.UnitArgs where
    
import Ohua.Prelude

import Ohua.Frontend.Lang


noEmptyArgs :: ErrAndLogM m => UnresolvedExpr ty -> m (UnresolvedExpr ty)
noEmptyArgs expr =  return $ preWalkE addUnit expr
    where addUnit = \case
                LamEU [] fn -> LamEU [VarP "_" $ UType] fn
                AppEU fn [] -> AppEU fn [LitE UnitLit]
                e -> e