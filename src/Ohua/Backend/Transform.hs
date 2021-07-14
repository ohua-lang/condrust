module Ohua.Backend.Transform where

import Ohua.Prelude hiding (Type)

import Ohua.Backend.Types
import Ohua.Backend.Lang


transform :: ( Architecture arch
             , Lang arch ~ lang
             , Integration lang
             , ty ~ Type lang
             , anno ~ AlgoSrc lang
             , Transform arch
             )
          => NS lang -> arch
          -> Namespace (TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)) anno
          -> Namespace (TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)) anno
transform lang arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ go)
  where
    go (TCProgram chans resultChan exprs) =
      TCProgram chans resultChan $ map (transformTaskExpr lang arch) exprs
