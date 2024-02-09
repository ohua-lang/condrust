{-#LANGUAGE TypeOperators #-}
module Ohua.Backend.Transform where

import Ohua.Commons.Prelude hiding (Type)

import Ohua.Backend.Types
import Ohua.Backend.Lang


transformTaskExprs
  :: ( Architecture arch
     , Lang arch ~ lang
     , Integration lang
     , ty ~ Type lang
     , anno ~ AlgoSrc lang
     , embExpr ~ EmbExpr lang
     , Transform arch
     )
  => HostModule lang -> arch
  -> Namespace (Program (Channel embExpr ty) (Com 'Recv embExpr ty) (TaskExpr embExpr ty) embExpr  ty) anno (OhuaType ty 'Resolved)
  -> Namespace (Program (Channel embExpr ty) (Com 'Recv embExpr ty) (TaskExpr embExpr ty) embExpr ty) anno (OhuaType ty 'Resolved)
transformTaskExprs lang arch = updateTaskExprs (transformTaskExpr lang arch)

transformTasks
  :: ( Architecture arch
     , Lang arch ~ lang
     , Integration lang
     , ty ~ Type lang
     , anno ~ AlgoSrc lang
     , embExpr ~ EmbExpr lang
     , Transform arch
     )
  => HostModule lang -> arch
  -> Namespace (Program (Channel embExpr ty) (Com 'Recv embExpr ty) (Task lang) embExpr ty) anno (OhuaType ty 'Resolved)
  -> Namespace (Program (Channel embExpr ty) (Com 'Recv embExpr ty) (Task lang) embExpr ty) anno (OhuaType ty 'Resolved)
transformTasks lang arch = updateTasks (transformTask lang arch)
