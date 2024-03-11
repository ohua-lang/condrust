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
  -> Namespace (Program (Channel embExpr annot ty) (Com 'Recv embExpr annot ty) (TaskExpr embExpr annot ty) embExpr  ty) anno (OhuaType ty 'Resolved)
  -> Namespace (Program (Channel embExpr annot ty) (Com 'Recv embExpr annot ty) (TaskExpr embExpr annot ty) embExpr annot ty) anno (OhuaType ty 'Resolved)
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
  -> Namespace (Program (Channel embExpr annot ty) (Com 'Recv embExpr annot ty) (Task lang) embExpr annot ty) anno (OhuaType ty 'Resolved)
  -> Namespace (Program (Channel embExpr annot ty) (Com 'Recv embExpr annot ty) (Task lang) embExpr annot ty) anno (OhuaType ty 'Resolved)
transformTasks lang arch = updateTasks (transformTask lang arch)
