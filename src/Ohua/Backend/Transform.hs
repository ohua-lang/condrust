module Ohua.Backend.Transform where

import Ohua.Prelude hiding (Type)

import Ohua.Backend.Types
import Ohua.Backend.Lang


transformTaskExprs
  :: ( Architecture arch
     , Lang arch ~ lang
     , Integration lang
     , ty ~ Type lang
     , anno ~ AlgoSrc lang
     , Transform arch
     )
  => HostModule lang -> arch
  -> Namespace (Program (Channel ty) (Com 'Recv ty) (TaskExpr ty) ty) anno (HostType ty)
  -> Namespace (Program (Channel ty) (Com 'Recv ty) (TaskExpr ty) ty) anno (HostType ty)
transformTaskExprs lang arch = updateTaskExprs (transformTaskExpr lang arch)

transformTasks
  :: ( Architecture arch
     , Lang arch ~ lang
     , Integration lang
     , ty ~ Type lang
     , anno ~ AlgoSrc lang
     , Transform arch
     )
  => HostModule lang -> arch
  -> Namespace (Program (Channel ty) (Com 'Recv ty) (Task lang) ty) anno (HostType ty)
  -> Namespace (Program (Channel ty) (Com 'Recv ty) (Task lang) ty) anno (HostType ty)
transformTasks lang arch = updateTasks (transformTask lang arch)
