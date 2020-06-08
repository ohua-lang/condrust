module Ohua.Core.DFLang.Refs where

import Ohua.Core.Prelude

import qualified Ohua.Core.ALang.Refs as Refs
import Ohua.Core.DFLang.Lang

import Data.HashMap.Lazy as HM

lowerBuiltinFunctions :: QualifiedBinding -> Maybe DFFnRef
lowerBuiltinFunctions = flip HM.lookup builtInFunctions
  where
    builtInFunctions =
        HM.fromList
            [ (Refs.collect, collect)
            , (Refs.smapFun, smapFun)
            , (Refs.ifFun, ifFun)
            , (Refs.select, select)
            , (Refs.ctrl, ctrl)
            , (recurFunBnd, recurFun)
            , (Refs.seqFun, seqFun)
            ]

collect :: DFFnRef
collect = DFFunction Refs.collect

smapFun :: DFFnRef
smapFun = DFFunction Refs.smapFun

ifFun :: DFFnRef
ifFun = EmbedSf Refs.ifFun

select :: DFFnRef
select = DFFunction Refs.select

ctrl :: DFFnRef
ctrl = DFFunction Refs.ctrl

recurFunBnd :: QualifiedBinding
recurFunBnd = "Ohua.Core.lang/recurFun"

recurFun :: DFFnRef
recurFun = DFFunction recurFunBnd

seqFun :: DFFnRef
seqFun = EmbedSf Refs.seqFun
