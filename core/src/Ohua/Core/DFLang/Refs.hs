module Ohua.Core.DFLang.Refs where

import Ohua.Core.Prelude

import qualified Ohua.Core.ALang.Refs as Refs


collect :: QualifiedBinding
collect = Refs.collect

--smapFun :: QualifiedBinding
--smapFun = Refs.smapFun

--ifFun :: QualifiedBinding
--ifFun = Refs.ifFun

select :: QualifiedBinding
select = Refs.select

ctrl :: QualifiedBinding
ctrl = Refs.ctrl

--recurFunBnd :: QualifiedBinding
--recurFunBnd = "ohua.lang/recurFun"

--recurFun :: QualifiedBinding
--recurFun = recurFunBnd

seqFun :: QualifiedBinding
seqFun = Refs.seq

unitFun :: QualifiedBinding
unitFun = Refs.unitFun

runSTCLangSMap :: QualifiedBinding
runSTCLangSMap = Refs.runSTCLangSMap

runSTCLangIf :: QualifiedBinding
runSTCLangIf = Refs.runSTCLangIf

id :: QualifiedBinding
id = Refs.id
