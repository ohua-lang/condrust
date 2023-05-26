module Ohua.Core.InternalFunctions where

import Ohua.Prelude

-- ToDo: These functions need to be typed here. VarType is indepent of IR and we can make them FunTypes with defined types

id :: QualifiedBinding
id = "ohua.lang/id"

-- transforms into `ifFun` and `select`
ifThenElse :: QualifiedBinding
ifThenElse = "ohua.lang/if"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `ifThenElse`
ifFun :: QualifiedBinding
ifFun = "ohua.lang/ifFun"

select :: QualifiedBinding
select = "ohua.lang/select"

-- transforms into `smapFun` and `collect`
smap :: QualifiedBinding
smap = "ohua.lang/smap"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `smap`
smapFun :: QualifiedBinding
smapFun = "ohua.lang/smapFun"

collect :: QualifiedBinding
collect = "ohua.lang/collect"

-- transforms into `seqFun`
seq :: QualifiedBinding
seq = "ohua.lang/seq"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `seq`
seqFun :: QualifiedBinding
seqFun = "ohua.lang/seq"

recur :: QualifiedBinding
recur = "ohua.lang/recur"

nth :: QualifiedBinding
nth = "ohua.lang/nth"

ctrl :: QualifiedBinding
ctrl = "ohua.lang/ctrl"

unitFun :: QualifiedBinding
unitFun = "ohua.lang/unitFun"


runSTCLangIf :: QualifiedBinding
runSTCLangIf = "ohua.lang/runSTCLang-If"

runSTCLangSMap :: QualifiedBinding
runSTCLangSMap = "ohua.lang/runSTCLang-Smap"


recurFun :: QualifiedBinding
recurFun = "ohua.lang/recurFun"

tupleFun :: QualifiedBinding
tupleFun = "ohua.lang/(,)"

-------------------------------------------------------------------------------
--  Helper functions to wrap Strings as namespaced bindings or expressions
-------------------------------------------------------------------------------

ohuaLangNS :: NSRef
ohuaLangNS = makeThrow ["ohua", "lang"]

mkTuple :: QualifiedBinding
mkTuple = QualifiedBinding ohuaLangNS "(,)"
