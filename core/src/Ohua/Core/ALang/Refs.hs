module Ohua.Core.ALang.Refs where

import Ohua.Prelude

id :: QualifiedBinding
id = "Ohua.Core.lang/id"

-- transforms into `ifFun` and `select`
ifThenElse :: QualifiedBinding
ifThenElse = "Ohua.Core.lang/if"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `ifThenElse`
ifFun :: QualifiedBinding
ifFun = "Ohua.Core.lang/ifFun"

select :: QualifiedBinding
select = "Ohua.Core.lang/select"

-- transforms into `smapFun` and `collect`
smap :: QualifiedBinding
smap = "Ohua.Core.lang/smap"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `smap`
smapFun :: QualifiedBinding
smapFun = "Ohua.Core.lang/smapFun"

collect :: QualifiedBinding
collect = "Ohua.Core.lang/collect"

-- transforms into `seqFun`
seq :: QualifiedBinding
seq = "Ohua.Core.lang/seq"

-- TODO: maybe these functions belong into the concrete passes
-- semantically a different function from `seq`
seqFun :: QualifiedBinding
seqFun = "Ohua.Core.lang/seqFun"

recur :: QualifiedBinding
recur = "Ohua.Core.lang/recur"

nth :: QualifiedBinding
nth = "Ohua.Core.lang/nth"

ctrl :: QualifiedBinding
ctrl = "Ohua.Core.lang/ctrl"
