module Ohua.Util
    ( runExceptM
    , neUnzip
    , neUnzip3
    ) where

import Universum
import Data.List.NonEmpty ((<|))
import Ohua.Types.Error


runExceptM :: ExceptT Error IO a -> IO a
runExceptM c = runExceptT c >>= either error pure

neUnzip :: NonEmpty (a,b) -> (NonEmpty a, NonEmpty b)
neUnzip ((x,y) :| [] ) = (x:|[], y:|[])
neUnzip ((x,y) :| (xy : xys)) =
  let (xs, ys) = neUnzip (xy :| xys)
  in (x<|xs , y<|ys)

neUnzip3 :: NonEmpty (a,b,c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
neUnzip3 ((x,y,z) :| [] ) = (x:|[], y:|[], z:|[])
neUnzip3 ((x,y,z) :| (xyz : xyzs)) =
  let (xs, ys, zs) = neUnzip3 (xyz :| xyzs)
  in (x<|xs , y<|ys, z<|zs)
