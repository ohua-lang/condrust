module Ohua.Commons.Util
    ( runExceptM
    , neUnzip
    , neUnzip3
    , neUnzip4
    , neConcat
    ) where

import Universum
import Data.List.NonEmpty ((<|), NonEmpty)
import Ohua.Commons.Types.Error


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

neUnzip4 :: NonEmpty (a,b,c, d) -> (NonEmpty a, NonEmpty b, NonEmpty c, NonEmpty d)
neUnzip4 ((w,x,y,z) :| [] ) = (w:|[], x:|[], y:|[], z:|[])
neUnzip4 ((w,x,y,z) :| (wxyz : wxyzs)) =
  let (ws, xs, ys, zs) = neUnzip4 (wxyz :| wxyzs)
  in (w<| ws, x<|xs , y<|ys, z<|zs)

neConcat :: (NonEmpty (NonEmpty a)) -> NonEmpty a
neConcat (x :| []) = x
neConcat (x :| (y:ys)) = x <> neConcat (y:|ys)
