import Ohua.Prelude

import qualified DFLowering

import qualified TailRecSpec
-- FIXME
import           PassesSpec
import Test.Hspec
import qualified TestALangUtils
import qualified TestAesonConvert

main :: IO ()
main =
    hspec $
     do
        passesSpec
        DFLowering.generalLowering
        DFLowering.ifSpec
        DFLowering.seqSpec
        DFLowering.smapSpec
        TestALangUtils.spec
        TestAesonConvert.spec
        TailRecSpec.passesSpec
