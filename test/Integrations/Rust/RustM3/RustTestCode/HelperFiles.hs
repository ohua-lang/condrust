{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustM3.RustTestCode.HelperFiles where

import Data.Text as T (Text)
import Language.Rust.Quote


-- ISSUE: Replace unecessay stuff
cargoFile :: Text
cargoFile =
  " [package] \n\
  \ name = \"ohua-m3-test\" \n\
  \ version = \"0.1.0\" \n\
  \ edition = \"2021\" \n\
  \ \n\
  \ [dependencies] \n\
  \ "

libFile = [sourceFile|
    mod funs;  
    mod benchs;  
    mod test;  
     
    fn main() {  
        test::test();  
    }  
|]

