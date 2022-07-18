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


funs =[sourceFile|    
    fn hello_world() -> String { unimplemented!{} }  
    fn f() -> i32 { unimplemented!{} }  
    fn f_tup() -> (i32,i32) { unimplemented!{} }  
    fn fi_tup(i:i32) -> (i32,i32) { unimplemented!{} }  
    fn f_arc() -> Arc<i32> { unimplemented!{} }  
    fn g(i:i32) -> String { unimplemented!{} }  
    fn h(i:i32) -> i32 { unimplemented!{} }  
    fn h2(i:i32, j:i32) -> i32 { unimplemented!{} } 
    fn f0(i:i32) -> i32 { unimplemented!{} }  
    fn f1(i:i32) -> i32 { unimplemented!{} }  
    fn f2(i:i32) -> i32 { unimplemented!{} }  
    fn g0(i:i32) -> i32 { unimplemented!{} }  
    fn g1(i:i32) -> i32 { unimplemented!{} }  
    fn check(i:i32) -> bool { unimplemented!{} }  
    
    struct S {}  
    impl S {  
      fn new_state(i:i32) -> S { unimplemented!{} }  
      fn gs(self, i:i32) -> i32 { unimplemented!{} }  
      fn modify(&mut self, i:i32) { unimplemented!{} }  
      fn gs1(self, i:i32) -> String { unimplemented!{} }  
    }  
    fn k(s:S) -> () { unimplemented!{} }  
    
    fn iter() -> Iterator<S> { unimplemented!{} }  
    impl Iterator for S {  
      type Item=S;  
      fn next(&mut self) -> Option<S> { unimplemented!{} }  
      fn size_hint(&self) -> (usize, Option<usize>) { unimplemented!{} }  
      fn has_next(&self) -> bool { unimplemented!{} }  
    }  
    impl Clone for S {  
      fn clone(&self) -> Self { unimplemented!() }  
    }  
    fn iter_i32() -> Iterator<i32> { unimplemented!{} }  
    fn f_s(s:&S, i:i32) -> i32 { unimplemented!() }  
|]
  

std = [sourceFile|
    struct Vec<T> {}  
    impl<T> Vec<T> {  
      pub fn default() -> Self { unimplemented!() }  
      pub fn push(&mut self, value: T) { unimplemented!() }  
      pub fn evict_mapped(&mut self) { unimplemented!() }  
      pub fn calculate_done1(&mut self, its_left: u32) -> bool { unimplemented!() }  
      pub fn len(&self) -> usize { unimplemented!() }  
      pub fn exp(&mut self, other: Self) { unimplemented!() }  
    }  
     
    enum Option<T> {}  
     
    struct Arc<T> {}  
    impl<T> Arc<T> {  
      fn new(i: T) -> Self { unimplemented!() }  
    }  
    impl<T> Clone for Arc<T> {  
      fn clone(&self) -> Self { unimplemented!() }  
    }  
     
    pub fn id<T>(item: T) -> T { unimplemented!() }  
 |] 
