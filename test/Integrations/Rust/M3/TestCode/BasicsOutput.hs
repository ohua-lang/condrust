{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.BasicsOutput where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)


helloWorld :: SourceFile Span
helloWorld = [sourceFile|
use crate::funs::hello_world;

fn test() -> String {
   use m3::com::channel::{Sender, Receiver};
   use m3::activity;
   let (a_0_0_tx, mut a_0_0_rx) = channel(); 
    activity!(
        (|a_0_0_child_tx: Sender| {    
         let a_0_0 = hello_world();
        a_0_0_child_tx.send(a_0_0)?;
        Ok(())
          }
        )(a_0_0_tx)
    );
    a_0_0_rx.activate()?;
    a_0_0_rx
    .recv::<String,>()
    .expect("The retrieval of the result value failed.\nOhua turned your sequential program into a distributed one.\nHence, all Ohua can do at this point is error out.\nIf you would like to have support for handligng these errors in your application then please submit an issue.\n")
}
                |]

simpleComposition :: SourceFile Span
simpleComposition = [sourceFile|
use crate::funs::{f, g};


fn test() -> String {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (a_0_0_tx, mut a_0_0_rx) = channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = channel();
  activity!(
    (
      |a_0_0_child_tx: Sender, x_0_0_0_child_tx: Receiver| {
       
 Ok(
          loop {
            let var_0 = x_0_0_0_child_rx.recv::<  i32,>()?;
            let a_0_0 = g(var_0);
            a_0_0_child_tx.send(a_0_0)?;
            ()
          }
        )
      }
    )(a_0_0_tx, x_0_0_0_rx)
  );
  activity!(
    (
      |x_0_0_0_child_tx: Sender| {
        let x_0_0_0 = f();
        x_0_0_0_child_tx.send(x_0_0_0)?;
        Ok(())
      }
    )(x_0_0_0_tx)
  );
  a_0_0_rx.activate()?;
  a_0_0_rx
    .recv::<  String,>()
    .expect("The retrieval of the result value failed.\nOhua turned your sequential program into a distributed one.\nHence, all Ohua can do at this point is error out.\nIf you would like to have support for handligng these errors in your application then please submit an issue.\n")
}
                |]

cloneVar::SourceFile Span
cloneVar =  [sourceFile|
use crate::funs::*;

fn test() -> String {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (a_0_0_tx, mut a_0_0_rx) = channel();
  let (x_0_0_1_tx, x_0_0_1_rx) = channel();
  let (x_0_0_0_0_tx, x_0_0_0_0_rx) = channel();
  let (y_0_0_0_tx, y_0_0_0_rx) = channel();
  let (x1_0_0_0_tx, x1_0_0_0_rx) = channel();
  activity!(
    (
      |a_0_0_child_tx: Sender, x1_0_0_0_child_tx: Receiver, y_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x1_0_0_0_child_rx.recv::<  String,>()?;
            let var_1 = y_0_0_0_child_rx.recv::<  String,>()?;
            let a_0_0 = h2(var_0, var_1);
            a_0_0_child_tx.send(a_0_0)?;
            ()
          }
        )
      }
    )(a_0_0_tx, x1_0_0_0_rx, y_0_0_0_rx)
  );
  activity!(
    (
      |y_0_0_0_child_tx: Sender, x_0_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x_0_0_0_0_child_rx.recv::<  String,>()?;
            let y_0_0_0 = h(var_0);
            y_0_0_0_child_tx.send(y_0_0_0)?;
            ()
          }
        )
      }
    )(y_0_0_0_tx, x_0_0_0_0_rx)
  );
  activity!(
    (
      |x1_0_0_0_child_tx: Sender, x_0_0_0_0_child_tx: Sender, x_0_0_1_child_tx: Receiver| {
        Ok(
          loop {
            let mut var_0 = x_0_0_1_child_rx.recv::<  String,>()?;
            let x1_0_0_0 = var_0.clone();
            x1_0_0_0_child_tx.send(x1_0_0_0)?;
            x_0_0_0_0_child_tx.send(var_0)?
          }
        )
      }
    )(x1_0_0_0_tx, x_0_0_0_0_tx, x_0_0_1_rx)
  );
  activity!(
    (
      |x_0_0_1_child_tx: Sender| {
        let x_0_0_1 = f();
        x_0_0_1_child_tx.send(x_0_0_1)?;
        Ok(())
      }
    )(x_0_0_1_tx)
  );
  a_0_0_rx.activate()?;
  a_0_0_rx
    .recv::<  String,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}
|]



arcAndClone :: SourceFile Span
arcAndClone =  [sourceFile|
use crate::funs::*;

fn test() -> String {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (a_0_0_tx, mut a_0_0_rx) = channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = channel();
  let (x1_0_0_1_tx, x1_0_0_1_rx) = channel();
  let (x1_0_0_0_0_tx, x1_0_0_0_0_rx) = channel();
  let (y_0_0_0_tx, y_0_0_0_rx) = channel();
  let (x2_0_0_0_tx, x2_0_0_0_rx) = channel();
  activity!(
    (
      |a_0_0_child_tx: Sender, x2_0_0_0_child_tx: Receiver, y_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x2_0_0_0_child_rx.recv::<  Arc<  i32,>,>()?;
            let var_1 = y_0_0_0_child_rx.recv::<  i32,>()?;
            let a_0_0 = h2(var_0, var_1);
            a_0_0_child_tx.send(a_0_0)?;
            ()
          }
        )
      }
    )(a_0_0_tx, x2_0_0_0_rx, y_0_0_0_rx)
  );
  activity!(
    (
      |y_0_0_0_child_tx: Sender, x1_0_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x1_0_0_0_0_child_rx.recv::<  Arc<  i32,>,>()?;
            let y_0_0_0 = h(var_0);
            y_0_0_0_child_tx.send(y_0_0_0)?;
            ()
          }
        )
      }
    )(y_0_0_0_tx, x1_0_0_0_0_rx)
  );
  activity!(
    (
      |x2_0_0_0_child_tx: Sender, x1_0_0_0_0_child_tx: Sender, x1_0_0_1_child_tx: Receiver| {
        Ok(
          loop {
            let mut var_0 = x1_0_0_1_child_rx.recv::<  Arc<  i32,>,>()?;
            let x2_0_0_0 = var_0.clone();
            x2_0_0_0_child_tx.send(x2_0_0_0)?;
            x1_0_0_0_0_child_tx.send(var_0)?
          }
        )
      }
    )(x2_0_0_0_tx, x1_0_0_0_0_tx, x1_0_0_1_rx)
  );
  activity!(
    (
      |x1_0_0_1_child_tx: Sender, x_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x_0_0_0_child_rx.recv::<  i32,>()?;
            let x1_0_0_1 = std::sync::Arc::new(var_0);
            x1_0_0_1_child_tx.send(x1_0_0_1)?;
            ()
          }
        )
      }
    )(x1_0_0_1_tx, x_0_0_0_rx)
  );
  activity!(
    (
      |x_0_0_0_child_tx: Sender| {
        let x_0_0_0 = f();
        x_0_0_0_child_tx.send(x_0_0_0)?;
        Ok(())
      }
    )(x_0_0_0_tx)
  );
  a_0_0_rx.activate()?;
  a_0_0_rx
    .recv::<  String,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}
|] 

useFun:: SourceFile Span
useFun = [sourceFile|
use crate::funs::*;

fn test(i: i32) -> String {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (a_0_0_tx, mut a_0_0_rx) = channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = channel();
  activity!(
    (
      |a_0_0_child_tx: Sender, x_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x_0_0_0_child_rx.recv::<  i32,>()?;
            let a_0_0 = funs::g(var_0);
            a_0_0_child_tx.send(a_0_0)?;
            ()
          }
        )
      }
    )(a_0_0_tx, x_0_0_0_rx)
  );
  activity!(
    (
      |x_0_0_0_child_tx: Sender| {
        let x_0_0_0 = funs::h(i);
        x_0_0_0_child_tx.send(x_0_0_0)?;
        Ok(())
      }
    )(x_0_0_0_tx)
  );
  a_0_0_rx.activate()?;
  a_0_0_rx
    .recv::<  String,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}

|]

inlineAlgo::SourceFile Span 
inlineAlgo = [sourceFile|
use crate::funs::*;

fn algo(i: i32) -> String {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (a_0_0_tx, mut a_0_0_rx) = channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = channel();
  activity!(
    (
      |a_0_0_child_tx: Sender, x_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x_0_0_0_child_rx.recv::<  i32,>()?;
            let a_0_0 = g(var_0);
            a_0_0_child_tx.send(a_0_0)?;
            ()
          }
        )
      }
    )(a_0_0_tx, x_0_0_0_rx)
  );
  activity!(
    (
      |x_0_0_0_child_tx: Sender| {
        let x_0_0_0 = h(i);
        x_0_0_0_child_tx.send(x_0_0_0)?;
        Ok(())
      }
    )(x_0_0_0_tx)
  );
  a_0_0_rx.activate()?;
  a_0_0_rx
    .recv::<  String,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}

fn test() -> String {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (a_0_0_tx, mut a_0_0_rx) = channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = channel();
  activity!(
    (
      |a_0_0_child_tx: Sender, x_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x_0_0_0_child_rx.recv::<  i32,>()?;
            let a_0_0 = g(var_0);
            a_0_0_child_tx.send(a_0_0)?;
            ()
          }
        )
      }
    )(a_0_0_tx, x_0_0_0_rx)
  );
  activity!(
    (
      |x_0_0_0_child_tx: Sender| {
        let x_0_0_0 = h(4);
        x_0_0_0_child_tx.send(x_0_0_0)?;
        Ok(())
      }
    )(x_0_0_0_tx)
  );
  a_0_0_rx.activate()?;
  a_0_0_rx
    .recv::<  String,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}

|]


algoLoadingEnv::SourceFile Span
algoLoadingEnv = [sourceFile|
use crate::funs::*;

fn algo(i: i32) -> String {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (a_0_0_tx, mut a_0_0_rx) = channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = channel();
  activity!(
    (
      |a_0_0_child_tx: Sender, x_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x_0_0_0_child_rx.recv::<  i32,>()?;
            let a_0_0 = funs::g(var_0);
            a_0_0_child_tx.send(a_0_0)?;
            ()
          }
        )
      }
    )(a_0_0_tx, x_0_0_0_rx)
  );
  activity!(
    (
      |x_0_0_0_child_tx: Sender| {
        let x_0_0_0 = funs::h(i);
        x_0_0_0_child_tx.send(x_0_0_0)?;
        Ok(())
      }
    )(x_0_0_0_tx)
  );
  a_0_0_rx.activate()?;
  a_0_0_rx
    .recv::<  String,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}

fn test() -> String {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (a_0_0_tx, mut a_0_0_rx) = channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = channel();
  activity!(
    (
      |a_0_0_child_tx: Sender, x_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x_0_0_0_child_rx.recv::<  i32,>()?;
            let a_0_0 = funs::g(var_0);
            a_0_0_child_tx.send(a_0_0)?;
            ()
          }
        )
      }
    )(a_0_0_tx, x_0_0_0_rx)
  );
  activity!(
    (
      |x_0_0_0_child_tx: Sender| {
        let x_0_0_0 = funs::h(4);
        x_0_0_0_child_tx.send(x_0_0_0)?;
        Ok(())
      }
    )(x_0_0_0_tx)
  );
  a_0_0_rx.activate()?;
  a_0_0_rx
    .recv::<  String,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}

|]

tupleFromUnit::SourceFile Span
tupleFromUnit = [sourceFile|
use crate::funs::*;

fn test() -> i32 {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (b_0_0_tx, mut b_0_0_rx) = channel();
  let (x0_0_0_0_tx, x0_0_0_0_rx) = channel();
  let (y0_0_0_0_tx, y0_0_0_0_rx) = channel();
  let (y1_0_0_0_tx, y1_0_0_0_rx) = channel();
  let (x1_0_0_0_tx, x1_0_0_0_rx) = channel();
  activity!(
    (
      |b_0_0_child_tx: Sender, x1_0_0_0_child_tx: Receiver, y1_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x1_0_0_0_child_rx.recv::<  i32,>()?;
            let var_1 = y1_0_0_0_child_rx.recv::<  String,>()?;
            let b_0_0 = h2(var_0, var_1);
            b_0_0_child_tx.send(b_0_0)?;
            ()
          }
        )
      }
    )(b_0_0_tx, x1_0_0_0_rx, y1_0_0_0_rx)
  );
  activity!(
    (
      |y1_0_0_0_child_tx: Sender, y0_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = y0_0_0_0_child_rx.recv::<  String,>()?;
            let y1_0_0_0 = f1(var_0);
            y1_0_0_0_child_tx.send(y1_0_0_0)?;
            ()
          }
        )
      }
    )(y1_0_0_0_tx, y0_0_0_0_rx)
  );
  activity!(
    (
      |x1_0_0_0_child_tx: Sender, x0_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x0_0_0_0_child_rx.recv::<  i32,>()?;
            let x1_0_0_0 = f0(var_0);
            x1_0_0_0_child_tx.send(x1_0_0_0)?;
            ()
          }
        )
      }
    )(x1_0_0_0_tx, x0_0_0_0_rx)
  );
  activity!(
    (
      |x0_0_0_0_child_tx: Sender, y0_0_0_0_child_tx: Sender| {
        let res = f_tup();
        let x0_0_0_0 = res.0;
        x0_0_0_0_child_tx.send(x0_0_0_0)?;
        let y0_0_0_0 = res.1;
        y0_0_0_0_child_tx.send(y0_0_0_0)?;
        Ok(())
      }
    )(x0_0_0_0_tx, y0_0_0_0_tx)
  );
  b_0_0_rx.activate()?;
  b_0_0_rx
    .recv::<  i32,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}
|]

tupleFromParam:: SourceFile Span
tupleFromParam = [sourceFile|
use crate::funs::*;

fn test() -> i32 {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (b_0_0_tx, mut b_0_0_rx) = channel();
  let (x0_0_0_0_tx, x0_0_0_0_rx) = channel();
  let (y0_0_0_0_tx, y0_0_0_0_rx) = channel();
  let (y1_0_0_0_tx, y1_0_0_0_rx) = channel();
  let (x1_0_0_0_tx, x1_0_0_0_rx) = channel();
  activity!(
    (
      |b_0_0_child_tx: Sender, x1_0_0_0_child_tx: Receiver, y1_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x1_0_0_0_child_rx.recv::<  i32,>()?;
            let var_1 = y1_0_0_0_child_rx.recv::<  String,>()?;
            let b_0_0 = h2(var_0, var_1);
            b_0_0_child_tx.send(b_0_0)?;
            ()
          }
        )
      }
    )(b_0_0_tx, x1_0_0_0_rx, y1_0_0_0_rx)
  );
  activity!(
    (
      |y1_0_0_0_child_tx: Sender, y0_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = y0_0_0_0_child_rx.recv::<  String,>()?;
            let y1_0_0_0 = f1(var_0);
            y1_0_0_0_child_tx.send(y1_0_0_0)?;
            ()
          }
        )
      }
    )(y1_0_0_0_tx, y0_0_0_0_rx)
  );
  activity!(
    (
      |x1_0_0_0_child_tx: Sender, x0_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = x0_0_0_0_child_rx.recv::<  i32,>()?;
            let x1_0_0_0 = f0(var_0);
            x1_0_0_0_child_tx.send(x1_0_0_0)?;
            ()
          }
        )
      }
    )(x1_0_0_0_tx, x0_0_0_0_rx)
  );
  activity!(
    (
      |x0_0_0_0_child_tx: Sender, y0_0_0_0_child_tx: Sender| {
        let res = f_tup(23);
        let x0_0_0_0 = res.0;
        x0_0_0_0_child_tx.send(x0_0_0_0)?;
        let y0_0_0_0 = res.1;
        y0_0_0_0_child_tx.send(y0_0_0_0)?;
        Ok(())
      }
    )(x0_0_0_0_tx, y0_0_0_0_tx)
  );
  b_0_0_rx.activate()?;
  b_0_0_rx
    .recv::<  i32,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}
|]

conditionBinOp :: SourceFile Span
conditionBinOp = [sourceFile|

pub fn dunno(){
    todo!()
}

|]

smapBound :: SourceFile Span
smapBound = [sourceFile|
use crate::funs::*;

fn test() -> State {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (s_0_1_0_tx, mut s_0_1_0_rx) = channel();
  let (s_0_0_1_tx, s_0_0_1_rx) = channel();
  let (ctrl_0_0_tx, ctrl_0_0_rx) = channel();
  let (d_0_tx, d_0_rx) = channel();
  let (r_0_0_0_tx, r_0_0_0_rx) = channel();
  let (s_0_0_0_0_tx, s_0_0_0_0_rx) = channel();
  let (size_0_tx, size_0_rx) = channel();
  activity!(
    (
      |s_0_0_0_0_child_tx: Sender, s_0_0_1_child_tx: Receiver, ctrl_0_0_child_tx: Receiver, r_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let mut renew = false;
            let mut s_0_0_1_0 = s_0_0_1_child_rx.recv::<  State,>()?;
            while !renew {
              let sig = ctrl_0_0_child_rx.recv::<  (bool, usize),>()?;
              let count = sig.1;
              for _ in 0 .. count {
                let var_1 = r_0_0_0_child_rx.recv::<  i32,>()?;
                s_0_0_1_0.gs(var_1);
                ()
              };
              let renew_next_time = sig.0;
              renew = renew_next_time;
              ()
            };
            s_0_0_0_0_child_tx.send(s_0_0_1_0)?;
            ()
          }
        )
      }
    )(s_0_0_0_0_tx, s_0_0_1_rx, ctrl_0_0_rx, r_0_0_0_rx)
  );
  activity!(
    (
      |s_0_1_0_child_tx: Sender, size_0_child_tx: Receiver, s_0_0_0_0_child_tx: Receiver, s_0_0_0_0_child_tx: Receiver| {
        let num = size_0_child_rx.recv::<  usize,>()?;
        let toDrop = num - 1;
        for _ in 0 .. toDrop { s_0_0_0_0_child_rx.recv::<  State,>()?; () };
        let s = s_0_0_0_0_child_rx.recv::<  State,>()?;
        Ok(s_0_1_0_child_tx.send(s)?)
      }
    )(s_0_1_0_tx, size_0_rx, s_0_0_0_0_rx, s_0_0_0_0_rx)
  );
  activity!(
    (
      |r_0_0_0_child_tx: Sender, d_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = d_0_child_rx.recv::<  i32,>()?;
            let r_0_0_0 = h(var_0);
            r_0_0_0_child_tx.send(r_0_0_0)?;
            ()
          }
        )
      }
    )(r_0_0_0_tx, d_0_rx)
  );
  activity!(
    (
      |size_0_child_tx: Sender, ctrl_0_0_child_tx: Sender, d_0_child_tx: Sender, d_0_child_tx: Sender, ctrl_0_0_child_tx: Sender, size_0_child_tx: Sender, ctrl_0_0_child_tx: Sender| {
        let mut stream_0_0_0 = iter_i32();
        let hasSize =
        {
          let tmp_has_size = stream_0_0_0.iter().size_hint();
          tmp_has_size.1.is_some()
        };
        Ok(
          if hasSize {
            let size = stream_0_0_0.len();
            size_0_child_tx.send(size)?;
            let ctrl = (true, size);
            ctrl_0_0_child_tx.send(ctrl)?;
            for d in stream_0_0_0 { d_0_child_tx.send(d)?; () }
          } else {
            let mut size = 0;
            for d in stream_0_0_0 {
              d_0_child_tx.send(d)?;
              let ctrl = (false, 1);
              ctrl_0_0_child_tx.send(ctrl)?;
              size = size + 1;
              ()
            };
            size_0_child_tx.send(size)?;
            let ctrl = (true, 0);
            ctrl_0_0_child_tx.send(ctrl)?;
            ()
          }
        )
      }
    )(
      size_0_tx, ctrl_0_0_tx, d_0_tx, d_0_tx, ctrl_0_0_tx, size_0_tx, ctrl_0_0_tx
    )
  );
  activity!(
    (
      |s_0_0_1_child_tx: Sender| {
        let s_0_0_1 = State::new_state();
        s_0_0_1_child_tx.send(s_0_0_1)?;
        Ok(())
      }
    )(s_0_0_1_tx)
  );
  s_0_1_0_rx.activate()?;
  s_0_1_0_rx
    .recv::<  State,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}
|]

smapUnbound :: SourceFile Span
smapUnbound = [sourceFile|
use crate::funs::*;

fn test(i: i32) -> () {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (c_0_0_tx, mut c_0_0_rx) = channel();
  let (s_0_0_1_tx, s_0_0_1_rx) = channel();
  let (ctrl_0_0_tx, ctrl_0_0_rx) = channel();
  let (ctrl_0_1_tx, ctrl_0_1_rx) = channel();
  let (d_1_tx, d_1_rx) = channel();
  let (r_0_0_0_tx, r_0_0_0_rx) = channel();
  let (d_0_0_tx, d_0_0_rx) = channel();
  let (size_0_tx, size_0_rx) = channel();
  let (x_0_0_0_tx, x_0_0_0_rx) = channel();
  activity!(
    (
      |d_0_0_child_tx: Sender, ctrl_0_1_child_tx: Receiver| {
        Ok(
          loop {
            let mut renew = false;
            while !renew {
              let sig = ctrl_0_1_child_rx.recv::<  (bool, usize),>()?;
              let count = sig.1;
              for _ in 0 .. count { d_0_0_child_tx.send(())?; () };
              let renew_next_time = sig.0;
              renew = renew_next_time;
              ()
            }
          }
        )
      }
    )(d_0_0_tx, ctrl_0_1_rx)
  );
  activity!(
    (
      |s_0_0_1_child_tx: Receiver, ctrl_0_0_child_tx: Receiver, r_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let mut renew = false;
            let mut s_0_0_1_0 = s_0_0_1_child_rx.recv::<  State,>()?;
            while !renew {
              let sig = ctrl_0_0_child_rx.recv::<  (bool, usize),>()?;
              let count = sig.1;
              for _ in 0 .. count {
                let var_1 = r_0_0_0_child_rx.recv::<  i32,>()?;
                s_0_0_1_0.gs(var_1);
                ()
              };
              let renew_next_time = sig.0;
              renew = renew_next_time;
              ()
            };
            ()
          }
        )
      }
    )(s_0_0_1_rx, ctrl_0_0_rx, r_0_0_0_rx)
  );
  activity!(
    (
      |x_0_0_0_child_tx: Sender, size_0_child_tx: Receiver, d_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let num = size_0_child_rx.recv::<  usize,>()?;
            let mut collection = Vec::new();
            for _ in 0 .. num {
              let data = d_0_0_child_rx.recv::<  (),>()?;
              collection.push(data)
            };
            x_0_0_0_child_tx.send(collection)?
          }
        )
      }
    )(x_0_0_0_tx, size_0_rx, d_0_0_rx)
  );
  activity!(
    (
      |c_0_0_child_tx: Sender, x_0_0_0_child_tx: Receiver| {
        x_0_0_0_child_rx.recv::<  Vec<  (),>,>()?;
        Ok(c_0_0_child_tx.send(())?)
      }
    )(c_0_0_tx, x_0_0_0_rx)
  );
  activity!(
    (
      |r_0_0_0_child_tx: Sender, d_1_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = d_1_child_rx.recv::<  i32,>()?;
            let r_0_0_0 = h(var_0);
            r_0_0_0_child_tx.send(r_0_0_0)?;
            ()
          }
        )
      }
    )(r_0_0_0_tx, d_1_rx)
  );
  activity!(
    (
      |size_0_child_tx: Sender, ctrl_0_0_child_tx: Sender, ctrl_0_1_child_tx: Sender, d_1_child_tx: Sender, d_1_child_tx: Sender, ctrl_0_0_child_tx: Sender, ctrl_0_1_child_tx: Sender, size_0_child_tx: Sender, ctrl_0_0_child_tx: Sender, ctrl_0_1_child_tx: Sender| {
        let mut a_0_0 = range_from(i);
        let hasSize =
        {
          let tmp_has_size = a_0_0.iter().size_hint(); tmp_has_size.1.is_some()
        };
        Ok(
          if hasSize {
            let size = a_0_0.len();
            size_0_child_tx.send(size)?;
            let ctrl = (true, size);
            ctrl_0_0_child_tx.send(ctrl)?;
            let ctrl = (true, size);
            ctrl_0_1_child_tx.send(ctrl)?;
            for d in a_0_0 { d_1_child_tx.send(d)?; () }
          } else {
            let mut size = 0;
            for d in a_0_0 {
              d_1_child_tx.send(d)?;
              let ctrl = (false, 1);
              ctrl_0_0_child_tx.send(ctrl)?;
              let ctrl = (false, 1);
              ctrl_0_1_child_tx.send(ctrl)?;
              size = size + 1;
              ()
            };
            size_0_child_tx.send(size)?;
            let ctrl = (true, 0);
            ctrl_0_0_child_tx.send(ctrl)?;
            let ctrl = (true, 0);
            ctrl_0_1_child_tx.send(ctrl)?;
            ()
          }
        )
      }
    )(
      size_0_tx,
      ctrl_0_0_tx,
      ctrl_0_1_tx,
      d_1_tx,
      d_1_tx,
      ctrl_0_0_tx,
      ctrl_0_1_tx,
      size_0_tx,
      ctrl_0_0_tx,
      ctrl_0_1_tx,
    )
  );
  activity!(
    (
      |s_0_0_1_child_tx: Sender| {
        let s_0_0_1 = State::new_state();
        s_0_0_1_child_tx.send(s_0_0_1)?;
        Ok(())
      }
    )(s_0_0_1_tx)
  );
  c_0_0_rx.activate()?;
  c_0_0_rx
    .recv::<  (),>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}
|]
multi_var_expl_clone :: SourceFile Span
multi_var_expl_clone = [sourceFile|
use crate::funs::*;

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_1_tx, x_0_0_1_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_0_tx, x_0_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (y_0_0_0_tx, y_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x1_0_0_0_tx, x1_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(x1_0_0_0.sel()).unwrap();
    vpe.delegate_obj(y_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        x1_0_0_0.activate().unwrap();
        y_0_0_0.activate().unwrap();
        loop {
          let var_0 = x1_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let var_1 = y_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let a_0_0 = h2(var_0, var_1);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(y_0_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        y_0_0_0.activate().unwrap();
        x_0_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let y_0_0_0 = h(var_0);
          y_0_0_0_tx.send_msg(y_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x1_0_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x1_0_0_0.activate().unwrap();
        x_0_0_0_0.activate().unwrap();
        x_0_0_1.activate().unwrap();
        loop {
          let var_0 = x_0_0_1_rx.recv_msg::<  State ,>().unwrap();
          let x1_0_0_0 = var_0.clone();
          x1_0_0_0_tx.send_msg(x1_0_0_0).unwrap();
          x_0_0_0_0_tx.send_msg(var_0).unwrap()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_0_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_0_0_1.activate().unwrap();
        let x_0_0_1 = f();
        x_0_0_1_tx.send_msg(x_0_0_1).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}
                |]

env_vars :: SourceFile Span
env_vars =  [sourceFile|
                use funs;

fn test(i: i32) -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let a_0_0 = funs::g(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_0_0_0.activate().unwrap();
        let x_0_0_0 = funs::h(i);
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}
                |]

algo_loading :: SourceFile Span
algo_loading = [sourceFile|
use crate::funs::*;

fn algo(i: i32) -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let a_0_0 = g(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_0_0_0.activate().unwrap();
        let x_0_0_0 = h(i);
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let a_0_0 = g(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_0_0_0.activate().unwrap();
        let x_0_0_0 = h(4);
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}
                |]

algo_loading_env :: SourceFile Span
algo_loading_env =  [sourceFile|
use funs;

fn algo(i: i32) -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let a_0_0 = funs::g(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_0_0_0.activate().unwrap();
        let x_0_0_0 = funs::h(i);
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let a_0_0 = funs::g(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_0_0_0.activate().unwrap();
        let x_0_0_0 = funs::h(4);
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}
                |]


-- Tuple Tests
tuple_from_unit_fun :: SourceFile Span
tuple_from_unit_fun = [sourceFile|
use crate::funs::*;

fn test() -> i32 {
  let (b_0_0_tx, b_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x0_0_0_0_tx, x0_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (y0_0_0_0_tx, y0_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (y1_0_0_0_tx, y1_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x1_0_0_0_tx, x1_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(b_0_0.sel()).unwrap();
    vpe.delegate_obj(x1_0_0_0.sel()).unwrap();
    vpe.delegate_obj(y1_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        b_0_0.activate().unwrap();
        x1_0_0_0.activate().unwrap();
        y1_0_0_0.activate().unwrap();
        loop {
          let var_0 = x1_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let var_1 = y1_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let b_0_0 = h2(var_0, var_1);
          b_0_0_tx.send_msg(b_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(y1_0_0_0.sel()).unwrap();
    vpe.delegate_obj(y0_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        y1_0_0_0.activate().unwrap();
        y0_0_0_0.activate().unwrap();
        loop {
          let var_0 = y0_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let y1_0_0_0 = f1(var_0);
          y1_0_0_0_tx.send_msg(y1_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x1_0_0_0.sel()).unwrap();
    vpe.delegate_obj(x0_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x1_0_0_0.activate().unwrap();
        x0_0_0_0.activate().unwrap();
        loop {
          let var_0 = x0_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let x1_0_0_0 = f0(var_0);
          x1_0_0_0_tx.send_msg(x1_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(y0_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x0_0_0_0.activate().unwrap();
        y0_0_0_0.activate().unwrap();
        let res = f_tup();
        let x0_0_0_0 = res.0;
        x0_0_0_0_tx.send_msg(x0_0_0_0).unwrap();
        let y0_0_0_0 = res.1;
        y0_0_0_0_tx.send_msg(y0_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  b_0_0_rx.recv_msg::<  i32,>().unwrap()
}
                |]

-- ISSUE : There are multiple Problems with this case 
  -- a) same channels are activated and used for delegation twice
  -- b) there are '!' type annotations, which are placeholders for types we don't know -> this must not happen
  -- c) Is it on purpose, that only one of algos is replaced ? 
  -- d) We need to adapt to actual M3 API
if_recursion_only_call_in_branch :: SourceFile Span
if_recursion_only_call_in_branch = [sourceFile|
use crate::funs::*;

fn algo_rec(i: i32, state: State) -> State {
  state.gs(i);
  let i_new = add(i, 1);
  if islowerthan23(i) { algo_rec(i_new, state) } else { state }
}

fn test(i: i32) -> State {
  let (c_0_0_tx, c_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (state_0_0_0_0_tx, state_0_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (i_new_0_0_0_tx, i_new_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (state_1_0_0_tx, state_1_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (state_0_0_1_tx, state_0_0_1_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (i_0_0_0_tx, i_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(i_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        i_0_0_0.activate().unwrap();
        loop {
          let var_0 = i_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let a_0_0 = islowerthan23(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(i_new_0_0_0.sel()).unwrap();
    vpe.delegate_obj(i_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        i_new_0_0_0.activate().unwrap();
        i_0_0_0.activate().unwrap();
        loop {
          let var_0 = i_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let i_new_0_0_0 = add(var_0, 1);
          i_new_0_0_0_tx.send_msg(i_new_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(state_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(state_0_0_1.sel()).unwrap();
    vpe.delegate_obj(i_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        state_0_0_0_0.activate().unwrap();
        state_0_0_1.activate().unwrap();
        i_0_0_0.activate().unwrap();
        loop {
          let var_0 = state_0_0_1_rx.recv_msg::<  State ,>().unwrap();
          let var_1 = i_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          var_0.gs(var_1);
          state_0_0_0_0_tx.send_msg(var_0).unwrap()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(i_0_0_0.sel()).unwrap();
    vpe.delegate_obj(state_0_0_1.sel()).unwrap();
    vpe.delegate_obj(i_0_0_0.sel()).unwrap();
    vpe.delegate_obj(state_0_0_1.sel()).unwrap();
    vpe.delegate_obj(c_0_0.sel()).unwrap();
    vpe.delegate_obj(state_1_0_0.sel()).unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(i_new_0_0_0.sel()).unwrap();
    vpe.delegate_obj(state_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(i_new_0_0_0.sel()).unwrap();
    vpe.delegate_obj(state_0_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        i_0_0_0.activate().unwrap();
        state_0_0_1.activate().unwrap();
        i_0_0_0.activate().unwrap();
        state_0_0_1.activate().unwrap();

        c_0_0.activate().unwrap();
        state_1_0_0.activate().unwrap();
        
        a_0_0.activate().unwrap();
        
        i_new_0_0_0.activate().unwrap();
        state_0_0_0_0.activate().unwrap();
        i_new_0_0_0.activate().unwrap();
        state_0_0_0_0.activate().unwrap();
        
        let init_1 = state_1_0_0_rx.recv_msg::<  !,>().unwrap();
        i_0_0_0_tx.send_msg(i).unwrap();
        state_0_0_1_tx.send_msg(init_1).unwrap();
        while a_0_0_rx.recv_msg::<  !,>().unwrap() {
          let loop_res_0 = i_new_0_0_0_rx.recv_msg::<  !,>().unwrap();
          let loop_res_1 = state_0_0_0_0_rx.recv_msg::<  !,>().unwrap();
          i_0_0_0_tx.send_msg(loop_res_0).unwrap();
          state_0_0_1_tx.send_msg(loop_res_1).unwrap();
          ()
        };
        i_new_0_0_0_rx.recv_msg::<  !,>().unwrap();
        let finalResult = state_0_0_0_0_rx.recv_msg::<  !,>().unwrap();
        c_0_0_tx.send_msg(finalResult).unwrap()
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(state_1_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        state_1_0_0.activate().unwrap();
        let state_1_0_0 = S::new_state();
        state_1_0_0_tx.send_msg(state_1_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  c_0_0_rx.recv_msg::<  State ,>().unwrap()
}
|]

binary_operations :: SourceFile Span
binary_operations = [sourceFile|
fn test() -> i32 {
  let (z1_0_0_0_tx, z1_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (z_0_0_0_tx, z_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(z1_0_0_0.sel()).unwrap();
    vpe.delegate_obj(z_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        z1_0_0_0.activate().unwrap();
        z_0_0_0.activate().unwrap();
        loop {
          let var_0 = z_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let z1_0_0_0 = var_0 * 2;
          z1_0_0_0_tx.send_msg(z1_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(z_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        z_0_0_0.activate().unwrap();
        let z_0_0_0 = 0 + 42;
        z_0_0_0_tx.send_msg(z_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  z1_0_0_0_rx.recv_msg::<  i32,>().unwrap()
}

|]


assign_literal :: SourceFile Span
assign_literal = [sourceFile|
use crate::funs::*;

use std;

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (y_0_0_0_tx, y_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(y_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        y_0_0_0.activate().unwrap();
        loop {
          let var_0 = y_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let a_0_0 = f(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(y_0_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        y_0_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let y_0_0_0 = std::std_fun(var_0);
          y_0_0_0_tx.send_msg(y_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_0_0_0.activate().unwrap();
        let x_0_0_0 = f();
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}

|]
