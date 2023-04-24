{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.KVAppCompiled where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)


basicsPlaceholder :: SourceFile Span
basicsPlaceholder= [sourceFile|

pub fn placeholder(){
  todo!();
}

|]

kvApplication :: SourceFile Span
kvApplication = [sourceFile|

pub fn placeholder(){
  todo!();
}

|]

stateInBranches :: SourceFile Span
stateInBranches = [sourceFile|
use stuff::*;

pub fn main() -> Something {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (result_1_tx, mut result_1_rx) = channel();
  let (condition_0_0_0_0_tx, condition_0_0_0_0_rx) = channel();
  let (obj1_0_0_1_tx, obj1_0_0_1_rx) = channel();
  let (ctrlTrue_0_tx, ctrlTrue_0_rx) = channel();
  let (obj2_0_0_1_tx, obj2_0_0_1_rx) = channel();
  let (ctrlFalse_0_tx, ctrlFalse_0_rx) = channel();
  let (b_0_0_tx, b_0_0_rx) = channel();
  let (a_0_0_tx, a_0_0_rx) = channel();
  let (condition_0_0_0_1_tx, condition_0_0_0_1_rx) = channel();
  activity!(
    (
      |b_0_0_child_tx: Sender, obj2_0_0_1_child_tx: Receiver, ctrlFalse_0_child_tx: Receiver| {
        Ok(
          loop {
            let mut renew = false;
            let mut obj2_0_0_1_0 = obj2_0_0_1_child_rx.recv::<  OtherThing,>()?;
            while !renew {
              let sig = ctrlFalse_0_child_rx.recv::<  (bool, usize),>()?;
              let count = sig.1;
              for _ in 0 .. count {
                let b_0_0 = obj2_0_0_1_0.compute();
                b_0_0_child_tx.send(b_0_0)?;
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
    )(b_0_0_tx, obj2_0_0_1_rx, ctrlFalse_0_rx)
  );
  activity!(
    (
      |a_0_0_child_tx: Sender, obj1_0_0_1_child_tx: Receiver, ctrlTrue_0_child_tx: Receiver| {
        Ok(
          loop {
            let mut renew = false;
            let mut obj1_0_0_1_0 = obj1_0_0_1_child_rx.recv::<  Something,>()?;
            while !renew {
              let sig = ctrlTrue_0_child_rx.recv::<  (bool, usize),>()?;
              let count = sig.1;
              for _ in 0 .. count {
                let a_0_0 = obj1_0_0_1_0.calculate();
                a_0_0_child_tx.send(a_0_0)?;
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
    )(a_0_0_tx, obj1_0_0_1_rx, ctrlTrue_0_rx)
  );
  activity!(
    (
      |result_1_child_tx: Sender, result_1_child_tx: Sender, condition_0_0_0_1_child_tx: Receiver, a_0_0_child_tx: Receiver, b_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let branchSelection = condition_0_0_0_1_child_rx.recv::<  bool,>()?;
            if branchSelection {
              let result = a_0_0_child_rx.recv::<  Something,>()?;
              result_1_child_tx.send(result)?
            } else {
              let result = b_0_0_child_rx.recv::<  Something,>()?;
              result_1_child_tx.send(result)?
            }
          }
        )
      }
    )(result_1_tx, result_1_tx, condition_0_0_0_1_rx, a_0_0_rx, b_0_0_rx)
  );
  activity!(
    (
      |ctrlTrue_0_child_tx: Sender, ctrlFalse_0_child_tx: Sender, ctrlTrue_0_child_tx: Sender, ctrlFalse_0_child_tx: Sender, condition_0_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let branchSelection = condition_0_0_0_0_child_rx.recv::<  bool,>()?;
            if branchSelection {
              let ctrlTrue = (true, 1);
              let ctrlFalse = (true, 0);
              ctrlTrue_0_child_tx.send(ctrlTrue)?;
              ctrlFalse_0_child_tx.send(ctrlFalse)?
            } else {
              let ctrlTrue = (true, 0);
              let ctrlFalse = (true, 1);
              ctrlTrue_0_child_tx.send(ctrlTrue)?;
              ctrlFalse_0_child_tx.send(ctrlFalse)?
            }
          }
        )
      }
    )(
      ctrlTrue_0_tx,
      ctrlFalse_0_tx,
      ctrlTrue_0_tx,
      ctrlFalse_0_tx,
      condition_0_0_0_0_rx,
    )
  );
  activity!(
    (
      |condition_0_0_0_0_child_tx: Sender, condition_0_0_0_1_child_tx: Sender| {
        let res = function(23);
        condition_0_0_0_0_child_tx.send(res)?;
        condition_0_0_0_1_child_tx.send(res)?;
        Ok(())
      }
    )(condition_0_0_0_0_tx, condition_0_0_0_1_tx)
  );
  activity!(
    (
      |obj2_0_0_1_child_tx: Sender| {
        let obj2_0_0_1 = Some::thingelse();
        obj2_0_0_1_child_tx.send(obj2_0_0_1)?;
        Ok(())
      }
    )(obj2_0_0_1_tx)
  );
  activity!(
    (
      |obj1_0_0_1_child_tx: Sender| {
        let obj1_0_0_1 = Something::new();
        obj1_0_0_1_child_tx.send(obj1_0_0_1)?;
        Ok(())
      }
    )(obj1_0_0_1_tx)
  );
  result_1_rx.activate()?;
  result_1_rx
    .recv::<  Something,>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}
|]