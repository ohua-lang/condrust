{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.KVAppCompiled where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

k_v_application_1_2_components :: SourceFile Span
k_v_application_1_2_components = [sourceFile|
use std::os::unix::io::RawFd;

use std::time::Instant;

fn main() -> () {
  use m3::com::channel::{Sender, Receiver};
  use m3::activity;
  let (h_0_0_tx, mut h_0_0_rx) = channel();
  let (device_and_fd_0_0_0_tx, device_and_fd_0_0_0_rx) = channel();
  let (f_0_0_tx, f_0_0_rx) = channel();
  let (sockets_do_app_stuff_0_0_0_tx, sockets_do_app_stuff_0_0_0_rx) =
    channel();
  let (device_poll_0_0_0_tx, device_poll_0_0_0_rx) = channel();
  let (ip_stack_0_0_0_0_tx, ip_stack_0_0_0_0_rx) = channel();
  let (app_0_0_0_0_tx, app_0_0_0_0_rx) = channel();
  let (sockets_1_0_0_tx, sockets_1_0_0_rx) = channel();
  let (device_1_0_0_tx, device_1_0_0_rx) = channel();
  let (ip_stack_1_0_0_tx, ip_stack_1_0_0_rx) = channel();
  let (app_1_0_0_tx, app_1_0_0_rx) = channel();
  let (ctrl_0_0_0_tx, ctrl_0_0_0_rx) = channel();
  let (sockets_0_0_0_tx, sockets_0_0_0_rx) = channel();
  let (device_0_0_0_tx, device_0_0_0_rx) = channel();
  let (timestamp_0_0_0_tx, timestamp_0_0_0_rx) = channel();
  let (ip_stack_0_0_1_tx, ip_stack_0_0_1_rx) = channel();
  let (needlessly_combined_tuple_0_0_0_tx, needlessly_combined_tuple_0_0_0_rx) =
    channel();
  let (poll_res_0_0_0_tx, poll_res_0_0_0_rx) = channel();
  let (sockets_poll_0_0_0_tx, sockets_poll_0_0_0_rx) = channel();
  let (app_0_0_1_tx, app_0_0_1_rx) = channel();
  let (should_continue_0_0_0_tx, should_continue_0_0_0_rx) = channel();
  activity!(
    (
      |timestamp_0_0_0_child_tx: Sender, ctrl_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let mut renew = false;
            while !renew {
              let sig = ctrl_0_0_0_child_rx.recv::<  (bool, usize),>()?;
              let count = sig.1;
              for _ in 0 .. count {
              let timestamp_0_0_0 = Instant::now();
                timestamp_0_0_0_child_tx.send(timestamp_0_0_0)?;
                ()
              };
              let renew_next_time = sig.0;
              renew = renew_next_time;
              ()
            }
          }
        )
      }
    )(timestamp_0_0_0_tx, ctrl_0_0_0_rx)
  );
  activity!(
    (
      |f_0_0_child_tx: Sender, should_continue_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 = should_continue_0_0_0_child_rx.recv::<  (),>()?;
            let f_0_0 = is_not_unit(var_0);
            f_0_0_child_tx.send(f_0_0)?;
            ()
          }
        )
      }
    )(f_0_0_tx, should_continue_0_0_0_rx)
  );
  activity!(
    (
      |should_continue_0_0_0_child_tx: Sender, sockets_do_app_stuff_0_0_0_child_tx: Sender, app_0_0_0_0_child_tx: Sender, app_0_0_1_child_tx: Receiver, sockets_poll_0_0_0_child_tx: Receiver, poll_res_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let mut var_0 = app_0_0_1_child_rx.recv::<  App,>()?;
            let var_1 = sockets_poll_0_0_0_child_rx.recv::<  SocketSet,>()?;
            let var_2 = poll_res_0_0_0_child_rx.recv::<  Result<bool, Err>,>(

            )?;
            let res = var_0.do_app_stuff(var_1, var_2);
            let should_continue_0_0_0 = res.0;
            should_continue_0_0_0_child_tx.send(should_continue_0_0_0)?;
            let sockets_do_app_stuff_0_0_0 = res.1;
            sockets_do_app_stuff_0_0_0_child_tx.send(
            sockets_do_app_stuff_0_0_0
            )?;
            app_0_0_0_0_child_tx.send(var_0)?
          }
        )
      }
    )(
      should_continue_0_0_0_tx,
      sockets_do_app_stuff_0_0_0_tx,
      app_0_0_0_0_tx,
      app_0_0_1_rx,
      sockets_poll_0_0_0_rx,
      poll_res_0_0_0_rx,
    )
  );
  activity!(
    (
      |poll_res_0_0_0_child_tx: Sender, device_poll_0_0_0_child_tx: Sender, needlessly_combined_tuple_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 =
            needlessly_combined_tuple_0_0_0_child_rx
            .recv::<  (Result<bool, Err>, TunTapDevice),>()?;
            let res = destruct(var_0);
            let poll_res_0_0_0 = res.0;
            poll_res_0_0_0_child_tx.send(poll_res_0_0_0)?;
            let device_poll_0_0_0 = res.1;
            device_poll_0_0_0_child_tx.send(device_poll_0_0_0)?;
            ()
          }
        )
      }
    )(
      poll_res_0_0_0_tx, device_poll_0_0_0_tx, needlessly_combined_tuple_0_0_0_rx
    )
  );
  activity!(
    (
      |needlessly_combined_tuple_0_0_0_child_tx: Sender, sockets_poll_0_0_0_child_tx: Sender, ip_stack_0_0_0_0_child_tx: Sender, ip_stack_0_0_1_child_tx: Receiver, timestamp_0_0_0_child_tx: Receiver, device_0_0_0_child_tx: Receiver, sockets_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let mut var_0 = ip_stack_0_0_1_child_rx.recv::<  OInterface,>()?;
            let var_1 = timestamp_0_0_0_child_rx.recv::<  Instant,>()?;
            let var_2 = device_0_0_0_child_rx.recv::<  TunTapDevice,>()?;
            let var_3 = sockets_0_0_0_child_rx.recv::<  SocketSet,>()?;
            let res = var_0.poll_wrapper(var_1, var_2, var_3);
            let needlessly_combined_tuple_0_0_0 = res.0;
            needlessly_combined_tuple_0_0_0_child_tx
            .send(needlessly_combined_tuple_0_0_0)?;
            let sockets_poll_0_0_0 = res.1;
            sockets_poll_0_0_0_child_tx.send(sockets_poll_0_0_0)?;
            ip_stack_0_0_0_0_child_tx.send(var_0)?
          }
        )
      }
    )(
      needlessly_combined_tuple_0_0_0_tx,
      sockets_poll_0_0_0_tx,
      ip_stack_0_0_0_0_tx,
      ip_stack_0_0_1_rx,
      timestamp_0_0_0_rx,
      device_0_0_0_rx,
      sockets_0_0_0_rx,
    )
  );
  activity!(
    (
      |ctrl_0_0_0_child_tx: Sender, app_0_0_1_child_tx: Sender, ip_stack_0_0_1_child_tx: Sender, device_0_0_0_child_tx: Sender, sockets_0_0_0_child_tx: Sender, ctrl_0_0_0_child_tx: Sender, app_0_0_1_child_tx: Sender, ip_stack_0_0_1_child_tx: Sender, device_0_0_0_child_tx: Sender, sockets_0_0_0_child_tx: Sender, ctrl_0_0_0_child_tx: Sender, h_0_0_child_tx: Sender, app_1_0_0_child_tx: Receiver, ip_stack_1_0_0_child_tx: Receiver, device_1_0_0_child_tx: Receiver, sockets_1_0_0_child_tx: Receiver, f_0_0_child_tx: Receiver, should_continue_0_0_0_child_tx: Receiver, app_0_0_0_0_child_tx: Receiver, ip_stack_0_0_0_0_child_tx: Receiver, device_poll_0_0_0_child_tx: Receiver, sockets_do_app_stuff_0_0_0_child_tx: Receiver, app_0_0_0_0_child_tx: Receiver, ip_stack_0_0_0_0_child_tx: Receiver, device_poll_0_0_0_child_tx: Receiver, sockets_do_app_stuff_0_0_0_child_tx: Receiver, should_continue_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let ctrlSig = (true, 1);
            ctrl_0_0_0_child_tx.send(ctrlSig)?;
            let init_0 = app_1_0_0_child_rx.recv::<  App,>()?;
            let init_1 = ip_stack_1_0_0_child_rx.recv::<  OInterface,>()?;
            let init_2 = device_1_0_0_child_rx.recv::<  TunTapDevice,>()?;
            let init_3 = sockets_1_0_0_child_rx.recv::<  SocketSet,>()?;
            app_0_0_1_child_tx.send(init_0)?;
            ip_stack_0_0_1_child_tx.send(init_1)?;
            device_0_0_0_child_tx.send(init_2)?;
            sockets_0_0_0_child_tx.send(init_3)?;
            while f_0_0_child_rx.recv::<  bool,>()? {
              should_continue_0_0_0_child_rx.recv::<  (),>()?;
              let ctrlSig = (true, 1);
              ctrl_0_0_0_child_tx.send(ctrlSig)?;
              let loop_res_0 = app_0_0_0_0_child_rx.recv::<  App,>()?;
              let loop_res_1 = ip_stack_0_0_0_0_child_rx.recv::<  OInterface,>()?;
              let loop_res_2 = device_poll_0_0_0_child_rx.recv::<  TunTapDevice,>()?;
              let loop_res_3 =
              sockets_do_app_stuff_0_0_0_child_rx.recv::<  SocketSet,>()?;
              app_0_0_1_child_tx.send(loop_res_0)?;
              ip_stack_0_0_1_child_tx.send(loop_res_1)?;
              device_0_0_0_child_tx.send(loop_res_2)?;
              sockets_0_0_0_child_tx.send(loop_res_3)?;
              ()
            };
            let ctrlSig = (false, 0);
            ctrl_0_0_0_child_tx.send(ctrlSig)?;
            app_0_0_0_0_child_rx.recv::<  App,>()?;
            ip_stack_0_0_0_0_child_rx.recv::<  OInterface,>()?;
            device_poll_0_0_0_child_rx.recv::<  TunTapDevice,>()?;
            sockets_do_app_stuff_0_0_0_child_rx.recv::<  SocketSet,>()?;
            let finalResult = should_continue_0_0_0_child_rx.recv::<  (),>()?;
            h_0_0_child_tx.send(finalResult)?
          }
        )
      }
    )(
      ctrl_0_0_0_tx,
      app_0_0_1_tx,
      ip_stack_0_0_1_tx,
      device_0_0_0_tx,
      sockets_0_0_0_tx,
      ctrl_0_0_0_tx,
      app_0_0_1_tx,
      ip_stack_0_0_1_tx,
      device_0_0_0_tx,
      sockets_0_0_0_tx,
      ctrl_0_0_0_tx,
      h_0_0_tx,
      app_1_0_0_rx,
      ip_stack_1_0_0_rx,
      device_1_0_0_rx,
      sockets_1_0_0_rx,
      f_0_0_rx,
      should_continue_0_0_0_rx,
      app_0_0_0_0_rx,
      ip_stack_0_0_0_0_rx,
      device_poll_0_0_0_rx,
      sockets_do_app_stuff_0_0_0_rx,
      app_0_0_0_0_rx,
      ip_stack_0_0_0_0_rx,
      device_poll_0_0_0_rx,
      sockets_do_app_stuff_0_0_0_rx,
      should_continue_0_0_0_rx,
    )
  );
  activity!(
    (
      |device_1_0_0_child_tx: Sender, device_and_fd_0_0_0_child_tx: Receiver| {
        Ok(
          loop {
            let var_0 =
            device_and_fd_0_0_0_child_rx.recv::<  (TunTapDevice, RawFd),>()?;
            let device_1_0_0 = destruct(var_0);
            device_1_0_0_child_tx.send(device_1_0_0)?;
            ()
          }
        )
      }
    )(device_1_0_0_tx, device_and_fd_0_0_0_rx)
  );
  activity!(
    (
      |ip_stack_1_0_0_child_tx: Sender, device_and_fd_0_0_0_child_tx: Sender| {
        let res = init_stack_and_device();
        let ip_stack_1_0_0 = res.0;
        ip_stack_1_0_0_child_tx.send(ip_stack_1_0_0)?;
        let device_and_fd_0_0_0 = res.1;
        device_and_fd_0_0_0_child_tx.send(device_and_fd_0_0_0)?;
        Ok(())
      }
    )(ip_stack_1_0_0_tx, device_and_fd_0_0_0_tx)
  );
  activity!(
    (
      |app_1_0_0_child_tx: Sender, sockets_1_0_0_child_tx: Sender| {
        let res = init_app_and_sockets();
        let app_1_0_0 = res.0;
        app_1_0_0_child_tx.send(app_1_0_0)?;
        let sockets_1_0_0 = res.1;
        sockets_1_0_0_child_tx.send(sockets_1_0_0)?;
        Ok(())
      }
    )(app_1_0_0_tx, sockets_1_0_0_tx)
  );
  h_0_0_rx.activate()?;
  h_0_0_rx
    .recv::<  (),>()
    .expect("The retrieval of the result value failed.
Ohua turned your sequential program into a distributed one.
Hence, all Ohua can do at this point is error out.
If you would like to have support for handligng these errors in your application then please submit an issue.
")
}

fn loop_as_rec(
  mut app: App,
  mut ip_stack: OInterface,
  mut device: TunTapDevice,
  mut sockets: SocketSet,
) -> () {
  let timestamp: Instant = Instant::now();
  let (needlessly_combined_tuple, sockets_poll):
    ((Result<bool, Err>, TunTapDevice), SocketSet) =
    ip_stack.poll_wrapper(timestamp, device, sockets);
  let (poll_res, device_poll): (Result<bool, Err>, TunTapDevice) =
    destruct(needlessly_combined_tuple);
  let (should_continue, sockets_do_app_stuff): ((), SocketSet) =
    app.do_app_stuff(sockets_poll, poll_res);
  if is_not_unit(should_continue) {
    loop_as_rec(app, ip_stack, device_poll, sockets_do_app_stuff)
  } else { should_continue }
}
|]

k_v_application_2_poll_loop_rec :: SourceFile Span
k_v_application_2_poll_loop_rec = [sourceFile|
use M3;

fn nothing_here() -> bool {
  return false;
}

|]

k_v_latest:: SourceFile Span
k_v_latest = [sourceFile|
use M3;

fn nothing_here() -> bool {
  return false;
}

|]