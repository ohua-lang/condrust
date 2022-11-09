{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.KVAppCompiled where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

k_v_application :: SourceFile Span
k_v_application = [sourceFile|
use std::os::unix::io::RawFd;
use std::time::Instant;
fn main() -> () {
  let (h_0_0_tx, h_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (device_and_fd_0_0_0_tx, device_and_fd_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (f_0_0_tx, f_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (sockets_do_app_stuff_0_0_0_tx, sockets_do_app_stuff_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (device_poll_0_0_0_tx, device_poll_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (ip_stack_0_0_0_0_tx, ip_stack_0_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (app_0_0_0_0_tx, app_0_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (sockets_1_0_0_tx, sockets_1_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (device_1_0_0_tx, device_1_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (ip_stack_1_0_0_tx, ip_stack_1_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (app_1_0_0_tx, app_1_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (ctrl_0_0_0_tx, ctrl_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (sockets_0_0_0_tx, sockets_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (device_0_0_0_tx, device_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (timestamp_0_0_0_tx, timestamp_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (ip_stack_0_0_1_tx, ip_stack_0_0_1_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (needlessly_combined_tuple_0_0_0_tx, needlessly_combined_tuple_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (poll_res_0_0_0_tx, poll_res_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (sockets_poll_0_0_0_tx, sockets_poll_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (app_0_0_1_tx, app_0_0_1_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (should_continue_0_0_0_tx, should_continue_0_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(timestamp_0_0_0.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        timestamp_0_0_0.activate().unwrap();
        ctrl_0_0_0.activate().unwrap();
        loop {
          let mut renew = false;
          while !renew {
            let sig = ctrl_0_0_0_rx.recv_msg::<  (bool, usize),>().unwrap();
            let count = sig.1;
            for _ in 0 .. count {
              let timestamp_0_0_0 = Instant::now();
              timestamp_0_0_0_tx.send_msg(timestamp_0_0_0).unwrap();
              ()
            };
            let renew_next_time = sig.0;
            renew = renew_next_time;
            ()
          }
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(f_0_0.sel()).unwrap();
    vpe.delegate_obj(should_continue_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        f_0_0.activate().unwrap();
        should_continue_0_0_0.activate().unwrap();
        loop {
          let var_0 = should_continue_0_0_0_rx.recv_msg::<  (),>().unwrap();
          let f_0_0 = is_not_unit(var_0);
          f_0_0_tx.send_msg(f_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(should_continue_0_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_do_app_stuff_0_0_0.sel()).unwrap();
    vpe.delegate_obj(app_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(app_0_0_1.sel()).unwrap();
    vpe.delegate_obj(sockets_poll_0_0_0.sel()).unwrap();
    vpe.delegate_obj(poll_res_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        should_continue_0_0_0.activate().unwrap();
        sockets_do_app_stuff_0_0_0.activate().unwrap();
        app_0_0_0_0.activate().unwrap();
        app_0_0_1.activate().unwrap();
        sockets_poll_0_0_0.activate().unwrap();
        poll_res_0_0_0.activate().unwrap();
        loop {
          let mut var_0 = app_0_0_1_rx.recv_msg::<  App,>().unwrap();
          let var_1 = sockets_poll_0_0_0_rx.recv_msg::<  SocketSet,>().unwrap();
          let var_2 =
            poll_res_0_0_0_rx.recv_msg::<  Result<bool, Err>,>().unwrap();
          let res = var_0.do_app_stuff(var_1, var_2);
          let should_continue_0_0_0 = res.0;
          should_continue_0_0_0_tx.send_msg(should_continue_0_0_0).unwrap();
          let sockets_do_app_stuff_0_0_0 = res.1;
          sockets_do_app_stuff_0_0_0_tx
            .send_msg(sockets_do_app_stuff_0_0_0)
            .unwrap();
          app_0_0_0_0_tx.send_msg(var_0).unwrap()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(poll_res_0_0_0.sel()).unwrap();
    vpe.delegate_obj(device_poll_0_0_0.sel()).unwrap();
    vpe.delegate_obj(needlessly_combined_tuple_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        poll_res_0_0_0.activate().unwrap();
        device_poll_0_0_0.activate().unwrap();
        needlessly_combined_tuple_0_0_0.activate().unwrap();
        loop {
          let var_0 =
            needlessly_combined_tuple_0_0_0_rx
              .recv_msg::<  (Result<bool, Err>, TunTapInterface),>()
              .unwrap();
          let res = destruct(var_0);
          let poll_res_0_0_0 = res.0;
          poll_res_0_0_0_tx.send_msg(poll_res_0_0_0).unwrap();
          let device_poll_0_0_0 = res.1;
          device_poll_0_0_0_tx.send_msg(device_poll_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(needlessly_combined_tuple_0_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_poll_0_0_0.sel()).unwrap();
    vpe.delegate_obj(ip_stack_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(ip_stack_0_0_1.sel()).unwrap();
    vpe.delegate_obj(timestamp_0_0_0.sel()).unwrap();
    vpe.delegate_obj(device_0_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        needlessly_combined_tuple_0_0_0.activate().unwrap();
        sockets_poll_0_0_0.activate().unwrap();
        ip_stack_0_0_0_0.activate().unwrap();
        ip_stack_0_0_1.activate().unwrap();
        timestamp_0_0_0.activate().unwrap();
        device_0_0_0.activate().unwrap();
        sockets_0_0_0.activate().unwrap();
        loop {
          let mut var_0 =
            ip_stack_0_0_1_rx.recv_msg::<  OInterface,>().unwrap();
          let var_1 = timestamp_0_0_0_rx.recv_msg::<  Instant,>().unwrap();
          let var_2 = device_0_0_0_rx.recv_msg::<  TunTapInterface,>().unwrap();
          let var_3 = sockets_0_0_0_rx.recv_msg::<  SocketSet,>().unwrap();
          let res = var_0.poll_wrapper(var_1, var_2, var_3);
          let needlessly_combined_tuple_0_0_0 = res.0;
          needlessly_combined_tuple_0_0_0_tx
            .send_msg(needlessly_combined_tuple_0_0_0)
            .unwrap();
          let sockets_poll_0_0_0 = res.1;
          sockets_poll_0_0_0_tx.send_msg(sockets_poll_0_0_0).unwrap();
          ip_stack_0_0_0_0_tx.send_msg(var_0).unwrap()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(ctrl_0_0_0.sel()).unwrap();
    vpe.delegate_obj(app_0_0_1.sel()).unwrap();
    vpe.delegate_obj(ip_stack_0_0_1.sel()).unwrap();
    vpe.delegate_obj(device_0_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_0_0_0.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_0_0.sel()).unwrap();
    vpe.delegate_obj(app_0_0_1.sel()).unwrap();
    vpe.delegate_obj(ip_stack_0_0_1.sel()).unwrap();
    vpe.delegate_obj(device_0_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_0_0_0.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_0_0.sel()).unwrap();
    vpe.delegate_obj(h_0_0.sel()).unwrap();
    vpe.delegate_obj(app_1_0_0.sel()).unwrap();
    vpe.delegate_obj(ip_stack_1_0_0.sel()).unwrap();
    vpe.delegate_obj(device_1_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_1_0_0.sel()).unwrap();
    vpe.delegate_obj(f_0_0.sel()).unwrap();
    vpe.delegate_obj(should_continue_0_0_0.sel()).unwrap();
    vpe.delegate_obj(app_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(ip_stack_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(device_poll_0_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_do_app_stuff_0_0_0.sel()).unwrap();
    vpe.delegate_obj(app_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(ip_stack_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(device_poll_0_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_do_app_stuff_0_0_0.sel()).unwrap();
    vpe.delegate_obj(should_continue_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        ctrl_0_0_0.activate().unwrap();
        app_0_0_1.activate().unwrap();
        ip_stack_0_0_1.activate().unwrap();
        device_0_0_0.activate().unwrap();
        sockets_0_0_0.activate().unwrap();
        ctrl_0_0_0.activate().unwrap();
        app_0_0_1.activate().unwrap();
        ip_stack_0_0_1.activate().unwrap();
        device_0_0_0.activate().unwrap();
        sockets_0_0_0.activate().unwrap();
        ctrl_0_0_0.activate().unwrap();
        h_0_0.activate().unwrap();
        app_1_0_0.activate().unwrap();
        ip_stack_1_0_0.activate().unwrap();
        device_1_0_0.activate().unwrap();
        sockets_1_0_0.activate().unwrap();
        f_0_0.activate().unwrap();
        should_continue_0_0_0.activate().unwrap();
        app_0_0_0_0.activate().unwrap();
        ip_stack_0_0_0_0.activate().unwrap();
        device_poll_0_0_0.activate().unwrap();
        sockets_do_app_stuff_0_0_0.activate().unwrap();
        app_0_0_0_0.activate().unwrap();
        ip_stack_0_0_0_0.activate().unwrap();
        device_poll_0_0_0.activate().unwrap();
        sockets_do_app_stuff_0_0_0.activate().unwrap();
        should_continue_0_0_0.activate().unwrap();
        loop {
          let ctrlSig = (true, 1);
          ctrl_0_0_0_tx.send_msg(ctrlSig).unwrap();
          let init_0 = app_1_0_0_rx.recv_msg::<  App,>().unwrap();
          let init_1 = ip_stack_1_0_0_rx.recv_msg::<  OInterface,>().unwrap();
          let init_2 =
            device_1_0_0_rx.recv_msg::<  TunTapInterface,>().unwrap();
          let init_3 = sockets_1_0_0_rx.recv_msg::<  SocketSet,>().unwrap();
          app_0_0_1_tx.send_msg(init_0).unwrap();
          ip_stack_0_0_1_tx.send_msg(init_1).unwrap();
          device_0_0_0_tx.send_msg(init_2).unwrap();
          sockets_0_0_0_tx.send_msg(init_3).unwrap();
          while f_0_0_rx.recv_msg::<  bool,>().unwrap() {
            should_continue_0_0_0_rx.recv_msg::<  (),>().unwrap();
            let ctrlSig = (true, 1);
            ctrl_0_0_0_tx.send_msg(ctrlSig).unwrap();
            let loop_res_0 = app_0_0_0_0_rx.recv_msg::<  App,>().unwrap();
            let loop_res_1 =
              ip_stack_0_0_0_0_rx.recv_msg::<  OInterface,>().unwrap();
            let loop_res_2 =
              device_poll_0_0_0_rx.recv_msg::<  TunTapInterface,>().unwrap();
            let loop_res_3 =
              sockets_do_app_stuff_0_0_0_rx.recv_msg::<  SocketSet,>().unwrap();
            app_0_0_1_tx.send_msg(loop_res_0).unwrap();
            ip_stack_0_0_1_tx.send_msg(loop_res_1).unwrap();
            device_0_0_0_tx.send_msg(loop_res_2).unwrap();
            sockets_0_0_0_tx.send_msg(loop_res_3).unwrap();
            ()
          };
          let ctrlSig = (false, 0);
          ctrl_0_0_0_tx.send_msg(ctrlSig).unwrap();
          app_0_0_0_0_rx.recv_msg::<  App,>().unwrap();
          ip_stack_0_0_0_0_rx.recv_msg::<  OInterface,>().unwrap();
          device_poll_0_0_0_rx.recv_msg::<  TunTapInterface,>().unwrap();
          sockets_do_app_stuff_0_0_0_rx.recv_msg::<  SocketSet,>().unwrap();
          let finalResult =
            should_continue_0_0_0_rx.recv_msg::<  (),>().unwrap();
          h_0_0_tx.send_msg(finalResult).unwrap()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(device_1_0_0.sel()).unwrap();
    vpe.delegate_obj(device_and_fd_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        device_1_0_0.activate().unwrap();
        device_and_fd_0_0_0.activate().unwrap();
        loop {
          let var_0 =
            device_and_fd_0_0_0_rx
              .recv_msg::<  (TunTapInterface, RawFd),>()
              .unwrap();
          let device_1_0_0 = destruct(var_0);
          device_1_0_0_tx.send_msg(device_1_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(ip_stack_1_0_0.sel()).unwrap();
    vpe.delegate_obj(device_and_fd_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        ip_stack_1_0_0.activate().unwrap();
        device_and_fd_0_0_0.activate().unwrap();
        let res = init_stack_and_device();
        let ip_stack_1_0_0 = res.0;
        ip_stack_1_0_0_tx.send_msg(ip_stack_1_0_0).unwrap();
        let device_and_fd_0_0_0 = res.1;
        device_and_fd_0_0_0_tx.send_msg(device_and_fd_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(app_1_0_0.sel()).unwrap();
    vpe.delegate_obj(sockets_1_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        app_1_0_0.activate().unwrap();
        sockets_1_0_0.activate().unwrap();
        let res = init_app_and_sockets();
        let app_1_0_0 = res.0;
        app_1_0_0_tx.send_msg(app_1_0_0).unwrap();
        let sockets_1_0_0 = res.1;
        sockets_1_0_0_tx.send_msg(sockets_1_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  h_0_0_rx.recv_msg::<  (),>().unwrap()
}

fn loop_as_rec(
  mut app: App,
  mut ip_stack: OInterface,
  mut device: TunTapInterface,
  mut sockets: SocketSet,
) -> () {
  let timestamp: Instant = Instant::now();
  let (needlessly_combined_tuple, sockets_poll):
    ((Result<bool, Err>, TunTapInterface), SocketSet) =
    ip_stack.poll_wrapper(timestamp, device, sockets);
  let (poll_res, device_poll): (Result<bool, Err>, TunTapInterface) =
    destruct(needlessly_combined_tuple);
  let (should_continue, sockets_do_app_stuff): ((), SocketSet) =
    app.do_app_stuff(sockets_poll, poll_res);
  if is_not_unit(should_continue) {
    loop_as_rec(app, ip_stack, device_poll, sockets_do_app_stuff)
  } else { should_continue }
}
|]
