{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.WIPOutput where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)


scope:: SourceFile Span
scope = [sourceFile|
fn test() -> i32 {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate = RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_1_0_0_tx, x_1_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (s_0_0_1_tx, s_0_0_1_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate =SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
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
          let a_0_0 = h(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(s_0_0_1.sel()).unwrap();
    vpe.delegate_obj(x_1_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        s_0_0_1.activate().unwrap();
        x_1_0_0.activate().unwrap();
        loop {
          let var_0 = s_0_0_1_rx.recv_msg::<  State,>().unwrap();
          let var_1 = x_1_0_0_rx.recv_msg::<  String,>().unwrap();
          var_0.do_it(var_1);
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_1_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_1_0_0.activate().unwrap();
        let x_1_0_0 = g();
        x_1_0_0_tx.send_msg(x_1_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(s_0_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        s_0_0_1.activate().unwrap();
        let s_0_0_1 = State::new();
        s_0_0_1_tx.send_msg(s_0_0_1).unwrap();
        ()
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
  a_0_0_rx.recv_msg::<  i32,>().unwrap()
}
|]

scope_for:: SourceFile Span
scope_for = [sourceFile|
fn test() -> i32 {
  let (c_0_0_tx, c_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (s_0_0_1_tx, s_0_0_1_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (ctrl_0_0_tx, ctrl_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (ctrl_0_1_tx, ctrl_0_1_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_1_0_0_tx, x_1_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x_1_0_0.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x_1_0_0.activate().unwrap();
        ctrl_0_1.activate().unwrap();
        loop {
          let renew = false;
          while !renew {
            let sig = ctrl_0_1_rx.recv_msg::<  !,>().unwrap();
            let count = sig.1;
            for _ in 0 .. count {
              let x_1_0_0 = g();
              x_1_0_0_tx.send_msg(x_1_0_0).unwrap();
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
    vpe.delegate_obj(s_0_0_1.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_0.sel()).unwrap();
    vpe.delegate_obj(x_1_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        s_0_0_1.activate().unwrap();
        ctrl_0_0.activate().unwrap();
        x_1_0_0.activate().unwrap();
        loop {
          let renew = false;
          let s_0_0_1_0 = s_0_0_1_rx.recv_msg::<  State,>().unwrap();
          while !renew {
            let sig =
              ctrl_0_0_rx.recv_msg::<  (PlaceHolder, PlaceHolder),>().unwrap();
            let count = sig.1;
            for _ in 0 .. count {
              let var_1 = x_1_0_0_rx.recv_msg::<  String,>().unwrap();
              s_0_0_1_0.do_it(var_1);
              ()
            };
            let renew_next_time = sig.0;
            renew = renew_next_time;
            ()
          };
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(c_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        c_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let c_0_0 = h(var_0);
          c_0_0_tx.send_msg(c_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(ctrl_0_0.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_1.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_0.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_1.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_0.sel()).unwrap();
    vpe.delegate_obj(ctrl_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        ctrl_0_0.activate().unwrap();
        ctrl_0_1.activate().unwrap();
        ctrl_0_0.activate().unwrap();
        ctrl_0_1.activate().unwrap();
        ctrl_0_0.activate().unwrap();
        ctrl_0_1.activate().unwrap();
        let a_0_0 = iter();
        loop {
          let hasSize =
            {
              let tmp_has_size = a_0_0.iter().size_hint();
              tmp_has_size.1.is_some()
            };
          if hasSize {
            let size = a_0_0.len();
            let ctrl = (true, size);
            ctrl_0_0_tx.send_msg(ctrl).unwrap();
            let ctrl = (true, size);
            ctrl_0_1_tx.send_msg(ctrl).unwrap();
            ()
          } else {
            let size = 0;
            for d in a_0_0 {
              let ctrl = (false, 1);
              ctrl_0_0_tx.send_msg(ctrl).unwrap();
              let ctrl = (false, 1);
              ctrl_0_1_tx.send_msg(ctrl).unwrap();
              size = size + 1;
              ()
            };
            let ctrl = (true, 0);
            ctrl_0_0_tx.send_msg(ctrl).unwrap();
            let ctrl = (true, 0);
            ctrl_0_1_tx.send_msg(ctrl).unwrap();
            ()
          }
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(s_0_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        s_0_0_1.activate().unwrap();
        let s_0_0_1 = State::new();
        s_0_0_1_tx.send_msg(s_0_0_1).unwrap();
        ()
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
  c_0_0_rx.recv_msg::<  i32,>().unwrap()
}
|]


scope_const :: SourceFile Span
scope_const = [sourceFile|
const global: i32 = 9;

fn test() -> i32 {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (y_0_0_0_tx, y_0_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
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
          let var_0 = y_0_0_0_rx.recv_msg::<  String,>().unwrap();
          let a_0_0 = h(var_0);
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
          let y_0_0_0 = g(var_0);
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
        let x_0_0_0 = f(global);
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  i32,>().unwrap()
}

|]
scope_static :: SourceFile Span
scope_static = [sourceFile|
const global: i32 = 9;

static st_thing: String = String::from(" times hello");

fn test() -> i32 {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (as_string_0_0_1_tx, as_string_0_0_1_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (y_0_0_0_tx, y_0_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
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
          let var_0 = y_0_0_0_rx.recv_msg::<  String,>().unwrap();
          let a_0_0 = h(var_0);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(y_0_0_0.sel()).unwrap();
    vpe.delegate_obj(as_string_0_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        y_0_0_0.activate().unwrap();
        as_string_0_0_1.activate().unwrap();
        loop {
          let var_0 = as_string_0_0_1_rx.recv_msg::<  String,>().unwrap();
          let y_0_0_0 = var_0.push(st_thing);
          y_0_0_0_tx.send_msg(y_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(as_string_0_0_1.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        as_string_0_0_1.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let as_string_0_0_1 = to_string(var_0);
          as_string_0_0_1_tx.send_msg(as_string_0_0_1).unwrap();
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
        let x_0_0_0 = f(global);
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  i32,>().unwrap()
}

fn second_algo(arg: i32) -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  let (as_string_0_0_1_tx, as_string_0_0_1_rx) =
    {
      let mut rgate =
         RecvGate::new(math::next_log2(256), math::next_log2(256));
      let sgate = SendGate::new_with(SGateArgs::new(&rgate).credits(1));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(as_string_0_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        as_string_0_0_1.activate().unwrap();
        loop {
          let var_0 = as_string_0_0_1_rx.recv_msg::<  String,>().unwrap();
          let a_0_0 = var_0.push(st_thing);
          a_0_0_tx.send_msg(a_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(as_string_0_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        as_string_0_0_1.activate().unwrap();
        let as_string_0_0_1 = to_string(arg);
        as_string_0_0_1_tx.send_msg(as_string_0_0_1).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}
|]
