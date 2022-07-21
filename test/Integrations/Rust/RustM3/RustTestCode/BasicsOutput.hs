{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustM3.RustTestCode.BasicsOutput where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

-- QUESTION: This code doesn't seem to be valid anymore.e.g. there is no new_child_vpe in VPE
-- REMINDER : Examples for gate use at https://github.com/TUD-OS/M3/blob/3c6acd4e7a462ef4d832e83cc17ed29998b2b484/src/apps/rust/unittests/src/tsgate.rs
-- QUESTION: Why do we use gates, not pipes?
hello_world :: SourceFile Span
hello_world = [sourceFile|
use funs::hello_world;

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        let a_0_0 = hello_world();
        a_0_0_tx.send_msg(a_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}
                |]

simple_composition :: SourceFile Span
simple_composition = [sourceFile|
                use funs::*;

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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
        let x_0_0_0 = f();
        x_0_0_0_tx.send_msg(x_0_0_0).unwrap();
        ()
      }))
      .unwrap()
  };
  a_0_0_rx.recv_msg::<  String,>().unwrap()
}
                |]

multi_var :: SourceFile Span
multi_var = [sourceFile|
use funs::*;

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (y_0_0_0_tx, y_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe.delegate_obj(y_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        y_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
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
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        y_0_0_0.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let y_0_0_0 = h(var_0);
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

multi_var_read_only :: SourceFile Span
multi_var_read_only =  [sourceFile|
use funs::*;

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x1_0_0_1_tx, x1_0_0_1_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x1_0_0_0_0_tx, x1_0_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (y_0_0_0_tx, y_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x2_0_0_0_tx, x2_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(a_0_0.sel()).unwrap();
    vpe.delegate_obj(x2_0_0_0.sel()).unwrap();
    vpe.delegate_obj(y_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        a_0_0.activate().unwrap();
        x2_0_0_0.activate().unwrap();
        y_0_0_0.activate().unwrap();
        loop {
          let var_0 = x2_0_0_0_rx.recv_msg::<  i32,>().unwrap();
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
    vpe.delegate_obj(x1_0_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        y_0_0_0.activate().unwrap();
        x1_0_0_0_0.activate().unwrap();
        loop {
          let var_0 = x1_0_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let y_0_0_0 = h(var_0);
          y_0_0_0_tx.send_msg(y_0_0_0).unwrap();
          ()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x2_0_0_0.sel()).unwrap();
    vpe.delegate_obj(x1_0_0_0_0.sel()).unwrap();
    vpe.delegate_obj(x1_0_0_1.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x2_0_0_0.activate().unwrap();
        x1_0_0_0_0.activate().unwrap();
        x1_0_0_1.activate().unwrap();
        loop {
          let var_0 = x1_0_0_1_rx.recv_msg::<  S,>().unwrap();
          let x2_0_0_0 = var_0.clone();
          x2_0_0_0_tx.send_msg(x2_0_0_0).unwrap();
          x1_0_0_0_0_tx.send_msg(var_0).unwrap()
        }
      }))
      .unwrap()
  };
  {
    let mut vpe = VPE::new_child_vpe().unwrap();
    vpe.delegate_obj(x1_0_0_1.sel()).unwrap();
    vpe.delegate_obj(x_0_0_0.sel()).unwrap();
    vpe
      .run(Box::new(move || -> _ {
        x1_0_0_1.activate().unwrap();
        x_0_0_0.activate().unwrap();
        loop {
          let var_0 = x_0_0_0_rx.recv_msg::<  i32,>().unwrap();
          let x1_0_0_1 = std::sync::Arc::new(var_0);
          x1_0_0_1_tx.send_msg(x1_0_0_1).unwrap();
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

multi_var_expl_clone :: SourceFile Span
multi_var_expl_clone = [sourceFile|
use funs::*;

fn test() -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_1_tx, x_0_0_1_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_0_tx, x_0_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (y_0_0_0_tx, y_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x1_0_0_0_tx, x1_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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
          let var_0 = x_0_0_1_rx.recv_msg::<  S,>().unwrap();
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
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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
use funs::*;

fn algo(i: i32) -> String {
  let (a_0_0_tx, a_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x_0_0_0_tx, x_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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
use funs::*;

fn test() -> i32 {
  let (b_0_0_tx, b_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x0_0_0_0_tx, x0_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (y0_0_0_0_tx, y0_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (y1_0_0_0_tx, y1_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x1_0_0_0_tx, x1_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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

tuple_from_param :: SourceFile Span
tuple_from_param = [sourceFile|
use funs::*;

fn test(i: i32) -> i32 {
  let (b_0_0_tx, b_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x0_0_0_0_tx, x0_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (y0_0_0_0_tx, y0_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (y1_0_0_0_tx, y1_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
      (sgate, rgate)
    };
  let (x1_0_0_0_tx, x1_0_0_0_rx) =
    {
      let mut rgate =
        wv_assert_ok!(
          RecvGate::new(math::next_log2(256), math::next_log2(256))
        );
      let sgate =
        wv_assert_ok!(SendGate::new_with(SGateArgs::new(&rgate).credits(1)));
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
        let res = fi_tup(i);
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

          
