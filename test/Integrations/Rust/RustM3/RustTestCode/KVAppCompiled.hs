{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustM3.RustTestCode.KVAppCompiled where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

kv_application :: SourceFile Span
kv_application = [sourceFile|
mod ohua_util;

use ohua_util::init_components::{init_app,init_device,init_tcp_ip_stack};
use smoltcp::phy::{Device};
use smoltcp::iface::composition::{poll};

fn main() {
    let mut app = init_app();
    let device = init_device();
    let (mut tcp_ip_stack, socket_handle) = init_tcp_ip_stack(device);
    // Contains: Actuall payload messages for the sockets i.e. like {handle:[msgs]}, maybe also socket state
    let socket_proxy_state = init_socket_proxy();

    loop {
        let states_n_data = poll(tcp_ip_stack, device, socket_proxy_state);
        let processed_data = app.do_your_thing(states_n_data);
        socket_proxy_state.update_all(processed_data) 
    }
}
|]