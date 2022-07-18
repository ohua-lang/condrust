{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustM3.RustTestCode.KVAppCode where

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

-- REMINDER: Refactor poll, do we need to do things like that? 
-- REMINDER: Does the device return something 'interesting' upon sending? 
--          (it might for instance return an error, that causes the sockets to try again)

-- New interface of the interface :-)
composition_code::SourceFile Span
composition_code = 
    [sourceFile|
use funs::*;

pub fn poll(tcp_ip_stack:TcpIPInterface, nic:Device, prev_state: StatesAndData) -> Result<StatesAndData> {
    // We need to 
    // 1. receive data from device
    // 2. pass received device data 'up' and app data 'down' to the tcp_ip_stack
    // ->  get back processed received packages and possibly new socket state, 
    // ->  plus packaged app data ready to be send
    // 3. pass the packaged data to the device
    // 4. return the processed received data and updated socket state
    let new_inbound = device.receive_all();
    let (new_outbound, new_state) = tcp_ip_stack.process(prev_state, new_inbound);
    //Not sure what the device may reply and if it's needed somewhere
    let result = device.send_all(new_outbound);
    new_state
}


|]
