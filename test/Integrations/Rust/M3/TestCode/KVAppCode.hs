{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.KVAppCode where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)


k_v_application_1_2_components:: SourceFile Span
k_v_application_1_2_components = [sourceFile|
use std::os::unix::io::RawFd;
use std::time::Instant;

fn main() -> () {
    let (mut app, mut sockets):(App, SocketSet) = init_app_and_sockets();
    let (mut ip_stack, mut device_and_fd): (OInterface, (TunTapDevice, RawFd)) = init_stack_and_device();
    let mut device: TunTapDevice = destruct(device_and_fd);
    loop_as_rec(app, ip_stack, device, sockets)
}


fn loop_as_rec(
    mut app:App, mut ip_stack: OInterface,
    mut device:TunTapDevice, mut sockets: SocketSet) -> ()
{
    let timestamp:Instant = Instant::now();
    let (needlessly_combined_tuple, sockets_poll):((Result<bool, Err>, TunTapDevice), SocketSet) =
        ip_stack.poll_wrapper(timestamp, device, sockets);
    let (poll_res, device_poll):(Result<bool, Err>, TunTapDevice) = destruct(needlessly_combined_tuple);
    let (should_continue, sockets_do_app_stuff): ((), SocketSet) = app.do_app_stuff(sockets_poll, poll_res);

    if is_not_unit(should_continue) {
        loop_as_rec(app, ip_stack, device_poll, sockets_do_app_stuff)
    } else {should_continue}
}
|]

-- ToDo: These examples realy show we should enhance Ohuas destruct feature. Its just not acceptable to have to write this manually

-- | Lets pretend we allready had made poll a function and refactored its outer loop 
--   to a recursion. It would look like this:
k_v_application_2_poll_loop_rec:: SourceFile Span
k_v_application_2_poll_loop_rec = [sourceFile|
use std::os::unix::io::RawFd;
use std::time::Instant;

fn main() -> () {
    let (mut app, mut sockets):(App, SocketSet) = init_app_and_sockets();
    let (mut ip_stack, mut device_and_fd): (Interface, (TunTapDevice, RawFd)) = init_stack_and_device();
    let mut device: TunTapDevice = destruct(device_and_fd);
    loop_as_rec(app, ip_stack, device, sockets)
}


fn loop_as_rec(
    mut app:App, 
    mut ip_stack: Interface,
    mut device: TunTapDevice, 
    mut sockets: SocketSet) -> ()
{
    let timestamp:Instant = Instant::now();
    let (needlessly_combined_tuple, sockets_poll): (Result<bool, Err>, (Interface, TunTapDevice), SocketSet) =
        poll(ip_stack, timestamp, device, sockets);

    let (poll_res, device_and_interface) :(Result<bool, Err>, (Interface, TunTapDevice)) = destruct(needlessly_combined_tuple);
    let (ip_stack_poll, device_poll):(Interface, TunTapDevice) =  destruct(device_and_interface);
    let (should_continue, sockets_do_app_stuff): ((), SocketSet) = app.do_app_stuff(sockets_poll, poll_res);

    if is_not_unit(should_continue) {
        loop_as_rec(app, ip_stack_poll, device_poll, sockets_do_app_stuff)
    } else {should_continue}
}

fn poll<D>(
    ip_stack:Interface, 
    timestamp:Instant, 
    device: D,
    sockets: SocketSet
    ) -> (Result<bool, Err>, (TunTapDevice, Interface), SocketSet) 
{
    // We can't do this right now, needs to be fused later
    // ip_stack.inner.now = timestamp;  

    //  tuple_of_results : (Result<bool, Err>, (TunTapDevice, Interface), SocketSet)
    let tuple_of_results: (Result<bool, Err>, (TunTapDevice, Interface), SocketSet) = poll_inner_rec(false, ip_stack, device, sockets);
    tuple_of_results
}

fn poll_inner_rec<D>(
    has_changed_before: bool,
    ip_stack: Interface,
    device: D,
    sockets: SocketSet
    ) -> (Result<bool, Err>, (TunTapDevice, Interface), SocketSet) 
{
    let has_changed: bool = ip_stack.egress_and_ingress(device, sockets);
    let needlessly_wrapped:(Result<bool, Err>, (TunTapDevice, Interface), SocketSet) 
            = wrap(has_changed, ip_stack, device, sockets);
    if has_changed {
        poll_inner_rec(has_changed, ip_stack, device, sockets)
    } else {
        needlessly_wrapped
    }

}
|]
