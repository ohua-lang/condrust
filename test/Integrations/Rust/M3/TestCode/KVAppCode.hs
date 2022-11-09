{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.KVAppCode where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)


k_v_application:: SourceFile Span
k_v_application = [sourceFile|
use std::os::unix::io::RawFd;
use std::time::Instant;

fn main() -> () {
    let (mut app, mut sockets):(App, SocketSet) = init_app_and_sockets();
    let (mut ip_stack, mut device_and_fd): (OInterface, (TunTapInterface, RawFd)) = init_stack_and_device();
    let mut device: TunTapInterface = destruct(device_and_fd);
    loop_as_rec(app, ip_stack, device, sockets)
}


fn loop_as_rec(
    mut app:App, mut ip_stack: OInterface,
    mut device:TunTapInterface, mut sockets: SocketSet) -> ()
{
    let timestamp:Instant = Instant::now();
    let (needlessly_combined_tuple, sockets_poll):((Result<bool, Err>, TunTapInterface), SocketSet) =
        ip_stack.poll_wrapper(timestamp, device, sockets);
    let (poll_res, device_poll):(Result<bool, Err>, TunTapInterface) = destruct(needlessly_combined_tuple);
    let (should_continue, sockets_do_app_stuff): ((), SocketSet) = app.do_app_stuff(sockets_poll, poll_res);

    if is_not_unit(should_continue) {
        loop_as_rec(app, ip_stack, device_poll, sockets_do_app_stuff)
    } else {should_continue}
}
|]
