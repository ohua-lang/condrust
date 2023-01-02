{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.KVAppCode where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)


k_v_application_1_2_components:: SourceFile Span
k_v_application_1_2_components = [sourceFile|
use std::os::unix::io::RawFd;
use std::time::Instant;



fn app_iface_re(
    mut app:App, 
    mut ip_stack: Interface,
    mut device:TunTapDevice, 
    mut sockets: SocketSet) -> ()
{
    let mut local_app:App = id(app);
    let mut local_ip_stack: Interface = id(ip_stack);
    let timestamp:Instant = Instant::now();
    let (poll_res, device_poll, sockets_poll):(Result<bool, Err>, TunTapDevice, SocketSet) =
        local_ip_stack.poll_wrapper(timestamp, device, sockets);

    let sockets_do_app_stuff:  SocketSet = local_app.do_app_stuff(sockets_poll, poll_res);
    let pointless_var:() = pointless_function();
    if allways_true() {
        app_iface_rec(app, ip_stack, device_poll, sockets_do_app_stuff)
    } else {
        pointless_var
    }
}

fn main() -> () {
    let (mut app, mut sockets):(App, SocketSet) = init_app_and_sockets();
    let (mut ip_stack, mut device, fd): (Interface, TunTapDevice, RawFd) = init_stack_and_device();
    app_iface_rec(app, ip_stack, device, sockets)
}

|]

just_rec = [sourceFile|
use funs::*;

fn rec(one: i32, s:String, f:FunkyType, n:NeverMind) -> () {
    let k:() = h2();
    let (dunno, one_new):(SomeType, i32) = s.string_mangel(one, n);
    let n_new:NeverMind = f.funky_ride(dunno);
    if check() {
        rec(one_new, s, f, n_new)
    } else {
        k
    }
}

fn test() -> () {
    let (s, f):(String,FunkyType)  = say_oisann();
    let n:NeverMind = never_mind();
    rec(42, s, f, n)
}
|]

k_v_latest =  [sourceFile|
mod ohua_util;
use ohua_util::init_components::{App, init_app, init_stack_and_device};
use smoltcp::{Either};
use smoltcp::iface::{Interface, InterfaceCall, Messages, SocketHandle};
use smoltcp::phy::{Device,TunTapInterface};

fn main() {

    let (mut ip_stack, handels, mut device):(Interface, Vec<SocketHandle>, TunTapInterface) = init_stack_and_device();
    let mut app: App = init_app(handels);
    let mut iface_call: InterfaceCall = InterfaceCall::InitPoll;

    app_iface_rec(app, ip_stack, device, iface_call)

}

fn app_iface_rec(
    mut app: App,
    mut ip_stack: Interface,
    mut device: TunTapInterface,
    mut if_call: InterfaceCall,
    ) -> () {

    let device_or_app_call: Either<DeviceCall, (bool, Messages)> = ip_stack.process_call::<TunTapInterface>(if_call);
    // at this point we know it's a device_call
    let dev_call: Option<DeviceCall> = as_some_call(device_or_app_call);
    let (app_call, ip_stack_n, device_n): (Option<AppCall>, Interface, TunTapInterface)
        = iface_device_rec(ip_stack, device, dev_call);
    let nothing: () = dummy();
    let answers: Messages = app.do_app_stuff(app_call);
    let if_call_new: InterfaceCall = InterfaceCall::AnswerToSocket(answers);
    if should_continue() {
        app_iface_rec(app, ip_stack_n, device_n, if_call_new)
    } else {
        nothing
    }
}

fn iface_device_rec(
    mut ip_stack:Interface,
    mut device: TunTapInterface,
    mut dev_call:Option<DeviceCall>
) -> (Option<AppCall>, Interface, TunTapInterface) {
    let call: Either<InterfaceCall, (Option<smolDuration>, bool)> = device.process_call(dev_call);
    let iface_call: InterfaceCall = maybe_wait(call);
    let device_or_app_call: Either<DeviceCall, (bool, Messages)> = ip_stack.process_call::<TunTapInterface>(iface_call);
    let (sign, optn_dev_call, optn_app_call): (bool, Option<DeviceCall>, Option<AppCall>) = unwrap_call(device_or_app_call);
    if sign{
        iface_device_rec(ip_stack, device, optn_dev_call)
    } else {
        (optn_app_call, ip_stack, device)
    }
}

|]


if_else_in_rec = [sourceFile|
use funs::*;

fn rec(one: i32) -> () {
    let i:i32 = h(one);
    let obj:Object = SomeObject();
    let k:() = if check(i) {obj.h2(i)} else {obj.h3(i)};
    if check(k) {
        rec(i)
    } else {
        k
    }
}

fn test() -> () {
    rec(42)
}
|]
