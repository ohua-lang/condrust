{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.KVAppCode where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)


-- | We currently apply the following simplifications in this case:
-- 1. imports are simpliefied to be one-level i.e. no dir::module::submodule::import
-- 2. macros are replaced by functions 
-- 3. we us no generic types 
kvApplication:: SourceFile Span
kvApplication = [sourceFile|

#![no_std]
#![allow(dead_code)]
#![allow(unused_imports)]


mod loop_lib;
mod driver;

#[macro_use]
extern crate ffi_opaque;

use m3::{OwnActivity,Semaphore};


use local_smoltcp::{
    Interface, 
    NeighborCache, 
    SocketSet, 
    InterfaceCall,
    InterfaceCallState,
    Device,
    DeviceCapabilities,
    DeviceCall,
    Time, 
    Instant,
    Either, 
    Result};

use crate::driver::*;
use loop_lib::{init_device, init_ip_stack, init_app, App};

const DEBUG: bool = true;

// Change 0: Comment out for now
/*
fn maybe_wait(call: Either<InterfaceCall, (Option<Time>, bool)>) -> InterfaceCall {
    let mut next_call: InterfaceCall = InterfaceCall::InitPoll;
    if Either::is_left(&call) {
        next_call = call.left_or_panic();
    } else {
        let (advised_waiting_timeout, device_needs_poll): (Option<Time>, bool) = call.right_or_panic();
        if !device_needs_poll {
            match advised_waiting_timeout {
                None => OwnActivity::sleep_for(Time::from_millis(1)).ok(),
                Some(t) => OwnActivity::sleep_for(t).ok(),
            };
        }
    }
    return next_call;
}
*/

#[no_mangle]
pub fn main() {

    let mut app: App = init_app();

    let (mut device, caps): (Device, DeviceCapabilities) = init_device();

    let mut ip_stack: Interface<'_> = init_ip_stack(caps);



    // Change 2: Can't have (re-)assignments -> need to explicitely make the current call a state
    // after all this is just making the semantik of (re-)assignment more obviouse so we 
    // should be able to handle this later
    let first_call: InterfaceCall = InterfaceCall::InitPoll;
    let mut currentCall: InterfaceCallState = InterfaceCallState::new(first_call);

    let mut semaphore_set: bool = false;

    // Change 0: Comment out for now
    //m3::vfs::VFS::mount("/", "m3fs", "m3fs").expect("Failed to mount root filesystem on smoltcp_server");

    loop {
        // Change 1: Can't use references -> need to return an extra flag for case distinction
        let call: InterfaceCall = currentCall.get();
        let (is_call, device_or_app_call) : (bool, Either< DeviceCall, (bool, Messages)>)  = ip_stack.process_call(call);
        if id(is_call) {
            // Change 3: No function calls as arguments, primarily because we need type information -> extra assignment
            let dev_call: DeviceCall = device_or_app_call.left_or_panic();
            let call: Either<InterfaceCall, (Option<Duration>, bool)> = device.process_call(dev_call);
            let ip_stack_call: InterfaceCall = maybe_wait(call);
            currentCall.set(ip_stack_call);
        } else {
            /* Chaneg 0: comment out for now
            // I put semophore condition here, because it's the path thats least frequently taken
            if !semaphore_set {
                let _sem_r: Result<()> = Semaphore::attach("net").unwrap().up();
                semaphore_set = true;
            }
            */
            let (readiness_has_changed, message): (bool, Messages) = device_or_app_call.right_or_panic();
            // Change 4: Ok(readiness_has_changed) is also a function -> only pass readiness_has_changed
            let answers: Messages = app.process_message(readiness_has_changed, message);
            let ip_stack_call: InterfaceCall = InterfaceCall::AnswerToSocket(answers);
            currentCall.set(ip_stack_call)
        }
    }
}
|]

-- Test version of the m3 device driver module
driverLib :: SourceFile Span 
driverLib = [sourceFile|


|]

-- Test version of the m3 os rust interface 
m3Lib :: SourceFile Span 
m3Lib = [sourceFile|

impl OwnActivity {
    pub fn sleep_for(t:Time) -> () {}
}

pub struct Semaphore {}
impl Semaphore {
    pub fn attach(s:String) -> Result<()> {}
}


|]

-- Test version of al code encapsulated from the original main server loop
loopLib :: SourceFile Span 
loopLib = [sourceFile|

pub fn init_app()-> App {}
pub fn init_device() -> (Device, DeviceCapabilities) {}
pub fn init_ip_stack(caps: Capabilities) -> Interface {}

pub struct App {}

|]

-- Test version of the smoltcp library
localSmoltcp :: SourceFile Span 
localSmoltcp = [sourceFile|


pub struct Device {}
pub struct DeviceCapabilities {}
pub struct Interface {}
impl Interface {
    pub fn process_call(call:InterfaceCall) -> (bool, Either<DeviceCall, (bool, Messages)>) {}
}
pub struct NeighborCache {}
pub struct SocketSet {}
pub enum InterfaceCall {
    InitPoll,
}

pub struct InterfaceCallState {}
impl InterfaceCallState {
    pub fn new(call:InterfaceCall) -> Self {}
    pub fn get(&self) -> InterfaceCall {}
    pub fn set(&mut self, call:InterfaceCall) -> () {} 
} 

pub struct Time {}
impl Time {
    pub fn from_millis(i:i32) -> Time {}
}

pub struct Instant {}

pub enum Either {}
impl Either {
    pub fn right_or_panic(&self) -> (Option<Time>, bool) {}
    pub fn left_or_panic(&self) -> InterfaceCall {}
}
pub enum Result {}
|]
