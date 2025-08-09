mod internal;
mod interop;

pub use crate::interop::*;

#[unsafe(no_mangle)]
pub extern "C" fn compile(source: WasmSlicePointer<u8>, result: WasmResultPointer) {
    let source = WasmSlice::into_str(source);
    let bytecode = internal::compile(source);
    WasmResult::from_vec(bytecode, result);
}
