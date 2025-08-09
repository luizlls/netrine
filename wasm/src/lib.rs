mod internal;
mod interop;

pub use crate::interop::*;

#[unsafe(no_mangle)]
pub extern "C" fn compile(source: WasmSlicePointer<u8>, target: WasmSlicePointer<u8>) {
    let source = WasmSlice::into_str(source);
    let result = internal::compile(source);
    WasmSlice::from_vec(result, target);
}
