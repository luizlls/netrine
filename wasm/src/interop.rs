use std::alloc::{alloc, dealloc, Layout};

#[unsafe(no_mangle)]
pub extern "C" fn allocate(size: usize, align: usize) -> *mut u8 {
    let layout = Layout::from_size_align(size, align).unwrap();
    unsafe { alloc(layout) }
}

#[unsafe(no_mangle)]
pub extern "C" fn deallocate(ptr: *mut u8, size: usize, align: usize) {
    let layout = Layout::from_size_align(size, align).unwrap();
    unsafe { dealloc(ptr, layout) }
}

#[repr(C)]
pub struct WasmSlice<T> {
    pub ptr: *mut T,
    pub len: usize,
}

pub type WasmSlicePointer<T> = *mut WasmSlice<T>;

impl WasmSlice<u8> {
    pub fn into_str<'a>(slice: WasmSlicePointer<u8>) -> &'a str {
        unsafe {
            let slice = &*slice;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(slice.ptr, slice.len))
        }
    }
}

#[repr(C)]
pub struct WasmResult {
    pub ptr: *mut u8,
    pub len: usize,
}

pub type WasmResultPointer = *mut WasmResult;

impl WasmResult {
    pub fn from_vec(vec: Vec<u8>, target: WasmResultPointer) {
        let boxed = vec.into_boxed_slice();
        unsafe {
            let target = &mut *target;
            target.len = boxed.len();
            target.ptr = Box::into_raw(boxed) as *mut u8;
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn deallocate_result(result: WasmResultPointer) {
    unsafe {
        let result = &*result;
        drop(Box::from_raw(std::slice::from_raw_parts_mut(result.ptr, result.len)));
    }
}
