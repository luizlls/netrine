use core::str;
use std::alloc;

#[unsafe(no_mangle)]
pub extern "C" fn alloc(size: usize, align: usize) -> *mut u8 {
    let layout = alloc::Layout::from_size_align(size, align).unwrap();
    unsafe { alloc::alloc(layout) }
}

#[unsafe(no_mangle)]
pub extern "C" fn dealloc(ptr: *mut u8, size: usize, align: usize) {
    let layout = alloc::Layout::from_size_align(size, align).unwrap();
    unsafe { alloc::dealloc(ptr, layout) }
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

impl<T> WasmSlice<T> {
    pub fn from_vec(vec: Vec<T>, target: WasmSlicePointer<T>) {
        let boxed = vec.into_boxed_slice();
        unsafe {
            let target = &mut *target;
            target.len = boxed.len();
            target.ptr = Box::into_raw(boxed) as *mut T;
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn free_slice(slice: WasmSlicePointer<u8>) {
    unsafe {
        let slice = &*slice;
        drop(Box::from_raw(std::slice::from_raw_parts_mut(slice.ptr, slice.len)));
    }
}
