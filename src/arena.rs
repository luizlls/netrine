// A simple bump/arena allocator. Based on bumpalo

use std::alloc::{alloc, Layout, dealloc};
use std::cell::{Cell, RefCell};
use std::ptr::{NonNull, self};

const CHUNK_SIZE: usize = std::mem::size_of::<Chunk>();
const CHUNK_ALIGN: usize = std::mem::align_of::<Chunk>();
const CHUNK_OVERHEAD: usize = (CHUNK_SIZE + CHUNK_ALIGN - 1) & !(CHUNK_ALIGN - 1);

const DEFAULT_CHUNK_DATA_SIZE: usize = 1 << 10;
const DEFAULT_CHUNK_DATA_ALIGN: usize = 16;

#[derive(Debug)]
struct Chunk {
    ptr: Cell<NonNull<u8>>,
    data: Cell<NonNull<u8>>,
    layout: Layout,
}

impl Chunk {
    fn new() -> NonNull<Chunk> {
        unsafe {
            let layout = Layout::from_size_align_unchecked(DEFAULT_CHUNK_DATA_SIZE, DEFAULT_CHUNK_DATA_ALIGN);
            Chunk::from_layout(layout)
        }
    }

    fn from_layout(layout: Layout) -> NonNull<Chunk> {
        unsafe {
            let final_size = layout.size().max(DEFAULT_CHUNK_DATA_SIZE);
            let final_align = layout.align().max(DEFAULT_CHUNK_DATA_ALIGN);
            let layout = Layout::from_size_align_unchecked(final_size, final_align);

            let data = alloc(layout);

            if data.is_null() {
                panic!("[ARENA]: out of memory");
            }

            let data = NonNull::new_unchecked(data);

            // pointer to the "end" of the chunk data minus the chunk overhead (where the chunk data will be stored)
            let chunk_ptr = data.as_ptr().add(final_size - CHUNK_OVERHEAD);
            let chunk_ptr = chunk_ptr as *mut Chunk;

            let ptr = Cell::new(NonNull::new_unchecked(chunk_ptr as *mut u8));
            let data = Cell::new(data);

            ptr::write(chunk_ptr, Chunk { ptr, data, layout });

            // the chunk available space starts after the chunk ptr
            NonNull::new_unchecked(chunk_ptr)
        }
    }
}

#[derive(Debug)]
pub struct Arena {
    chunk: Cell<NonNull<Chunk>>,
    chunks: RefCell<Vec<NonNull<Chunk>>>,
}

impl Arena {
    pub fn new() -> Arena {
        let chunk = Chunk::new();
        Arena {
            chunk: Cell::new(chunk),
            chunks: RefCell::new(vec![chunk]),
        }
    }

    pub fn alloc<T>(&self, value: T) -> &T {
        let layout = Layout::new::<T>();
        unsafe {
            let ptr = self.alloc_inner(layout);
            let ptr = ptr.as_ptr() as *mut T;
            ptr::write(ptr, value);
            &*ptr
        }
    }

    fn alloc_inner(&self, layout: Layout) -> NonNull<u8> {
        if let Some(ptr) = self.try_alloc_fast(layout) {
            ptr
        } else if let Some(ptr) = self.try_alloc_slow(layout) {
            ptr
        } else {
            panic!("[ARENA]: out of memory");
        }
    }

    fn try_alloc_fast(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe {
            let chunk = self.chunk.get().as_ref();
            let start = chunk.data.get().as_ptr();
            let mut ptr = chunk.ptr.get().as_ptr();

            // not enough space
            if (ptr as usize) < layout.size() {
                return None;
            }

            // move backwards, "bumping" the pointer by decreasing it
            ptr = ptr.wrapping_sub(layout.size());
            ptr = ptr.wrapping_sub(ptr as usize % layout.align());

            if ptr >= start {
                let ptr = NonNull::new_unchecked(ptr as *mut u8);
                chunk.ptr.set(ptr);
                Some(ptr)
            } else {
                None
            }
        }
    }

    fn try_alloc_slow(&self, layout: Layout) -> Option<NonNull<u8>> {
        unsafe {
            let chunk = Chunk::from_layout(layout);
            self.chunk.set(chunk);
            self.chunks.borrow_mut().push(chunk);

            let mut ptr = chunk.as_ref().ptr.as_ptr();
            ptr = ptr.sub(layout.size());
            ptr = ptr.sub(ptr as usize % layout.align());

            Some(NonNull::new_unchecked(ptr as *mut u8))
        }
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        unsafe {
            for &mut chunk in self.chunks.get_mut() {
                dealloc(chunk.as_ref().data.get().as_ptr(), chunk.as_ref().layout);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Eq, PartialEq)]
    enum TestEnum<'a> {
        Simple,
        Value(&'a str),
        Struct(TestStruct<'a>),
        Ref(&'a TestEnum<'a>),
        Vec(Vec<TestEnum<'a>>),
        VecRef(Vec<&'a TestEnum<'a>>),
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    struct TestStruct<'a> {
        value: usize,
        reference: &'a TestEnum<'a>,
    }

    #[test]
    fn struct_simple() {
        let arena = Arena::new();

        let simple = arena.alloc(TestEnum::Simple);
        let struct_ = arena.alloc(TestStruct { value: 42, reference: simple });
        assert_eq!(
            struct_,
            &TestStruct {
                value: 42,
                reference: simple,
            },
        );
    }

    #[test]
    fn enum_simple() {
        let arena = Arena::new();

        let simple = arena.alloc(TestEnum::Simple);
        assert_eq!(
            simple,
            &TestEnum::Simple,
        );
    }

    #[test]
    fn enum_value() {
        let arena = Arena::new();

        let value = arena.alloc(TestEnum::Value("Hello, World"));
        assert_eq!(
            value,
            &TestEnum::Value("Hello, World"),
        );
    }

    #[test]
    fn enum_struct() {
        let arena = Arena::new();

        let simple = arena.alloc(TestEnum::Simple);
        let struct_ = arena.alloc(TestEnum::Struct(TestStruct { value: 42, reference: simple }));
        assert_eq!(
            struct_,
            &TestEnum::Struct(TestStruct { value: 42, reference: simple }),
        );
    }

    #[test]
    fn enum_ref() {
        let arena = Arena::new();

        let simple = arena.alloc(TestEnum::Simple);
        let enum_ref = arena.alloc(TestEnum::Ref(simple));
        assert_eq!(
            enum_ref,
            &TestEnum::Ref(simple),
        );
    }

    #[test]
    fn enum_vec() {
        let arena = Arena::new();

        let value = arena.alloc(TestEnum::Value("Hello, World"));
        let vector = arena.alloc(TestEnum::Vec(vec![TestEnum::Simple, TestEnum::Ref(value)]));
        assert_eq!(
            vector,
            &TestEnum::Vec(vec![TestEnum::Simple, TestEnum::Ref(value)]),
        );
    }

    #[test]
    fn enum_vec_ref() {
        let arena = Arena::new();

        let fst = arena.alloc(TestEnum::Simple);
        let snd = arena.alloc(TestEnum::Value("Hello, World"));
        let trd = arena.alloc(TestEnum::Ref(fst));
    
        let vec_ref = arena.alloc(TestEnum::VecRef(vec![fst, snd, trd]));
        assert_eq!(
            vec_ref,
            &TestEnum::VecRef(vec![fst, snd, trd]),
        );
    }
}
