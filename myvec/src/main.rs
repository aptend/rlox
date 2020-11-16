#![feature(ptr_internals)]
#![feature(allocator_api)]

use std::alloc::{handle_alloc_error, AllocRef, Global, Layout};
use std::mem;
use std::ptr::Unique;

struct MyVec<T> {
    ptr: Unique<T>,
    len: usize,
    cap: usize,
}

impl<T> MyVec<T> {
    pub fn new() -> Self {
        assert!(mem::size_of::<T>() != 0, "We're not ready to handle ZSTs");
        MyVec {
            ptr: Unique::dangling(),
            cap: 0,
            len: 0,
        }
    }

    fn current_layout(&self) -> Layout {
        let align = mem::align_of::<T>();
        let size = mem::size_of::<T>();
        unsafe { Layout::from_size_align_unchecked(size * self.cap, align) }
    }

    pub fn grow(&mut self) {
        unsafe {
            if self.cap == 0 {
                let layout = Layout::new::<T>();
                let ptr = match Global.alloc(layout) {
                    Ok(ptr) => ptr,
                    Err(_) => handle_alloc_error(layout),
                };
                self.ptr = Unique::new_unchecked(ptr.cast().as_ptr());
                self.cap = 1;
            } else {
                let old_layout = self.current_layout();
                let new_layout = match Layout::array::<T>(self.cap * 2) {
                    Ok(layout) => layout,
                    Err(_) => panic!("capacity overflow"),
                };
                let ptr = match Global.grow(
                    self.ptr.cast().into(),
                    old_layout,
                    new_layout,
                ) {
                    Ok(ptr) => ptr,
                    Err(_) => handle_alloc_error(new_layout),
                };
                self.ptr = Unique::new_unchecked(ptr.cast().as_ptr());
                self.cap *= 2;
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
