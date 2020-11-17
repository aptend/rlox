#![feature(ptr_internals)]
#![feature(allocator_api)]

// working on nightly 1.50

use std::alloc::{handle_alloc_error, AllocRef, Global, Layout};
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::{self, Unique};

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

    pub fn insert(&mut self, index: usize, elem: T) {
        assert!(index <= self.len, "index out of bounds");
        if self.cap == self.len {
            self.grow();
        }

        unsafe {
            if index < self.len {
                ptr::copy(
                    self.ptr.as_ptr().offset(index as isize),
                    self.ptr.as_ptr().offset(1 + index as isize),
                    self.len - index,
                );
            }
            ptr::write(self.ptr.as_ptr().offset(index as isize), elem);
            self.len += 1;
        }
    }

    pub fn remove(&mut self, index: usize) -> T {
        assert!(index <= self.len, "index out of bounds");
        unsafe {
            self.len -= 1;
            let result = ptr::read(self.ptr.as_ptr().offset(index as isize));
            ptr::copy(
                self.ptr.as_ptr().offset(1 + index as isize),
                self.ptr.as_ptr().offset( index as isize),
                self.len - index
            );
            result
        }

    }

    pub fn push(&mut self, elem: T) {
        if self.len == self.cap {
            self.grow();
        }

        unsafe {
            ptr::write(self.ptr.as_ptr().offset(self.len as isize), elem);
        }

        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe {
                Some(ptr::read(self.ptr.as_ptr().offset(self.len as isize)))
            }
        }
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

impl<T> Drop for MyVec<T> {
    fn drop(&mut self) {
        if self.cap != 0 {
            let len = self.len as isize;
            self.len = 0;
            unsafe {
                for i in 0..len {
                    ptr::drop_in_place(self.ptr.as_ptr().offset(i));
                }
                let layout = self.current_layout();
                Global.dealloc(self.ptr.cast().into(), layout);
            }
        }
    }
}

impl<T> Deref for MyVec<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }
}

impl<T> DerefMut for MyVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use std::rc::Rc;
    #[test]
    fn test_myvec_drop() {
        let mut v = MyVec::new();
        let s1 = Rc::new(String::from("hello"));
        let s1_weak = Rc::downgrade(&s1);
        let s2 = Rc::new(String::from("world"));
        let s2_weak = Rc::downgrade(&s2);
        let s3 = Rc::new(String::from("....."));
        let s3_weak = Rc::downgrade(&s3);
        v.push(s1);
        v.push(s2);
        v.push(s3);
        {
            v.pop();
        }
        assert!(s3_weak.upgrade().is_none(), "pop didn't drop");
        {
            drop(v);
        }
        assert!(s1_weak.upgrade().is_none(), "myvec didn't drop");
        assert!(s2_weak.upgrade().is_none(), "myvec didn't drop");
    }


    #[test]
    fn test_myvec_deref() {
        let mut v = MyVec::new();
        for x in 0..5usize {
            v.push(x);
        }
        assert_eq!(&[0, 1, 2, 3, 4], &*v);
    }

    #[test]
    fn test_myvec_insert() {
        let mut v = MyVec::new();
        for x in 1..5usize {
            v.push(x);
        }
        v.insert(0, 0);
        assert_eq!(&[0, 1, 2, 3, 4], &*v);
        v.insert(5, 5);
        assert_eq!(&[0, 1, 2, 3, 4, 5], &*v);
    }

    #[test]
    fn test_myvec_remove() {
        let mut v = MyVec::new();
        for x in 1..5usize {
            v.push(x);
        }
        assert_eq!(v.remove(2), 3);
        assert_eq!(&[1, 2, 4], &*v);
        assert_eq!(v.remove(2), 4);
        assert_eq!(&[1, 2], &*v);

    }
}

fn main() {}
