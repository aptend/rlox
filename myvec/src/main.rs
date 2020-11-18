#![feature(ptr_internals)]
#![feature(allocator_api)]
#![allow(dead_code)]
// working on nightly 1.50

use std::alloc::{handle_alloc_error, AllocRef, Global, Layout};
use std::marker::PhantomData;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::{self, Unique};

struct RawValIter<T> {
    startp: *const T,
    endp: *const T,
}

impl<T> RawValIter<T> {
    // mark it unsafe because it has unbonud lifetime
    // In our private implementation, we will store RawValIter with RawVec
    unsafe fn new(slice: &[T]) -> Self {
        RawValIter {
            startp: slice.as_ptr(),
            endp: if mem::size_of::<T>() == 0 {
                (slice.as_ptr() as usize + slice.len()) as *const T
            } else if slice.len() == 0 {
                // if `len = 0`, then this is *might* not actually allocated memory.
                // Need to avoid offsetting because that will give wrong
                // information to LLVM via GEP.
                slice.as_ptr()
            } else {
                slice.as_ptr().add(slice.len())
            },
        }
    }

    fn inc_startp(&mut self) {
        unsafe {
            self.startp = if mem::size_of::<T>() == 0 {
                (self.startp as usize + 1) as *const T
            } else {
                self.startp.add(1)
            };
        }
    }

    fn dec_endp(&mut self) {
        unsafe {
            self.endp = if mem::size_of::<T>() == 0 {
                (self.endp as usize - 1) as *const T
            } else {
                self.startp.sub(1)
            };
        }
    }
}

impl<T> Iterator for RawValIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.startp == self.endp {
            None
        } else {
            unsafe {
                let result = ptr::read(self.startp);
                self.inc_startp();
                Some(result)
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let elem_size = mem::size_of::<T>();
        let elem_size = if elem_size == 0 { 1 } else { elem_size };
        let len = (self.endp as usize - self.startp as usize) / elem_size;
        (len, Some(len))
    }
}

impl<T> Drop for RawValIter<T> {
    fn drop(&mut self) {
        unsafe {
            while self.startp < self.endp {
                (self.startp as *mut T).drop_in_place();
                self.inc_startp();
            }
        }
    }
}

impl<T> DoubleEndedIterator for RawValIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.startp == self.endp {
            None
        } else {
            unsafe {
                self.dec_endp();
                let result = ptr::read(self.endp);
                Some(result)
            }
        }
    }
}

struct IntoIter<T> {
    buf: RawVec<T>,
    iter: RawValIter<T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<T> {
        self.iter.next_back()
    }
}

struct Drain<'a, T> {
    // MyVec can't be mutated during Drain is being held.
    _vec: PhantomData<&'a mut MyVec<T>>,
    iter: RawValIter<T>,
}

impl<'a, T> Iterator for Drain<'a, T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for Drain<'a, T> {
    fn next_back(&mut self) -> Option<T> {
        self.iter.next_back()
    }
}

impl<T> IntoIterator for MyVec<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        unsafe {
            // RawVec !: Copy, so we get buf without destructure it.
            let buf = ptr::read(&self.buf);
            let raw_iter = RawValIter::new(&self);
            // IntoIter is responsible for dropping now.
            mem::forget(self);
            IntoIter {
                buf,
                iter: raw_iter,
            }
        }
    }
}

struct RawVec<T> {
    ptr: Unique<T>,
    cap: usize,
}

impl<T> RawVec<T> {
    pub fn new() -> Self {
        let cap = if mem::size_of::<T>() != 0 {
            0
        } else {
            usize::MAX
        };
        RawVec {
            ptr: Unique::dangling(),
            cap,
        }
    }

    fn current_layout(&self) -> Layout {
        let align = mem::align_of::<T>();
        let size = mem::size_of::<T>();
        unsafe { Layout::from_size_align_unchecked(size * self.cap, align) }
    }

    pub fn grow(&mut self) {
        unsafe {
            let elem_size = mem::size_of::<T>();
            // never allocate ZST
            assert!(elem_size != 0, "capacity overflow");

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

impl<T> Drop for RawVec<T> {
    fn drop(&mut self) {
        let elem_size = mem::size_of::<T>();
        if self.cap != 0 && elem_size != 0 {
            unsafe {
                let layout = self.current_layout();
                Global.dealloc(self.ptr.cast().into(), layout);
            }
        }
    }
}

struct MyVec<T> {
    buf: RawVec<T>,
    len: usize,
}

impl<T> MyVec<T> {
    pub fn new() -> Self {
        MyVec {
            buf: RawVec::new(),
            len: 0,
        }
    }

    fn ptr(&self) -> *mut T {
        self.buf.ptr.as_ptr()
    }

    fn cap(&self) -> usize {
        self.buf.cap
    }

    pub fn insert(&mut self, index: usize, elem: T) {
        assert!(index <= self.len, "index out of bounds");
        if self.cap() == self.len {
            self.buf.grow();
        }

        unsafe {
            if index < self.len {
                ptr::copy(
                    self.ptr().offset(index as isize),
                    self.ptr().offset(1 + index as isize),
                    self.len - index,
                );
            }
            ptr::write(self.ptr().offset(index as isize), elem);
            self.len += 1;
        }
    }

    pub fn remove(&mut self, index: usize) -> T {
        assert!(index <= self.len, "index out of bounds");
        unsafe {
            self.len -= 1;
            let result = ptr::read(self.ptr().offset(index as isize));
            ptr::copy(
                self.ptr().offset(1 + index as isize),
                self.ptr().offset(index as isize),
                self.len - index,
            );
            result
        }
    }

    pub fn push(&mut self, elem: T) {
        if self.cap() == self.len {
            self.buf.grow();
        }

        unsafe {
            ptr::write(self.ptr().offset(self.len as isize), elem);
        }

        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe { Some(ptr::read(self.ptr().offset(self.len as isize))) }
        }
    }

    pub fn drain(&mut self) -> Drain<'_, T> {
        unsafe {
            let iter = RawValIter::new(&self);

            // make it safe to forget Drain, but it also leads to leak amplification
            self.len = 0;

            Drain {
                iter,
                _vec: PhantomData,
            }
        }
    }
}

impl<T> Drop for MyVec<T> {
    fn drop(&mut self) {
        let len = self.len as isize;
        self.len = 0;
        unsafe {
            // it is safe even T is ZST, since self.ptr() is non-zero and aligned.
            for i in 0..len {
                ptr::drop_in_place(self.ptr().offset(i));
            }
        }
    }
}

impl<T> Deref for MyVec<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.ptr(), self.len) }
    }
}

impl<T> DerefMut for MyVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.ptr(), self.len) }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::rc::Rc;
    #[test]
    fn test_myvec_drop() {
        let mut v = MyVec::new();
        let strs = ["hello", "world", "again"];
        let mut weak = Vec::new();
        for str in &strs {
            let s = Rc::new(String::from(*str));
            weak.push(Rc::downgrade(&s));
            v.push(s);
        }
        {
            v.pop();
        }
        assert!(weak[2].upgrade().is_none(), "pop didn't drop");
        {
            drop(v);
        }
        assert!(weak[0].upgrade().is_none(), "myvec didn't drop");
        assert!(weak[1].upgrade().is_none(), "myvec didn't drop");
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

    #[test]
    fn test_myvec_intoiter() {
        let mut v = MyVec::new();
        let strs = ["hello", "world", "again", "and again"];
        let mut weak = Vec::new();
        for str in &strs {
            let s = Rc::new(String::from(*str));
            weak.push(Rc::downgrade(&s));
            v.push(s);
        }

        let mut iter = v.into_iter();
        {
            iter.next();
            iter.next_back();
        }

        assert!(weak[0].upgrade().is_none());
        assert!(weak[3].upgrade().is_none());
        {
            assert!(weak[1].upgrade().is_some());
            drop(iter);
        }

        assert!(weak[1].upgrade().is_none());
        assert!(weak[2].upgrade().is_none());
    }

    #[test]
    fn test_myvec_drain() {
        let mut v = MyVec::new();
        for x in 1..5usize {
            v.push(x);
        }
        v.drain();
        v.push(21);

        // error: cannot borrow `v` as mutable more than once at a time
        // let mut d = v.drain();
        // v.push(21);
        // println!("{}", d.next());
        assert_eq!(&[21], &*v);
    }

    #[test]
    fn test_myvec_zst() {
        let mut v = MyVec::new();
        for _ in 1..5usize {
            v.push(());
        }
        assert!(v.cap() == usize::MAX);
        assert!(() == v.pop().unwrap());

        v.drain();
        v.push(());
        assert!(() == v.pop().unwrap());
        assert!(v.len() == 0);
    }
}

fn main() {}
