use std::mem::ManuallyDrop;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::ops::{Deref, DerefMut};

pub struct Owned<T>(Rc<RefCell<T>>);

pub struct Refed<T>(Weak<RefCell<T>>);

impl<T> Owned<T> {
    pub fn new(x: T) -> Self {
        Owned(Rc::new(RefCell::new(x)))
    }

    pub fn make_ref(&self) -> Refed<T> {
        Refed(Rc::downgrade(&self.0))
    }

    pub fn ptr_eq(&self, other: &Refed<T>) -> bool {
        Rc::as_ptr(&self.0) == Weak::as_ptr(&other.0)
    }

    pub fn borrow<'a>(&'a self) -> impl Deref<Target=T> + 'a {
        self.0.borrow()
    }

    // doesn't really need &mut self,
    // but requires it for consistency with Box<T>
    pub fn borrow_mut<'a>(&'a mut self) -> impl Deref<Target=T> + DerefMut + 'a {
        self.0.borrow_mut()
    }
}

impl<T> Drop for Owned<T> {
    fn drop(&mut self) {
        assert_eq!(Rc::strong_count(&self.0), 1);
        assert_eq!(Rc::weak_count(&self.0), 0);
    }
}

impl<T> Refed<T> {
    #[allow(clippy::should_implement_trait)]
    pub fn clone(this: &Refed<T>) -> Refed<T> {
        Refed(Weak::clone(&this.0))
    }

    pub fn ptr_eq(&self, other: &Refed<T>) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }

    pub fn borrow<'a>(&'a self) -> impl Deref<Target=T> + 'a {
        let strong = Weak::upgrade(&self.0).unwrap();
        let ptr = Rc::as_ptr(&strong);
        let ref_cell: &RefCell<T> = unsafe { &*ptr };
        Ref {
            strong,
            r: ManuallyDrop::new(ref_cell.borrow()),
        }
    }

    pub fn borrow_mut<'a>(&'a self) -> impl Deref<Target=T> + DerefMut + 'a {
        let strong = Weak::upgrade(&self.0).unwrap();
        let ptr = Rc::as_ptr(&strong);
        let ref_cell: &RefCell<T> = unsafe { &*ptr };
        RefMut {
            strong,
            r: ManuallyDrop::new(ref_cell.borrow_mut()),
        }
    }
}

impl<T> Clone for Refed<T> {
    fn clone(&self) -> Self {
        Refed::clone(self)
    }
}

struct Ref<'a, T> {
    #[allow(dead_code)]
    strong: Rc<RefCell<T>>,
    r: ManuallyDrop<std::cell::Ref<'a, T>>,
    // to ensure it's dropped before strong
}

impl<'a, T> Deref for Ref<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.r.deref()
    }
}

impl<'a, T> Drop for Ref<'a, T> {
    fn drop(&mut self) {
        unsafe {
            ManuallyDrop::drop(&mut self.r);
        }
    }
}

struct RefMut<'a, T> {
    #[allow(dead_code)]
    strong: Rc<RefCell<T>>,
    r: ManuallyDrop<std::cell::RefMut<'a, T>>,
    // to ensure it's dropped before strong
}

impl<'a, T> Deref for RefMut<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.r.deref()
    }
}

impl<'a, T> DerefMut for RefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.r.deref_mut()
    }
}

impl<'a, T> Drop for RefMut<'a, T> {
    fn drop(&mut self) {
        unsafe {
            ManuallyDrop::drop(&mut self.r);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn z() {
        let o = Owned::new(42);
        let r = o.make_ref();
        *r.borrow_mut() += 1;
        assert_eq!(*o.borrow(), 43);
    }
}
