// Very much like slotmap crate.
// Differences:
//  - keys are 32-bit instead of 64-bit
//  - strict (doesn't allow to check if key is present, assumes it is)
//  - simpler, fewer features
//  - not thoroughly tested at the moment

use std::mem::ManuallyDrop;
use std::marker::PhantomData;
use std::convert::TryInto;

pub trait Key: Copy {
    fn new(index: u32, version: u8) -> Self;
    fn index(self) -> u32;
    fn version(self) -> u8;
}

union SlotUnion<T> {
    value: ManuallyDrop<T>,
    next_free: u32,
}

struct Slot<T> {
    version: u8,
    u: SlotUnion<T>, // even - vacant, odd - occupied
}

impl<T> Slot<T> {
    fn occupied(&self) -> bool {
        self.version % 2 != 0
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Slot<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("Slot");
        d.field("version", &self.version);
        if self.occupied() {
            d.field("u.value", unsafe { &*self.u.value });
        } else {
            d.field("u.next_free", unsafe { &self.u.next_free });
        }
        d.finish()
    }
}

impl<T> Drop for Slot<T> {
    fn drop(&mut self) {
        if std::mem::needs_drop::<T>() && self.occupied() {
            unsafe {
                ManuallyDrop::drop(&mut self.u.value);
            }
        }
    }
}

pub struct SlotMap<K: Key, T> {
    slots: Vec<Slot<T>>,
    free: u32,
    len: u32,
    _invariant: PhantomData<fn(K) -> K>,
}

impl<K: Key, T: std::fmt::Debug> std::fmt::Debug for SlotMap<K, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SlotMap")
            .field("slots", &self.slots)
            .field("free", &self.free)
            .field("len", &self.len)
            .finish()
    }
}

#[allow(clippy::len_without_is_empty)]
impl<K: Key, T> SlotMap<K, T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        SlotMap {
            slots: vec![],
            free: u32::max_value(),
            len: 0,
            _invariant: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn insert(&mut self, value: T) -> K {
        let slot_idx;
        let slot;
        if self.free != u32::max_value() {
            slot_idx = self.free;
            slot = &mut self.slots[self.free as usize];
            assert!(!slot.occupied());
            self.free = unsafe { slot.u.next_free };
            slot.u.value = ManuallyDrop::new(value);
        } else {
            slot_idx = self.slots.len().try_into().unwrap();
            self.slots.push(Slot {
                version: 0,
                u: SlotUnion { value: ManuallyDrop::new(value) }
            });
            slot = self.slots.last_mut().unwrap();
        }
        slot.version = slot.version.wrapping_add(1);
        self.len += 1;
        assert!(slot.occupied());
        K::new(slot_idx, slot.version)
    }

    pub fn remove(&mut self, k: K) -> T {
        let idx = k.index();
        let slot = &mut self.slots[idx as usize];
        assert_eq!(slot.version, k.version());
        assert!(slot.occupied());
        slot.version = slot.version.wrapping_add(1);
        let result = unsafe { ManuallyDrop::take(&mut slot.u.value) };
        slot.u.next_free = self.free;
        self.free = idx;
        self.len -= 1;
        result
    }
}

impl<K: Key, T> std::ops::Index<K> for SlotMap<K, T> {
    type Output = T;

    fn index(&self, k: K) -> &Self::Output {
        let slot = &self.slots[k.index() as usize];
        assert_eq!(slot.version, k.version());
        assert!(slot.occupied());
        unsafe { &*slot.u.value }
    }
}

impl<K: Key, T> std::ops::IndexMut<K> for SlotMap<K, T> {
    fn index_mut(&mut self, k: K) -> &mut Self::Output {
        let slot = &mut self.slots[k.index() as usize];
        assert_eq!(slot.version, k.version());
        assert!(slot.occupied());
        unsafe { &mut *slot.u.value }
    }
}

#[macro_use]
macro_rules! new_key_type {
    ($vis:vis $name:ident) => {

        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        $vis struct $name(std::num::NonZeroU32);

        impl $crate::slotmap::Key for $name {
            fn new(index: u32, version: u8) -> Self {
                assert!(version % 2 != 0);
                let x = index.checked_mul(256).unwrap() + version as u32;
                Self(std::num::NonZeroU32::new(x).unwrap())
            }
            fn index(self) -> u32 {
                self.0.get() / 256
            }
            fn version(self) -> u8 {
                (self.0.get() % 256) as u8
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use $crate::slotmap::Key;
                write!(f, "{}v{}", self.index(), self.version())
            }
        }        
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    new_key_type!(FooKey);

    #[test]
    fn stuff() {
        let mut sm = SlotMap::<FooKey, String>::new();
        assert_eq!(sm.len(), 0);
        dbg!(&sm);
        let k1 = sm.insert("hello".to_string());
        assert_eq!(sm.len(), 1);
        dbg!(k1);
        dbg!(&sm);
        let k2 = sm.insert("world".to_string());
        assert_eq!(sm.len(), 2);
        dbg!(k2);
        dbg!(&sm[k1]);
        dbg!(&sm);
        sm.remove(k1);
        assert_eq!(sm.len(), 1);
        dbg!(&sm[k2]);
        dbg!(&sm);
        let k3 = sm.insert("q".to_string());
        assert_eq!(sm.len(), 2);
        dbg!(k3);
        dbg!(&sm[k3]);
        dbg!(&sm);
        sm[k2].push('!');
        assert_eq!(sm[k2], "world!");
    }

    #[test]
    #[should_panic]
    fn deleted_key() {
        let mut sm = SlotMap::<FooKey, i32>::new();
        let k = sm.insert(42);
        sm.remove(k);
        dbg!(&sm[k]);
    }

    #[test]
    #[should_panic]
    fn reinserted_key() {
        let mut sm = SlotMap::<FooKey, i32>::new();
        let k1 = sm.insert(1);
        dbg!(k1);
        let k2 = sm.insert(2);
        dbg!(k2);
        sm.remove(k1);
        let k3 = sm.insert(3);
        dbg!(k3);
        dbg!(&sm[k3]);
        dbg!(&sm[k1]);
    }
}
