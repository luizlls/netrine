use hashbrown::HashMap;
use hashbrown::hash_map::RawEntryMut;
use std::hash::{Hash, Hasher};

use crate::collections::IndexVec;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Name(u32);

impl From<usize> for Name {
    fn from(value: usize) -> Name {
        Name(value as u32)
    }
}

impl From<Name> for usize {
    fn from(value: Name) -> usize {
        value.0 as usize
    }
}

#[derive(Debug)]
pub struct Interner {
    values: IndexVec<Name, Box<str>>,
    entries: HashMap<NameKey, Name>,
}

#[derive(Debug, Clone, Copy)]
struct NameKey(*const str);

impl Hash for NameKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // SAFETY: NameKey is always valid as it's either a pointer to a interned box<str> or a pointer to a to be interned string.
        unsafe { (*self.0).hash(state) };
    }
}

impl PartialEq for NameKey {
    fn eq(&self, other: &Self) -> bool {
        // SAFETY: both pointers always point to a valid str
        unsafe { &*self.0 == &*other.0 }
    }
}

impl Eq for NameKey {}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            values: IndexVec::new(),
            entries: HashMap::new(),
        }
    }

    pub fn intern(&mut self, value: &str) -> Name {
        let key = NameKey(value as *const str);

        match self.entries.raw_entry_mut().from_key(&key) {
            RawEntryMut::Occupied(e) => *e.get(),
            RawEntryMut::Vacant(e) => {
                let val: Box<str> = value.into();
                let key: NameKey = NameKey(val.as_ref() as *const str);
                let symbol = self.values.push(val);
                e.insert(key, symbol);
                symbol
            }
        }
    }

    pub fn get(&self, symbol: Name) -> Option<&str> {
        self.values.get(symbol).map(Box::as_ref)
    }
}

impl std::ops::Index<Name> for Interner {
    type Output = str;

    fn index(&self, index: Name) -> &Self::Output {
        self.values[index].as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_interning() {
        let mut interner = Interner::new();
        let foo = interner.intern("foo");
        let bar = interner.intern("bar");

        assert_eq!(&interner[foo], "foo");
        assert_eq!(&interner[bar], "bar");
        assert_eq!(interner.get(foo), Some("foo"));

        let foo1 = interner.intern("foo");
        assert_eq!(foo, foo1);

        assert_eq!(interner.get(Name(10)), None);
    }
}
