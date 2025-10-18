use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct NameId(u32);

impl NameId {
    #[inline(always)]
    pub fn id(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct Interner {
    entries: HashMap<Box<str>, NameId>,
    values: Vec<Box<str>>,
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            entries: HashMap::new(),
            values: Vec::new(),
        }
    }

    pub fn intern(&mut self, value: impl Into<Box<str>>) -> NameId {
        let value = value.into();
        match self.entries.entry(value.clone()) {
            Entry::Vacant(entry) => {
                let index = NameId(self.values.len() as u32);
                entry.insert(index);
                self.values.push(value);
                index
            }
            Entry::Occupied(entry) => {
                let index = *entry.get();
                self.values[index.id()] = value;
                index
            }
        }
    }

    pub fn get(&self, name_id: NameId) -> Option<&str> {
        self.values.get(name_id.id()).map(|it| it.as_ref())
    }
}

#[derive(Debug)]
pub struct State {
    pub(crate) interner: Interner,
}

impl State {
    pub fn new() -> State {
        State {
            interner: Interner::new(),
        }
    }
}
