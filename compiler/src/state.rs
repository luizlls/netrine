use std::fmt::Display;
use std::ops::Deref;

use crate::index_map::IndexMap;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct NameId(u32);

impl NameId {
    #[inline(always)]
    pub fn id(self) -> u32 {
        self.0
    }
}

impl From<usize> for NameId {
    fn from(value: usize) -> NameId {
        NameId(value as u32)
    }
}

impl Into<usize> for NameId {
    fn into(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Box<str>);

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Name {
        Name(value.into())
    }
}

impl From<Box<str>> for Name {
    fn from(value: Box<str>) -> Name {
        Name(value)
    }
}

impl Deref for Name {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Debug)]
pub struct State {
    names: IndexMap<Name, Name, NameId>,
}

impl State {
    pub fn new() -> State {
        State {
            names: IndexMap::new(),
        }
    }

    pub fn get_name(&self, id: NameId) -> &Name {
        &self.names[id]
    }

    pub fn add_name(&mut self, name: Name) -> NameId {
        self.names.insert_value(name)
    }
}
