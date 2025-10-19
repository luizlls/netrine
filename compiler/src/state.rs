use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::source::Span;
use crate::syntax::NodeId;
use crate::types::Type;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct NameId(u32);

impl NameId {
    #[inline(always)]
    pub fn id(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct SymbolId(u32);

impl SymbolId {
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
        self.values.get(name_id.id()).map(Box::as_ref)
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub(crate) name: NameId,
    pub(crate) node: NodeId,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub struct Symbols {
    symbols: Vec<Symbol>,
    entries: HashMap<NameId, SymbolId>,
}

impl Symbols {
    pub fn new() -> Symbols {
        Symbols {
            symbols: Vec::new(),
            entries: HashMap::new(),
        }
    }

    pub fn define(&mut self, symbol: Symbol) -> SymbolId {
        let symbol_id = SymbolId(self.symbols.len() as u32);
        self.entries.insert(symbol.name, symbol_id);
        self.symbols.push(symbol);
        symbol_id
    }

    pub fn get(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(symbol_id.id())
    }

    pub fn lookup(&self, name: NameId) -> Option<&Symbol> {
        self.entries.get(&name).and_then(|&id| self.get(id))
    }
}

#[derive(Debug)]
pub struct Types {
    types: Vec<Type>,
}

impl Types {
    fn new() -> Types {
        Types { types: vec![] }
    }

    pub fn insert(&mut self, id: NodeId, type_: Type) {
        debug_assert!(id.index() == self.types.len());
        self.types.push(type_)
    }

    pub fn get(&self, id: NodeId) -> Type {
        self.types.get(id.index()).copied().unwrap_or(Type::Unknown)
    }

    pub fn types(&self) -> &[Type] {
        &self.types
    }
}

#[derive(Debug)]
pub struct State {
    pub(crate) interner: Interner,
    pub(crate) symbols: Symbols,
    pub(crate) types: Types,
}

impl State {
    pub fn new() -> State {
        State {
            interner: Interner::new(),
            symbols: Symbols::new(),
            types: Types::new(),
        }
    }
}
