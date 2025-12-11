use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::{self, Display};

use crate::types::{self, Type};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct SymbolId(u32);

impl SymbolId {
    #[inline(always)]
    pub fn id(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for SymbolId {
    fn from(value: usize) -> SymbolId {
        SymbolId(value as u32)
    }
}

impl From<SymbolId> for usize {
    fn from(value: SymbolId) -> usize {
        value.0 as usize
    }
}

impl Display for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub(crate) symbol_id: SymbolId,
    pub(crate) name: String,
    pub(crate) type_: Type,
}

#[derive(Debug)]
pub struct Symbols {
    symbols: Vec<Symbol>,
    indices: HashMap<String, SymbolId>,
}

impl Symbols {
    pub fn new() -> Symbols {
        Symbols {
            symbols: Vec::new(),
            indices: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, type_: Type) -> SymbolId {
        match self.indices.entry(name.clone()) {
            Entry::Vacant(entry) => {
                let symbol_id = SymbolId::from(self.symbols.len());

                self.symbols.push(Symbol {
                    symbol_id,
                    name,
                    type_,
                });

                entry.insert(symbol_id);

                symbol_id
            }
            Entry::Occupied(entry) => *entry.get(),
        }
    }

    pub fn lookup(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(symbol_id.id())
    }

    pub fn resolve(&self, key: &String) -> Option<&Symbol> {
        self.indices
            .get(key)
            .map(|&symbol_id| &self.symbols[symbol_id.id()])
    }
}

pub const NONE: SymbolId = SymbolId(0);
pub const TRUE: SymbolId = SymbolId(1);
pub const FALSE: SymbolId = SymbolId(2);

#[derive(Debug)]
pub struct State {
    pub(crate) symbols: Symbols,
}

impl State {
    pub fn new() -> State {
        State {
            symbols: Symbols::new(),
        }
        .init()
    }

    fn init(mut self) -> State {
        let builtin_symbols = [
            ("", types::NOTHING, NONE),
            ("True", types::BOOLEAN, TRUE),
            ("False", types::BOOLEAN, FALSE),
        ];
        for (name, type_, symbol_id) in builtin_symbols {
            let result_id = self.define(name.into(), type_);
            debug_assert_eq!(result_id, symbol_id)
        }
        self
    }

    pub fn define(&mut self, name: String, type_: Type) -> SymbolId {
        self.symbols.define(name, type_)
    }

    pub fn symbol(&self, name: &String) -> Option<&Symbol> {
        self.symbols.resolve(name)
    }

    pub fn symbol_by_id(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbols.lookup(symbol_id)
    }
}
