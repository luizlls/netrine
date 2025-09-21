use std::fmt::{self, Display};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(u32);

impl TypeId {
    pub fn id(self) -> u32 {
        self.0
    }
}

const NOTHING_ID: TypeId = TypeId(0);
const NUMBER_ID: TypeId = TypeId(1);
const INTEGER_ID: TypeId = TypeId(2);
const BOOL_ID: TypeId = TypeId(3);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Builtin(TypeId),
    Unknown,
}

pub const NOTHING: Type = Type::Builtin(NOTHING_ID);
pub const NUMBER: Type = Type::Builtin(NUMBER_ID);
pub const INTEGER: Type = Type::Builtin(INTEGER_ID);
pub const BOOLEAN: Type = Type::Builtin(BOOL_ID);

pub const fn builtin_types() -> &'static [Type] {
    &[
        NOTHING,
        NUMBER,
        INTEGER,
        BOOLEAN,
    ]
}

impl Type {
    pub fn is(self, other: Type) -> bool {
        match (self, other) {
            (INTEGER | NUMBER, NUMBER) => true,
            _ => self == other  
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let description = match self {
            &NOTHING => "nothing",
            &NUMBER => "number",
            &INTEGER => "integer",
            &BOOLEAN => "boolean",
            Type::Unknown => "<unknown>",
            _ => unreachable!(),
        };

        write!(f, "{description}")
    }
}
