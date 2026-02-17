use std::fmt::{self, Display};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(u32);

impl TypeId {
    pub fn id(self) -> u32 {
        self.0
    }
}

pub const UNKNOWN: TypeId = TypeId(0);
pub const NUMBER: TypeId = TypeId(1);
pub const INTEGER: TypeId = TypeId(2);
pub const BOOLEAN: TypeId = TypeId(3);

impl TypeId {
    pub fn is(self, other: TypeId) -> bool {
        match (self, other) {
            (INTEGER | NUMBER, NUMBER) => true,
            _ => self == other,
        }
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let description = match *self {
            UNKNOWN => "unknwown",
            NUMBER => "number",
            INTEGER => "integer",
            BOOLEAN => "boolean",
            _ => unreachable!(),
        };

        write!(f, "{description}")
    }
}
