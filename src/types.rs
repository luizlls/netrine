use std::ops::Deref;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub usize);

impl Deref for TypeId {
    type Target = usize;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub const UNKNOWN: TypeId = TypeId(0);
pub const NUMBER: TypeId = TypeId(2);
pub const INTEGER: TypeId = TypeId(3);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Number,
    Integer,
}

impl Type {
    pub fn builtin() -> [Type; 3] {
        [
            Type::Unknown,
            Type::Number,
            Type::Integer,
        ]
    }
}
