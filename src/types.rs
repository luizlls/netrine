use std::ops::Deref;

use crate::source::{SourceId, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub usize);

impl Deref for TypeId {
    type Target = usize;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub const TYPE_UNKNOWN: TypeId = TypeId(0);
pub const TYPE_NUMBER: TypeId = TypeId(2);
pub const TYPE_INTEGER: TypeId = TypeId(3);

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
    pub source_id: SourceId,
}

impl Type {
    pub fn new(kind: TypeKind, span: Span, source_id: SourceId) -> Type {
        Type {
            kind,
            span,
            source_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Unknown,
    Named(String),
}

impl Type {
    pub fn builtin() -> [Type; 3] {
        [
            Type::new(TypeKind::Unknown, Span::default(), SourceId::default()),
            Type::new(TypeKind::Named("Integer".to_string()), Span::default(), SourceId::default()),
            Type::new(TypeKind::Named("Number".to_string()), Span::default(), SourceId::default()),
        ]
    }
}
