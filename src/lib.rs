#![feature(let_chains)]
#![feature(box_patterns)]

pub mod syntax;

mod error;
mod mir;
mod span;
mod state;
mod types;