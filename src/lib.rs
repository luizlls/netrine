#![feature(let_chains)]
#![feature(box_patterns)]

mod ast;
mod error;
mod infer;
mod ir;
mod lexer;
mod lower;
mod parser;
mod span;
mod state;
mod token;
mod types;

pub mod syntax;
