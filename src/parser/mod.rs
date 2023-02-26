use std::collections::HashMap;

use chumsky::prelude::*;

#[derive(Debug)]
pub enum Expr {
    Int(u64),
    Real(f64),
    Str(String),
    Bool(bool),

    Call(Box<Expr>, Vec<Expr>),
    Let(HashMap<String, Expr>, Box<Expr>),
    Def(Vec<String>, Box<Expr>),
}

pub fn unit_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    filter(|c: &char| c.is_ascii_digit()).map(|cs| Expr::Int(cs.to_digit(10).unwrap().into()))
}
