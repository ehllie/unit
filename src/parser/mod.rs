use std::collections::HashMap;

use chumsky::prelude::*;

#[derive(Debug)]
pub enum Expr {
    Int(u64),
    Real(f64),
    Str(String),
    Bool(bool),
    Name(String),

    Neg(Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),
    Let(HashMap<String, Expr>, Box<Expr>),
    Def(Vec<String>, Box<Expr>),
}

// https://github.com/rust-lang/rust/issues/63063
// type UnitParser = impl Parser<char, Expr, Error = Simple<char>>;

pub fn unit_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let op = |c| just(c).padded();

        let int = text::int(10)
            .map(|s: String| Expr::Int(s.parse().unwrap()))
            .padded();

        let name = text::ident().map(Expr::Name).padded();

        let bind = text::ident()
            .then_ignore(op('='))
            .then(expr.clone())
            .then_ignore(op(';'));

        let _let = text::keyword("let")
            .ignore_then(bind.repeated().collect())
            .then_ignore(text::keyword("in"))
            .then(expr.clone())
            .map(|(let_env, body)| Expr::Let(let_env, Box::new(body)));

        let atom = _let
            .or(int)
            .or(name)
            .or(expr.delimited_by(just('('), just(')')).padded());

        let unary = op('-')
            .repeated()
            .then(atom)
            .foldr(|_, e| Expr::Neg(Box::new(e)));

        // let call =

        let product = unary
            .clone()
            .then(
                op('*')
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(op('/').to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                op('+')
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
        sum
    })
    .then_ignore(end())
}
