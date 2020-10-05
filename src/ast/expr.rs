use crate::scanner::Token;
pub enum Expr {
    Unary(Unary),
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
}

pub struct Unary {
    pub op: Token,
    pub right: Box<Expr>,
}

pub struct Binary {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

pub struct Grouping {
    pub expr: Box<Expr>,
}

pub enum Literal {
    Nil,
    String(String),
    Number(f64),
    Boolean(bool),
}
