use crate::scanner::Token;
pub enum Expr {
    Unary(Unary),
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
}

impl Expr {
    pub fn new_string_literal(s: String) -> Expr {
        Expr::Literal(Literal::String(s))
    }

    pub fn new_number_literal(f: f64) -> Expr {
        Expr::Literal(Literal::Number(f))
    }

    pub fn new_bool_literal(b: bool) -> Expr {
        Expr::Literal(Literal::Boolean(b))
    }

    pub fn new_binary(op: Token, left: Expr, right: Expr) -> Expr {
        Expr::Binary(Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    pub fn new_unary(op: Token, right: Expr) -> Expr {
        Expr::Unary(Unary {
            op,
            right: Box::new(right),
        })
    }

    pub fn new_grouping(expr: Expr) -> Expr {
        Expr::Grouping(Grouping {
            expr: Box::new(expr),
        })
    }
}

impl std::default::Default for Expr {
    fn default() -> Self {
        Expr::Literal(Literal::Nil)
    }
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
