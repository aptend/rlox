use crate::scanner::Token;
pub enum Expr {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Grouping(Grouping),
    Literal(Literal),
    Variable(VariableExpr),
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
        Expr::Binary(BinaryExpr {
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    pub fn new_unary(op: Token, right: Expr) -> Expr {
        Expr::Unary(UnaryExpr {
            op,
            right: Box::new(right),
        })
    }

    pub fn new_grouping(expr: Expr) -> Expr {
        Expr::Grouping(Grouping {
            expr: Box::new(expr),
        })
    }

    pub fn new_variable(token: Token) -> Expr {
        Expr::Variable(VariableExpr { token })
    }
}

impl std::default::Default for Expr {
    fn default() -> Self {
        Expr::Literal(Literal::Nil)
    }
}

pub struct UnaryExpr {
    pub op: Token,
    pub right: Box<Expr>,
}

pub struct BinaryExpr {
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

pub struct VariableExpr {
    pub token: Token,
}
