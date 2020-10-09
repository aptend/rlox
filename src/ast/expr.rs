use crate::scanner::Token;
pub enum Expr {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Logical(LogicalExpr),
    Grouping(Grouping),
    Literal(Literal),
    Variable(VariableExpr),
    Assign(AssignExpr),
    Call(CallExpr),
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

    pub fn new_logical(op: Token, left: Expr, right: Expr) -> Expr {
        Expr::Logical(LogicalExpr {
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
        Expr::Variable(VariableExpr { name: token })
    }

    pub fn new_assign(name: Token, value: Expr) -> Expr {
        Expr::Assign(AssignExpr {
            name,
            value: Box::new(value),
        })
    }

    pub fn new_call(callee: Expr, paren: Token, arguments: Vec<Expr>) -> Expr {
        Expr::Call(CallExpr {
            callee: Box::new(callee),
            pos_tk: paren,
            arguments,
        })
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

pub struct LogicalExpr {
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
    pub name: Token,
}

pub struct AssignExpr {
    pub name: Token,
    pub value: Box<Expr>,
}

pub struct CallExpr {
    pub callee: Box<Expr>,
    pub pos_tk: Token,
    pub arguments: Vec<Expr>,
}
