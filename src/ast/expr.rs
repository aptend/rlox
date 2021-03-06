use crate::scanner::Token;

type ExprKey = u64;
pub enum Expr {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Logical(LogicalExpr),
    Grouping(Grouping),
    Literal(Literal),
    Variable(VariableExpr),
    Assign(AssignExpr),
    Call(CallExpr),
    Get(GetExpr),
    Set(SetExpr),
    This(ThisExpr),
    Super(SuperExpr),
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

    pub fn new_variable(expr_key: u64, token: Token) -> Expr {
        Expr::Variable(VariableExpr {
            expr_key,
            name: token,
        })
    }

    pub fn new_assign(expr_key: u64, name: Token, value: Expr) -> Expr {
        Expr::Assign(AssignExpr {
            expr_key,
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

    pub fn new_get(object: Expr, name: Token) -> Expr {
        Expr::Get(GetExpr {
            object: Box::new(object),
            name,
        })
    }

    pub fn new_set(object: Expr, name: Token, value: Expr) -> Expr {
        Expr::Set(SetExpr {
            object: Box::new(object),
            name,
            value: Box::new(value),
        })
    }

    pub fn new_this(expr_key: u64, this_tk: Token) -> Expr {
        Expr::This(ThisExpr { expr_key, this_tk })
    }

    pub fn new_super(expr_key: u64, super_tk: Token, method: Token) -> Expr {
        Expr::Super(SuperExpr {
            expr_key,
            super_tk,
            method,
        })
    }

    pub fn is_nil(expr: &Expr) -> bool {
        match expr {
            Expr::Literal(Literal::Nil) => true,
            _ => false,
        }
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

// expr_key is used for variable resovling,
// refer to the comments in Resovler::resolve_local
pub struct VariableExpr {
    pub expr_key: ExprKey,
    pub name: Token,
}

pub struct AssignExpr {
    pub expr_key: ExprKey,
    pub name: Token,
    pub value: Box<Expr>,
}

pub struct CallExpr {
    pub callee: Box<Expr>,
    pub pos_tk: Token,
    pub arguments: Vec<Expr>,
}

pub struct GetExpr {
    pub object: Box<Expr>,
    pub name: Token,
}

pub struct SetExpr {
    pub object: Box<Expr>,
    pub name: Token,
    pub value: Box<Expr>,
}

pub struct ThisExpr {
    pub expr_key: ExprKey,
    pub this_tk: Token,
}

pub struct SuperExpr {
    pub expr_key: ExprKey,
    pub super_tk: Token,
    pub method: Token,
}
