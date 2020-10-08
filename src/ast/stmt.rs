use super::Expr;
use crate::scanner::Token;

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(VariableStmt),
    Block(BlockStmt),
}

impl std::default::Default for Stmt {
    fn default() -> Self {
        Stmt::Expression(Expr::default())
    }
}

impl Stmt {
    pub fn new_expr(expr: Expr) -> Stmt {
        Stmt::Expression(expr)
    }
    pub fn new_print(expr: Expr) -> Stmt {
        Stmt::Print(expr)
    }

    pub fn new_variable(name: Token, init: Expr) -> Stmt {
        Stmt::Var(VariableStmt {
            name,
            init: Box::new(init),
        })
    }

    pub fn new_block(stmts: Vec<Stmt>) -> Stmt {
        Stmt::Block(BlockStmt {
            stmts
        })
    }
}

pub struct VariableStmt {
    pub name: Token,
    pub init: Box<Expr>,
}

pub struct BlockStmt {
    pub stmts: Vec<Stmt>
}
