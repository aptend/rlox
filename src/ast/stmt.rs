use super::Expr;
use crate::scanner::Token;

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(VariableStmt),
    Block(BlockStmt),
    If(IfStmt),
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
        Stmt::Var(VariableStmt { name, init })
    }

    pub fn new_block(stmts: Vec<Stmt>) -> Stmt {
        Stmt::Block(BlockStmt { stmts })
    }

    pub fn new_if(cond: Expr, taken: Stmt, no_taken: Option<Stmt>) -> Stmt {
        Stmt::If(IfStmt {
            cond,
            taken: Box::new(taken),
            no_token: no_taken.map(|s| Box::new(s)),
        })
    }
}

pub struct VariableStmt {
    pub name: Token,
    pub init: Expr,
}

pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

pub struct IfStmt {
    pub cond: Expr,
    pub taken: Box<Stmt>,
    pub no_token: Option<Box<Stmt>>,
}
