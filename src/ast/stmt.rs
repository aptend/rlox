use super::Expr;
use crate::scanner::Token;
use std::ops::Deref;
use std::rc::Rc;
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(VariableStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    Function(FunctionStmt),
    Return(ReturnStmt),
    Class(ClassStmt),
    Break,
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
            no_token: no_taken.map(Box::new),
        })
    }

    pub fn new_while(cond: Expr, body: Stmt) -> Stmt {
        Stmt::While(WhileStmt {
            cond,
            body: Box::new(body),
        })
    }

    pub fn new_break() -> Stmt {
        Stmt::Break
    }

    pub fn new_return(ret_tk: Token, value: Expr) -> Stmt {
        Stmt::Return(ReturnStmt { ret_tk, value })
    }

    pub fn new_function(
        name: Token,
        params: Vec<Token>,
        body: BlockStmt,
    ) -> Stmt {
        Stmt::Function(FunctionStmt {
            inner: Rc::new(FuncInner {
                name,
                params,
                body: Box::new(body),
            }),
        })
    }

    pub fn new_class(
        name: Token,
        superclass: Option<Expr>,
        methods: Vec<FunctionStmt>,
    ) -> Stmt {
        Stmt::Class(ClassStmt {
            name,
            superclass,
            methods,
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

pub struct WhileStmt {
    pub cond: Expr,
    pub body: Box<Stmt>,
}

pub struct ReturnStmt {
    pub ret_tk: Token,
    pub value: Expr,
}

// Function's body will be stored in environment for lazy computation
// which means it will exist in two places simultaneously. We need a cheap
// cloning way.
//
// Another way might be lifetime, interpreter goes after AST.
// But lifetime approach involves so many structs,
// like Callable<'ast>, Value<'ast>, Environment<'ast> and so on.
// Life is short, anyway.
#[derive(Clone)]
pub struct FunctionStmt {
    inner: Rc<FuncInner>,
}

impl Deref for FunctionStmt {
    type Target = FuncInner;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
pub struct FuncInner {
    pub name: Token,
    pub params: Vec<Token>,
    // represent body by Stmt::Block,
    pub body: Box<BlockStmt>,
}

pub struct ClassStmt {
    pub name: Token,
    pub superclass: Option<Expr>, // VariableExpr
    pub methods: Vec<FunctionStmt>,
}
