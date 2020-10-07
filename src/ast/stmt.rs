use super::Expr;

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
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
}
