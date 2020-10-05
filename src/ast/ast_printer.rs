use super::expr::*;

pub trait AstPrint {
    fn print_ast(&self) -> String;
}

impl AstPrint for Literal {
    fn print_ast(&self) -> String {
        let mut s = String::new();
        match self {
            Literal::Boolean(true) => s.push_str("true"),
            Literal::Boolean(false) => s.push_str("false"),
            Literal::Nil => s.push_str("nil"),
            Literal::Number(val) => s.push_str(format!("{}", val).as_str()),
            Literal::String(val) => s.push_str(val),
        }
        s
    }
}

impl AstPrint for Expr {
    fn print_ast(&self) -> String {
        match self {
            Expr::Literal(v) => v.print_ast(),
            Expr::Unary(v) => v.print_ast(),
            Expr::Binary(v) => v.print_ast(),
            Expr::Grouping(v) => v.print_ast(),
        }
    }
}

impl AstPrint for Unary {
    fn print_ast(&self) -> String {
        format!("({:?} {})", self.op, self.right.print_ast())
    }
}

impl AstPrint for Grouping {
    fn print_ast(&self) -> String {
        self.expr.print_ast()
    }
}

impl AstPrint for Binary {
    fn print_ast(&self) -> String {
        format!(
            "({:?} {} {})",
            self.op,
            self.left.print_ast(),
            self.right.print_ast()
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::*;
    #[test]
    fn test_ast_printer() {
        let expression = Expr::Binary(Binary {
            left: Box::new(Expr::Unary(Unary {
                op: Token::with_kind(TokenKind::MINUS),
                right: Box::new(Expr::Literal(Literal::Number(123.0))),
            })),
            op: Token::with_kind(TokenKind::STAR),
            right: Box::new(Expr::Grouping(Grouping {
                expr: Box::new(Expr::Literal(Literal::Number(45.67))),
            })),
        });

        assert_eq!("(* (- 123) 45.67)", expression.print_ast());
    }
}
