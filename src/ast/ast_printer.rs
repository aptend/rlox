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
            Expr::Variable(v) => v.print_ast(),
            Expr::Assign(v) => v.print_ast(),
            Expr::Logical(v) => v.print_ast(),
            Expr::Call(v) => v.print_ast(),
            Expr::Get(v) => v.print_ast(),
            Expr::Set(v) => v.print_ast(),
            Expr::Super(v) => v.print_ast(),
            Expr::This(_) => "this".to_string(),
        }
    }
}

impl AstPrint for SuperExpr {
    fn print_ast(&self) -> String {
        format!("super.{:?}", self.method)
    }
}

impl AstPrint for SetExpr {
    fn print_ast(&self) -> String {
        format!(
            "({}.{} <- {})",
            self.object.print_ast(),
            self.name.as_str().unwrap(),
            self.value.print_ast()
        )
    }
}

impl AstPrint for GetExpr {
    fn print_ast(&self) -> String {
        format!(
            "{}.{}",
            self.object.print_ast(),
            self.name.as_str().unwrap()
        )
    }
}

impl AstPrint for CallExpr {
    fn print_ast(&self) -> String {
        let mut s = format!("({}", self.callee.print_ast());
        if self.arguments.is_empty() {
            s.push_str(" nil");
        }
        for expr in &self.arguments {
            s.push(' ');
            s.push_str(expr.print_ast().as_str());
        }
        s.push(')');
        s
    }
}

impl AstPrint for LogicalExpr {
    fn print_ast(&self) -> String {
        format!(
            "({:?} {} {})",
            self.op,
            self.left.print_ast(),
            self.right.print_ast()
        )
    }
}

impl AstPrint for AssignExpr {
    fn print_ast(&self) -> String {
        format!(
            "({} <- {})",
            self.name.as_str().unwrap(),
            self.value.print_ast()
        )
    }
}

impl AstPrint for UnaryExpr {
    fn print_ast(&self) -> String {
        format!("({:?} {})", self.op, self.right.print_ast())
    }
}

impl AstPrint for Grouping {
    fn print_ast(&self) -> String {
        self.expr.print_ast()
    }
}

impl AstPrint for VariableExpr {
    fn print_ast(&self) -> String {
        self.name.as_str().unwrap().to_owned()
    }
}

impl AstPrint for BinaryExpr {
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
        let expression = Expr::new_binary(
            Token::with_kind(TokenKind::STAR),
            Expr::new_unary(
                Token::with_kind(TokenKind::MINUS),
                Expr::new_number_literal(123.0),
            ),
            Expr::new_grouping(Expr::new_number_literal(45.67)),
        );

        assert_eq!("(* (- 123) 45.67)", expression.print_ast());
    }
}
