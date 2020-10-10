// expression -> assignment;
// assignment -> IDENT "=" assignment | equality // right-associated
// equality   -> comparison (("==" | "!=") comparison)*;
// comparison -> term (("<" | "<=" | ">" | ">=") term)*;
// term       -> factor (("+" | "-") factor)*;
// factor     -> unary (("*" | "/") unary)*;
// unary      -> ("-" | "!") unary | primary;
// primary    -> LITERALs | "(" expression ")";

use std::fmt;

use crate::ast::{Expr, Stmt};
use crate::scanner::*;

type BoxToken = Box<Token>;
type ParseResult<T> = Result<T, SyntaxError>;

#[derive(Debug)]
pub enum SynCxt {
    VarDecl,
    FunDecl,
    MethodDecl,
}

impl fmt::Display for SynCxt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SynCxt::VarDecl => write!(f, "variable declaration"),
            SynCxt::FunDecl => write!(f, "function declaration"),
            SynCxt::MethodDecl => write!(f, "method declaration"),
        }
    }
}

#[derive(Debug)]
pub enum SyntaxError {
    ExpectExpression(BoxToken),
    ExpectLeftParen(BoxToken),
    ExpectRightParen(BoxToken),
    ExpectRightBrace(BoxToken),
    ExpectSemicolon(BoxToken),
    ExpectIdentifier(BoxToken, SynCxt),
    InvalidAssignTarget(BoxToken),
    BreakOutside(BoxToken),
    TooManyArguments(BoxToken),
    LexError(ScanError),
}

impl std::convert::From<ScanError> for SyntaxError {
    fn from(err: ScanError) -> Self {
        SyntaxError::LexError(err)
    }
}

fn write_position(f: &mut fmt::Formatter<'_>, token: &Token) -> fmt::Result {
    if token.kind == TokenKind::EOF {
        write!(f, "[the last line] SyntaxError: ")
    } else {
        let (line, col) = (token.position.line, token.position.column);
        write!(f, "[line {}, column {}] SyntaxError: ", line, col)
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxError::LexError(err) => write!(f, "{:?}", err),
            SyntaxError::ExpectExpression(t) => {
                write_position(f, t)?;
                write!(f, "expect an expression, but '{:?}' found", t)
            }
            SyntaxError::ExpectLeftParen(t) => {
                write_position(f, t)?;
                write!(f, "expect an opening parenthesis, but '{:?}' found", t)
            }
            SyntaxError::ExpectRightParen(t) => {
                write_position(f, t)?;
                write!(f, "expect an closing parenthesis, but '{:?}' found", t)
            }
            SyntaxError::ExpectRightBrace(t) => {
                write_position(f, t)?;
                write!(
                    f,
                    "expect an closing brace after block, but '{:?}' found",
                    t
                )
            }
            SyntaxError::ExpectSemicolon(t) => {
                write_position(f, t)?;
                write!(f, "expect a ';' after statement, but '{:?}' found", t)
            }
            SyntaxError::ExpectIdentifier(t, cxt) => {
                write_position(f, t)?;
                write!(
                    f,
                    "expect an identifier, required by {}, but '{:?}' found",
                    cxt, t
                )
            }
            SyntaxError::InvalidAssignTarget(t) => {
                write_position(f, t)?;
                write!(f, "invalid assignment target")
            }
            SyntaxError::BreakOutside(t) => {
                write_position(f, t)?;
                write!(f, "'break' can be used in a loop scope only")
            }
            SyntaxError::TooManyArguments(t) => {
                write_position(f, t)?;
                write!(f, "Can't have more than 255 arguments")
            }
        }
    }
}

pub struct Parser<'a> {
    errors: Vec<SyntaxError>,
    peeked: Option<Token>,
    tokens: Scanner<'a>,
    // count nest loop,
    // break; statement is allowed in loop scope
    n_loop: usize,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner) -> Parser {
        Parser {
            errors: Vec::new(),
            peeked: None,
            tokens: scanner,
            n_loop: 0,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        if self.peeked.is_some() {
            self.peeked.take()
        } else {
            // loop to find one valid token
            loop {
                if let Some(res) = self.tokens.next() {
                    match res {
                        Ok(token) => return Some(token),
                        Err(e) => self.errors.push(e.into()),
                    }
                } else {
                    return None;
                }
            }
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = self.advance();
        }
        self.peeked.as_ref()
    }

    fn peek_check<F: FnOnce(&TokenKind) -> bool>(&mut self, f: F) -> bool {
        if let Some(tk) = self.peek() {
            if f(&tk.kind) {
                return true;
            }
        }
        false
    }

    fn box_current_token(&mut self) -> BoxToken {
        if self.peek().is_some() {
            Box::new(self.peeked.take().unwrap())
        } else {
            Box::new(Token::new(Position::default(), TokenKind::EOF))
        }
    }

    fn advance_if_eq(&mut self, kind: &TokenKind) -> Option<Token> {
        if self.peek_check(|k| k == kind) {
            return self.advance();
        }
        None
    }

    fn advance_if_contains(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        if self.peek_check(|k| kinds.contains(k)) {
            return self.advance();
        }
        None
    }

    fn consume_or_err(&mut self, kind: &TokenKind) -> ParseResult<Token> {
        if let Some(tk) = self.advance_if_eq(kind) {
            Ok(tk)
        } else {
            let tk = self.box_current_token();
            match kind {
                TokenKind::SEMICOLON => Err(SyntaxError::ExpectSemicolon(tk)),
                TokenKind::RIGHT_BRACE => {
                    Err(SyntaxError::ExpectRightBrace(tk))
                }

                TokenKind::RIGHT_PAREN => {
                    Err(SyntaxError::ExpectRightParen(tk))
                }

                TokenKind::LEFT_PAREN => Err(SyntaxError::ExpectLeftParen(tk)),

                TokenKind::IDENTIFIER(_) => {
                    Err(SyntaxError::ExpectIdentifier(tk, SynCxt::VarDecl))
                }
                _ => unimplemented!(),
            }
        }
    }

    fn synchronize(&mut self) {
        loop {
            if let Some(token) = self.peek() {
                match &token.kind {
                    TokenKind::SEMICOLON => {
                        self.advance();
                        return;
                    }
                    TokenKind::CLASS
                    | TokenKind::FUN
                    | TokenKind::VAR
                    | TokenKind::FOR
                    | TokenKind::IF
                    | TokenKind::WHILE
                    | TokenKind::PRINT
                    | TokenKind::RETURN => return,
                    _ => {
                        self.advance();
                    }
                }
            } else {
                break;
            }
        }
    }

    /// ----------------------------------------------------------------------
    /// ---- expression recognition ------------------------------------------
    /// ----------------------------------------------------------------------

    fn primary(&mut self) -> ParseResult<Expr> {
        if let Some(t) = self.advance_if_eq(&TokenKind::NUMBER(0.0)) {
            if let TokenKind::NUMBER(f) = t.kind {
                return Ok(Expr::new_number_literal(f));
            }
        }
        if let Some(t) = self.advance_if_eq(&TokenKind::STRING(String::new())) {
            // move string out of token before droping it
            if let TokenKind::STRING(s) = t.kind {
                return Ok(Expr::new_string_literal(s));
            }
        }
        if self.advance_if_eq(&TokenKind::TRUE).is_some() {
            return Ok(Expr::new_bool_literal(true));
        }

        if self.advance_if_eq(&TokenKind::FALSE).is_some() {
            return Ok(Expr::new_bool_literal(false));
        }

        if self.advance_if_eq(&TokenKind::NIL).is_some() {
            return Ok(Expr::default());
        }

        if let Some(t) =
            self.advance_if_eq(&TokenKind::IDENTIFIER(String::new()))
        {
            // move this token into expr
            return Ok(Expr::new_variable(t));
        }

        if self.advance_if_eq(&TokenKind::LEFT_PAREN).is_some() {
            let expr = self.expression()?;
            self.consume_or_err(&TokenKind::RIGHT_PAREN)?;
            return Ok(Expr::new_grouping(expr));
        }
        Err(SyntaxError::ExpectExpression(self.box_current_token()))
    }

    fn call(&mut self) -> ParseResult<Expr> {
        let mut callee = self.primary()?;

        loop {
            if self.advance_if_eq(&TokenKind::LEFT_PAREN).is_some() {
                callee = self.finish_call(callee)?;
            } else {
                break;
            }
        }

        Ok(callee)
    }

    fn finish_call(&mut self, mut callee: Expr) -> ParseResult<Expr> {
        let mut arguments = vec![];
        if self.peek_check(|k| k != &TokenKind::RIGHT_PAREN) {
            // collect all arguments
            loop {
                if arguments.len() >= 255 {
                    // don't bail out here,
                    // just record SyntaxError and move on
                    let tk = self.box_current_token();
                    self.errors.push(SyntaxError::TooManyArguments(tk));
                }
                arguments.push(self.expression()?);
                if self.advance_if_eq(&TokenKind::COMMA).is_none() {
                    break;
                }
            }
        }
        let paren = self.consume_or_err(&TokenKind::RIGHT_PAREN)?;

        callee = Expr::new_call(callee, paren, arguments);
        Ok(callee)
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        if let Some(op) =
            self.advance_if_contains(&[TokenKind::MINUS, TokenKind::BANG])
        {
            let right = self.unary()?;
            Ok(Expr::new_unary(op, right))
        } else {
            self.call()
        }
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        let mut left = self.unary()?;
        while let Some(op) =
            self.advance_if_contains(&[TokenKind::STAR, TokenKind::SLASH])
        {
            let right = self.unary()?;
            left = Expr::new_binary(op, left, right);
        }
        Ok(left)
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let mut left = self.factor()?;
        while let Some(op) =
            self.advance_if_contains(&[TokenKind::PLUS, TokenKind::MINUS])
        {
            let right = self.factor()?;
            left = Expr::new_binary(op, left, right);
        }
        Ok(left)
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        let mut left = self.term()?;
        while let Some(op) = self.advance_if_contains(&[
            TokenKind::LESS,
            TokenKind::LESS_EQUAL,
            TokenKind::GREATER,
            TokenKind::GREATER_EQUAL,
        ]) {
            let right = self.term()?;
            left = Expr::new_binary(op, left, right);
        }
        Ok(left)
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut left = self.comparison()?;
        while let Some(op) = self.advance_if_contains(&[
            TokenKind::EQUAL_EQUAL,
            TokenKind::BANG_EQUAL,
        ]) {
            let right = self.comparison()?;
            left = Expr::new_binary(op, left, right);
        }
        Ok(left)
    }

    fn and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.equality()?;
        while let Some(op) = self.advance_if_eq(&TokenKind::AND) {
            let right = self.equality()?;
            left = Expr::new_logical(op, left, right);
        }
        Ok(left)
    }

    fn or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.and_expr()?;
        while let Some(op) = self.advance_if_eq(&TokenKind::OR) {
            let right = self.and_expr()?;
            left = Expr::new_logical(op, left, right);
        }
        Ok(left)
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        // parse left-value, it is also an expression, like x.y = 42
        // we check if it is valid later
        let l_value = self.or_expr()?;
        if let Some(equal_tk) = self.advance_if_eq(&TokenKind::EQUAL) {
            if let Expr::Variable(v) = l_value {
                // it is a valid assignment, so we continue to
                // parse right value recursively.
                let value = self.assignment()?;
                return Ok(Expr::new_assign(v.name, value));
            } else {
                return Err(SyntaxError::InvalidAssignTarget(Box::new(
                    equal_tk,
                )));
            }
        }
        Ok(l_value)
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    /// ----------------------------------------------------------------------
    /// ---- statement recognition ------------------------------------------
    /// ----------------------------------------------------------------------

    fn print_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume_or_err(&TokenKind::SEMICOLON)?;
        Ok(Stmt::new_print(expr))
    }

    fn expression_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume_or_err(&TokenKind::SEMICOLON)?;
        Ok(Stmt::new_expr(expr))
    }

    fn block_stmt(&mut self) -> ParseResult<Stmt> {
        let mut stmts = Vec::new();

        // if there is a missing right brace,
        // the SyntaxError will be reported at EOF, akward.
        while self.peek_check(|k| k != &TokenKind::RIGHT_BRACE) {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }

        self.consume_or_err(&TokenKind::RIGHT_BRACE)?;
        Ok(Stmt::new_block(stmts))
    }

    fn if_statement(&mut self) -> ParseResult<Stmt> {
        self.consume_or_err(&TokenKind::LEFT_PAREN)?;
        let cond = self.expression()?;
        self.consume_or_err(&TokenKind::RIGHT_PAREN)?;
        let taken = self.breakable_stmt()?;
        if self.advance_if_eq(&TokenKind::ELSE).is_some() {
            Ok(Stmt::new_if(cond, taken, Some(self.breakable_stmt()?)))
        } else {
            Ok(Stmt::new_if(cond, taken, None))
        }
    }

    fn while_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume_or_err(&TokenKind::LEFT_PAREN)?;
        let cond = self.expression()?;
        self.consume_or_err(&TokenKind::RIGHT_PAREN)?;
        self.n_loop += 1;
        let body = self.breakable_stmt()?;
        self.n_loop -= 1;
        Ok(Stmt::new_while(cond, body))
    }

    fn for_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume_or_err(&TokenKind::LEFT_PAREN)?;

        let initial = if self.advance_if_eq(&TokenKind::SEMICOLON).is_some() {
            None
        } else if self.advance_if_eq(&TokenKind::VAR).is_some() {
            Some(self.var_decl_stmt()?)
        } else {
            Some(self.expression_stmt()?)
        };

        let condition = if self.advance_if_eq(&TokenKind::SEMICOLON).is_some() {
            Expr::new_bool_literal(true)
        } else {
            let cond = self.expression()?;
            self.consume_or_err(&TokenKind::SEMICOLON)?;
            cond
        };

        let increment = if self.advance_if_eq(&TokenKind::RIGHT_PAREN).is_some()
        {
            None
        } else {
            let inc = self.expression()?;
            self.consume_or_err(&TokenKind::RIGHT_PAREN)?;
            Some(inc)
        };

        self.n_loop += 1;
        let mut body = self.breakable_stmt()?;
        self.n_loop -= 1;

        // assemble body and increment
        if let Some(inc_expr) = increment {
            body = Stmt::new_block(vec![body, Stmt::new_expr(inc_expr)]);
        }

        body = Stmt::new_while(condition, body);

        if let Some(init) = initial {
            body = Stmt::new_block(vec![init, body]);
        }

        Ok(body)
    }

    fn breakable_stmt(&mut self) -> ParseResult<Stmt> {
        if let Some(brk_tk) = self.advance_if_eq(&TokenKind::BREAK) {
            if self.n_loop > 0 {
                self.consume_or_err(&TokenKind::SEMICOLON)?;
                return Ok(Stmt::new_break());
            } else {
                return Err(SyntaxError::BreakOutside(Box::new(brk_tk)));
            }
        }
        self.statement()
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        if self.advance_if_eq(&TokenKind::PRINT).is_some() {
            return self.print_stmt();
        }
        if self.advance_if_eq(&TokenKind::IF).is_some() {
            return self.if_statement();
        }
        if self.advance_if_eq(&TokenKind::WHILE).is_some() {
            return self.while_stmt();
        }
        if self.advance_if_eq(&TokenKind::FOR).is_some() {
            return self.for_stmt();
        }
        if self.advance_if_eq(&TokenKind::LEFT_BRACE).is_some() {
            return self.block_stmt();
        }
        self.expression_stmt()
    }

    fn var_decl_stmt(&mut self) -> ParseResult<Stmt> {
        if let Some(token) =
            self.advance_if_eq(&TokenKind::IDENTIFIER(String::new()))
        {
            let mut init = Expr::default();
            if self.advance_if_eq(&TokenKind::EQUAL).is_some() {
                init = self.expression()?;
            }
            self.consume_or_err(&TokenKind::SEMICOLON)?;
            Ok(Stmt::new_variable(token, init))
        } else {
            Err(SyntaxError::ExpectIdentifier(
                self.box_current_token(),
                SynCxt::VarDecl,
            ))
        }
    }

    fn fun_decl_stmt(&mut self, cxt: SynCxt) -> ParseResult<Stmt> {
        let ident_kind = TokenKind::IDENTIFIER(String::new());
        let name = self.consume_or_err(&ident_kind)?;
        self.consume_or_err(&TokenKind::LEFT_PAREN)?;

        // do the same thing in finish_call
        let mut params = vec![];
        if self.peek_check(|k| k != &TokenKind::RIGHT_PAREN) {
            loop {
                if params.len() >= 255 {
                    let tk = self.box_current_token();
                    self.errors.push(SyntaxError::TooManyArguments(tk));
                }
                params.push(self.consume_or_err(&ident_kind)?);
                if self.advance_if_eq(&TokenKind::COMMA).is_none() {
                    break;
                }
            }
        }
        self.consume_or_err(&TokenKind::RIGHT_PAREN)?;
        self.consume_or_err(&TokenKind::LEFT_BRACE)?;
        let body = self.block_stmt()?;
        Ok(Stmt::new_function(name, params, body))
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        if self.advance_if_eq(&TokenKind::VAR).is_some() {
            return self.var_decl_stmt();
        }
        if self.advance_if_eq(&TokenKind::FUN).is_some() {
            return self.fun_decl_stmt(SynCxt::FunDecl);
        }
        self.breakable_stmt()
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<SyntaxError>> {
        let mut stmts = Vec::new();
        while self.peek().is_some() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }
        if !self.errors.is_empty() {
            Err(std::mem::take(&mut self.errors))
        } else {
            Ok(stmts)
        }
    }
}
