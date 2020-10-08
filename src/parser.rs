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
pub enum SyntaxError {
    ExpectExpression(BoxToken),
    ExpectLeftParen(BoxToken),
    ExpectRightParen(BoxToken),
    ExpectRightBrace(BoxToken),
    ExpectSemicolon(BoxToken),
    ExpectIdentifier(BoxToken),
    InvalidAssignTarget(BoxToken),
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
            SyntaxError::ExpectIdentifier(t) => {
                write_position(f, t)?;
                write!(f, "expect an identifier, but '{:?}' found", t)
            }
            SyntaxError::InvalidAssignTarget(t) => {
                write_position(f, t)?;
                write!(f, "invalid assignment target")
            }
        }
    }
}

pub struct Parser<'a> {
    errors: Vec<SyntaxError>,
    peeked: Option<Token>,
    tokens: Scanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner) -> Parser {
        Parser {
            errors: Vec::new(),
            peeked: None,
            tokens: scanner,
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

    fn box_current_token(&mut self) -> BoxToken {
        if self.peek().is_some() {
            Box::new(self.peeked.take().unwrap())
        } else {
            Box::new(Token::new(Position::default(), TokenKind::EOF))
        }
    }

    fn advance_if_eq(&mut self, kind: &TokenKind) -> Option<Token> {
        if let Some(token) = self.peek() {
            if &token.kind == kind {
                return self.advance();
            }
        }
        None
    }

    fn advance_if_contains(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        if let Some(t) = self.peek() {
            for kind in kinds {
                if &t.kind == kind {
                    return self.advance();
                }
            }
        }
        None
    }

    fn consume_or_err(&mut self, kind: &TokenKind) -> ParseResult<()> {
        if self.advance_if_eq(kind).is_some() {
            Ok(())
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

    fn unary(&mut self) -> ParseResult<Expr> {
        if let Some(op) =
            self.advance_if_contains(&[TokenKind::MINUS, TokenKind::BANG])
        {
            let right = self.unary()?;
            Ok(Expr::new_unary(op, right))
        } else {
            self.primary()
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
            let right = self.comparison()?;
            left = Expr::new_logical(op, left, right);
        }
        Ok(left)
    }

    fn or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.and_expr()?;
        while let Some(op) = self.advance_if_eq(&TokenKind::OR) {
            let right = self.comparison()?;
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

        while let Some(tk) = self.peek() {
            // if there is a missing right brace,
            // the SyntaxError will be reported at EOF, akward.
            if TokenKind::RIGHT_BRACE == tk.kind {
                break;
            }
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
        let taken = self.statement()?;
        if self.advance_if_eq(&TokenKind::ELSE).is_some() {
            Ok(Stmt::new_if(cond, taken, Some(self.statement()?)))
        } else {
            Ok(Stmt::new_if(cond, taken, None))
        }
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        if self.advance_if_eq(&TokenKind::PRINT).is_some() {
            return self.print_stmt();
        }
        if self.advance_if_eq(&TokenKind::IF).is_some() {
            return self.if_statement();
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
            Err(SyntaxError::ExpectIdentifier(self.box_current_token()))
        }
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        if self.advance_if_eq(&TokenKind::VAR).is_some() {
            return self.var_decl_stmt();
        }
        self.statement()
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
