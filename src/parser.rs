// expression -> equality;
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
    ExpectRightParen(BoxToken),
    ExpectSemicolon(BoxToken),
    ExpectIdentifier(BoxToken),
    LexError(ScanError),
}

impl std::convert::From<ScanError> for SyntaxError {
    fn from(err: ScanError) -> Self {
        SyntaxError::LexError(err)
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let SyntaxError::LexError(err) = self {
            write!(f, "{:?}", err)
        } else {
            let (token, msg) = match self {
                SyntaxError::ExpectExpression(t) => {
                    (Some(t), "expect a expression")
                }
                SyntaxError::ExpectRightParen(t) => {
                    (Some(t), "expect a close parenthese")
                }
                SyntaxError::ExpectSemicolon(t) => {
                    (Some(t), "expect ';' after value")
                }
                SyntaxError::ExpectIdentifier(t) => {
                    (Some(t), "expect an identifier")
                }
                _ => (None, ""),
            };
            let token = token.unwrap();
            if token.kind == TokenKind::EOF {
                write!(f, "[the last line] SyntaxError: {}, but EOF found", msg)
            } else {
                let (line, col) = (token.position.line, token.position.column);
                write!(
                    f,
                    "[line {}, column {}] SyntaxError: {}, but '{:?}' found",
                    line, col, msg, token
                )
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
        if self.peeked.is_some() {
            Box::new(self.peeked.take().unwrap())
        } else {
            Box::new(Token::new(Position::default(), TokenKind::EOF))
        }
    }

    fn advance_if_eq(&mut self, kind: TokenKind) -> Option<Token> {
        if let Some(token) = self.peek() {
            if token.kind == kind {
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

    fn consume_semicolon(&mut self) -> ParseResult<()> {
        if self.advance_if_eq(TokenKind::SEMICOLON).is_some() {
            Ok(())
        } else {
            Err(SyntaxError::ExpectSemicolon(self.box_current_token()))
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

    // ---- expression recognition ----

    fn primary(&mut self) -> ParseResult<Expr> {
        if let Some(t) = self.advance_if_eq(TokenKind::NUMBER(0.0)) {
            if let TokenKind::NUMBER(f) = t.kind {
                return Ok(Expr::new_number_literal(f));
            }
        }
        if let Some(t) = self.advance_if_eq(TokenKind::STRING(String::new())) {
            // move string out of token before droping it
            if let TokenKind::STRING(s) = t.kind {
                return Ok(Expr::new_string_literal(s));
            }
        }
        if self.advance_if_eq(TokenKind::TRUE).is_some() {
            return Ok(Expr::new_bool_literal(true));
        }

        if self.advance_if_eq(TokenKind::FALSE).is_some() {
            return Ok(Expr::new_bool_literal(false));
        }

        if self.advance_if_eq(TokenKind::NIL).is_some() {
            return Ok(Expr::default());
        }

        if let Some(t) =
            self.advance_if_eq(TokenKind::IDENTIFIER(String::new()))
        {
            // move this token into expr
            return Ok(Expr::new_variable(t));
        }

        if self.advance_if_eq(TokenKind::LEFT_PAREN).is_some() {
            let expr = self.expression()?;
            if self.advance_if_eq(TokenKind::RIGHT_PAREN).is_some() {
                return Ok(Expr::new_grouping(expr));
            } else {
                return Err(SyntaxError::ExpectRightParen(
                    self.box_current_token(),
                ));
            }
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

    fn expression(&mut self) -> ParseResult<Expr> {
        self.equality()
    }

    fn print_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume_semicolon()?;
        Ok(Stmt::new_print(expr))
    }

    fn expression_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume_semicolon()?;
        Ok(Stmt::new_expr(expr))
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        if self.advance_if_eq(TokenKind::PRINT).is_some() {
            return self.print_stmt();
        }
        self.expression_stmt()
    }

    fn var_decl_stmt(&mut self) -> ParseResult<Stmt> {
        if let Some(token) =
            self.advance_if_eq(TokenKind::IDENTIFIER(String::new()))
        {
            let mut init = Expr::default();
            if self.advance_if_eq(TokenKind::EQUAL).is_some() {
                init = self.expression()?;
            }
            self.consume_semicolon()?;
            Ok(Stmt::new_variable(token, init))
        } else {
            Err(SyntaxError::ExpectIdentifier(self.box_current_token()))
        }
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        if self.advance_if_eq(TokenKind::VAR).is_some() {
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
