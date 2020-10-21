use crate::scanner::TokenKind;
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Precedence {
    None,
    Assign,  // =
    Or,      // or
    And,     // and
    Equal,   // ==
    Compare, // > <  >= <=
    Term,    // + -
    Factor,  // * /
    Unary,   // ! -
    Call,    // . ()
    Primary, // literal  grouping
}

impl Precedence {
    pub fn of(kind: Option<&TokenKind>) -> Self {
        match kind {
            Some(&TokenKind::PLUS) | Some(TokenKind::MINUS) => Precedence::Term,
            Some(&TokenKind::STAR) | Some(TokenKind::SLASH) => {
                Precedence::Factor
            }
            Some(&TokenKind::DOT) | Some(TokenKind::LEFT_PAREN) => {
                Precedence::Call
            }
            _ => Precedence::None,
        }
    }

    pub fn next_prec(&self) -> Self {
        match self {
            Precedence::None => Precedence::Assign,
            Precedence::Assign => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equal,
            Precedence::Equal => Precedence::Compare,
            Precedence::Compare => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}
