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
