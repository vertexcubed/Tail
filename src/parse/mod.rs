
pub mod lex;


pub enum Token {
    NL,
    Semi,
    // number, i.e. 12351
    // TODO: support binary and hex and octal
    Num(String),
    // identifier
    Ident(String),
    // keyword, as opposed to identifiers
    Keyword(String),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    // operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Bang,
    Lt,
    Gt,
    Eq,
    And,
    Or,
    Caret,
    Dot,
    Comma,
    Eof,
}