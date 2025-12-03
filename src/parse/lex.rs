use crate::parse::Token;

pub fn read(input: impl std::io::Read) -> anyhow::Result<Vec<Token>> {


    let mut reader = utf8_read::Reader::new(input);

    let mut vec = Vec::new();
    let mut iter = reader.into_iter().peekable();
    loop {
        let Some(c) = iter.next() else { break; };
        let c = c?;
        
        match c {
            '\n' => vec.push(Token::NL),
            // has to be after \n cuz \n is whitespace, and we want to track it
            c if c.is_whitespace() => continue,
            ';' => vec.push(Token::Semi),
            '(' => vec.push(Token::OpenParen),
            ')' => vec.push(Token::CloseParen),
            '{' => vec.push(Token::OpenBrace),
            '}' => vec.push(Token::CloseBrace),
            '[' => vec.push(Token::OpenBracket),
            ']' => vec.push(Token::CloseBracket),
            '+' => vec.push(Token::Plus),
            '-' => vec.push(Token::Minus),
            '*' => vec.push(Token::Star),
            '/' => vec.push(Token::Slash),
            '%' => vec.push(Token::Percent),
            '!' => vec.push(Token::Bang),
            '<' => vec.push(Token::Lt),
            '>' => vec.push(Token::Gt),
            '=' => vec.push(Token::Eq),
            '&' => vec.push(Token::And),
            '|' => vec.push(Token::Or),
            '^' => vec.push(Token::Caret),
            '.' => vec.push(Token::Dot),
            ',' => vec.push(Token::Comma),
            _ => todo!(),
        }
    }
    vec.push(Token::Eof);






    Ok(vec)
}