#[derive(Debug, PartialEq)]
pub enum Token {
    PLUS,
    MINUS,
    ASSIGN,

    IDENT(Vec<char>),
    NUM(Vec<char>),
    SPACE(Vec<char>),

    FUNCTION,
    LOCAL,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    ILLEGAL,
    EOF,
}


pub fn get_keyword_token(ident: &Vec<char>) -> Option<Token> {
    let identifier: String = ident.into_iter().collect();
    match identifier.as_str() {
        "function" => Some(Token::FUNCTION),
        "local" => Some(Token::LOCAL),
        "true" => Some(Token::TRUE),
        "false" => Some(Token::FALSE),
        "if" => Some(Token::IF),
        "else" => Some(Token::ELSE),
        "return" => Some(Token::RETURN),
        _ => None
    }
}