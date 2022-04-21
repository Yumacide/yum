#[derive(Debug, PartialEq)]
pub enum Token {
    PLUS,
    MINUS,
    ASSIGN,

    IDENT(String),
    NUM(String),
    SPACE(String),

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


pub fn get_keyword_token(ident: &String) -> Option<Token> {
    match ident.as_str() {
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