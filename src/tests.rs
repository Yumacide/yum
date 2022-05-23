use crate::{parser::lexer};
use lexer::Token;

#[test]
fn lex_arithmetic() {
    assert_eq!(
        lexer::lex("1 *4+12-56/33".to_string()),
        vec![
            Token::NUM(1.0),
            Token::SPACE(String::from(" ")),
            Token::MUL,
            Token::NUM(4.0),
            Token::ADD,
            Token::NUM(12.0),
            Token::SUB,
            Token::NUM(56.0),
            Token::DIV,
            Token::NUM(33.0)
        ]
    )
}
