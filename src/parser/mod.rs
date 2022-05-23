pub mod lexer;
use lexer::Token;

struct ParseNode {}

struct Parser;

impl Parser {
    pub fn parse(input: Vec<char>) {
        let mut lexer = lexer::Lexer::new(input);
        let mut current_token = lexer.get_token();
        while current_token != Token::EOF {}
    }
}
