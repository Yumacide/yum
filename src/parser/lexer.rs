#[derive(Debug, PartialEq)]
pub enum Token {
    ADD,
    SUB,
    MUL,
    DIV,
    EQUAL,

    IDENT(String),
    NUM(f64),
    SPACE(String),

    FUNCTION,
    LET,
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
        "let" => Some(Token::LET),
        "true" => Some(Token::TRUE),
        "false" => Some(Token::FALSE),
        "if" => Some(Token::IF),
        "else" => Some(Token::ELSE),
        "return" => Some(Token::RETURN),
        _ => None,
    }
}

pub struct Lexer {
    input: Vec<char>,
    cursor: usize,
    char: char,
}

impl Lexer {
    pub fn new(input: Vec<char>) -> Self {
        let char = input[0];
        Self {
            input,
            cursor: 0,
            char,
        }
    }

    fn read_while(&mut self, accept: impl Fn(char) -> bool) -> String {
        let start = self.cursor;

        println!("{} {}", self.char, self.cursor);
        while self.cursor < self.input.len() && accept(self.char) {
            self.next_char();
            println!("{} {}", self.char, self.cursor);
        }

        self.input[start..self.cursor].to_vec().iter().collect()
    }

    fn read_ident(&mut self) -> String {
        self.read_while(|current_char| current_char.is_alphanumeric())
    }

    fn read_number(&mut self) -> String {
        self.read_while(|current_char| current_char.is_numeric() || current_char == '.')
    }

    fn read_whitespace(&mut self) -> String {
        self.read_while(|current_char| current_char.is_whitespace())
    }

    pub fn next_char(&mut self) {
        self.cursor += 1;
        if self.cursor >= self.input.len() {
            self.char = '\0';
        } else {
            self.char = self.input[self.cursor];
        }
    }

    pub fn get_token(&mut self) -> Token {
        let current_token = match self.char {
            '+' => Token::ADD,
            '-' => Token::SUB,
            '/' => Token::DIV,
            '*' => Token::MUL,
            '=' => Token::EQUAL,

            c if c.is_numeric() => {
                let num = self.read_number();
                return Token::NUM(num.parse().unwrap());
            }
            c if c.is_alphabetic() => {
                let ident = self.read_ident();
                return match get_keyword_token(&ident) {
                    Some(tok) => tok,
                    None => Token::IDENT(ident),
                };
            }
            c if c.is_whitespace() => {
                let whitespace = self.read_whitespace();
                return Token::SPACE(whitespace);
            }

            '\0' => Token::EOF,
            _ => Token::ILLEGAL,
        };

        self.next_char();
        current_token
    }
}

pub fn lex(input: String) -> Vec<Token> {
    let inputv: Vec<char> = input.chars().collect();

    let mut lexer = Lexer::new(inputv);
    let mut tokenv = Vec::<Token>::new();

    loop {
        let current_token = lexer.get_token();
        if current_token == Token::EOF {
            break;
        }
        tokenv.push(current_token);
    }

    tokenv
}
