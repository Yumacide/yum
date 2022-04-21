use std::io;

mod lexer;

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Failed to read stdin");

    println!("{:?}", lexer::lex(input));
}
