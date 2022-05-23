use std::{env, fs};

mod parser;
#[cfg(test)]
mod tests;

use parser::lexer;

fn main() {
    let mut args = env::args();
    args.next();
    let input = String::from_utf8(fs::read(args.next().unwrap()).expect("")).expect("");
    println!("{:?}", lexer::lex(input));
}
