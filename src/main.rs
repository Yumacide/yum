use ast::{lexer::Token, parser::Parser};
use clap::{arg, Command};
use logos::Logos;
use std::{fs, path::Path, str};

mod ast;

fn main() {
	let matches = Command::new("Yumc")
		.version("0.1.0")
		.author("Yumacide <yumanomics@gmail.com>")
		.about("The compiler for the Yum language")
		.arg(arg!([filename] "File to compile"))
		.get_matches();
	let path_str = matches
		.get_one::<String>("filename")
		.expect("filename is required");

	let path = Path::new(path_str);
	if !path.is_file() {
		panic!("invalid path {}", path_str)
	}

	let src = fs::read(path).unwrap();
	let src = str::from_utf8(&src).unwrap();

	let tokens = Token::lexer(src).spanned().collect();
	let mut parser = Parser::new(tokens, src);
	parser.parse_mod().unwrap();
}
