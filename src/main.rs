use std::{env, fs, str};

mod ast;

#[macro_use]
mod macros;

use logos::Logos;

use crate::parser::lexer::Token;

fn main() {
	let mut args = env::args();
	args.next();

	let filename = args.next().unwrap();
	let src = fs::read(filename).unwrap();
	let src = str::from_utf8(&src).unwrap();

	let lex = Token::lexer(src);
	lex.for_each(|x| println!("{:?} foo", x));
	println!("ran");
	/*
	let mut args = env::args();
	args.next();
	let filename = args.next().unwrap();
	Parser::parse(
		fs::read(&filename)
			.unwrap()
			.into_iter()
			.map(|x| x as char)
			.collect(),
		filename,
	);*/
}
