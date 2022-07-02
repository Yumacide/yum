use std::{fs, str};

use clap::Parser;
use logos::Logos;

use crate::ast::lexer::Token;

mod ast;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
	// The name of the file to compile
	#[clap(value_parser)]
	filename: String,
}

fn main() {
	let args = Args::parse();
	let src = fs::read(args.filename).unwrap();
	let src = str::from_utf8(&src).unwrap();

	let lex = Token::lexer(src);
	lex.for_each(|x| println!("{:?}", x));
}
