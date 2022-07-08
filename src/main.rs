use ast::{parser::Parser, lexer::Token};
use clap::Parser as ArgParser;
use std::{fs, str};
use logos::Logos;

mod ast;

#[derive(ArgParser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
	#[clap(value_parser)]
	filename: String,
}

fn main() {
	let args = Args::parse();
	let src = fs::read(args.filename).unwrap();
	let src = str::from_utf8(&src).unwrap();

	let tokens = Token::lexer(src).spanned().collect();
	let mut parser = Parser::new(tokens, src);
	parser.parse_mod().unwrap();
}
