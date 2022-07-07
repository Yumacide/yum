use ast::parser::Parser;
use clap::Parser as ArgParser;
use std::{fs, str};

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

	let mut parser = Parser::new(src);
	parser.parse_mod().unwrap();
}
