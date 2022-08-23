use std::{fs, path::PathBuf, str};

use clap::Parser as ArgParser;
use logos::Logos;

use crate::ast::{lexer::Token, parser::Parser};

#[derive(ArgParser)]
pub struct CompileSubcommand {
	#[clap(value_parser)]
	pub path: PathBuf,
}

impl CompileSubcommand {
	pub fn run(self) -> anyhow::Result<()> {
		let src = fs::read(self.path)?;
		let src = str::from_utf8(&src)?;

		let tokens = Token::lexer(src).spanned().collect();
		let mut parser = Parser::new(tokens, src);
		parser.parse_mod()?;

		Ok(())
	}
}
