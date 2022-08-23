use clap::Parser;
use cli::{Options, Subcommands};

mod ast;
mod cli;

fn main() -> anyhow::Result<()> {
	let options = Options::parse();
	let result = match options.command {
		Subcommands::Compile(command) => command.run(),
		Subcommands::Build(command) => command.run(),
	};
	result?;
	Ok(())
}
