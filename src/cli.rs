mod build;
mod compile;

use clap::{Parser, Subcommand};

use self::{build::BuildSubcommand, compile::CompileSubcommand};

#[derive(Parser)]
pub(crate) struct Options {
	#[clap(subcommand)]
	pub command: Subcommands,
}

#[derive(Subcommand)]
pub(crate) enum Subcommands {
	Build(BuildSubcommand),
	Compile(CompileSubcommand),
}
