use clap::Parser;

#[derive(Parser)]
pub struct BuildSubcommand {}

impl BuildSubcommand {
	pub fn run(self) -> anyhow::Result<()> {
		todo!()
	}
}
