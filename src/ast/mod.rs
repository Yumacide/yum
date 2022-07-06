use logos::Span;

pub mod item;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct EnumDef {
	pub variants: Vec<Variant>,
}

impl PartialEq for EnumDef {
	fn eq(&self, other: &Self) -> bool {
		self.variants == other.variants
	}
}

#[derive(PartialEq, Debug, Clone)]
pub struct Variant {
	pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Ident {
	pub span: Span,
}
