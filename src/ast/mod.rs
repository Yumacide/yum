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
	pub data: VariantData,
}

#[derive(PartialEq, Debug, Clone)]
pub enum VariantData {
	Struct(Vec<FieldDef>),
	Tuple(Vec<FieldDef>),
	Unit,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FieldDef {
	pub ident: Ident,
	pub ty: Type,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Type {
	// TODO
}

#[derive(PartialEq, Debug, Clone)]
pub struct Ident {
	pub span: Span,
}
