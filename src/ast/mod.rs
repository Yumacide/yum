use logos::Span;

pub mod item;
pub mod lexer;
pub mod parser;

#[derive(PartialEq, Debug, Clone)]
pub struct EnumDef {
	pub variants: Vec<Variant>,
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
	pub vis: Visibility,
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

#[derive(PartialEq, Debug, Clone)]
pub struct Visibility {
	pub kind: VisKind,
}

#[derive(PartialEq, Debug, Clone)]
pub enum VisKind {
	Public,
	Restricted { path: Path },
	Inherited,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Path {
	segments: Vec<PathSegment>,
}

pub enum PathStyle {
	Expr,
	Type,
	Mod,
}

#[derive(PartialEq, Debug, Clone)]
pub struct PathSegment {
	pub ident: Ident,
}
