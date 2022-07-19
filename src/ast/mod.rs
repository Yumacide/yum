// Parts of the AST are adapted from rustc under the MIT license:
// https://github.com/rust-lang/rust

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

impl Ident {
	pub fn empty() -> Self {
		Ident { span: 0..0 }
	}
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

#[derive(PartialEq, Debug, Clone)]
pub struct UseTree {
	pub kind: UseTreeKind,
	pub prefix: Path,
}

#[derive(PartialEq, Debug, Clone)]
pub enum UseTreeKind {
	Simple(Option<Ident>),
	Nested(Vec<UseTree>),
	Glob,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
	stmts: Vec<Stmt>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Stmt {}
