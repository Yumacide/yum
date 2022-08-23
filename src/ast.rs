// Parts of the AST are adapted from rustc under the MIT license:
// https://github.com/rust-lang/rust

use self::item::{AssocItemKind, Item};
use logos::Span;

pub mod item;
pub mod lexer;
pub mod parser;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct EnumDef {
	pub variants: Vec<Variant>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Variant {
	pub span: Span,
	pub vdata: VariantData,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum VariantData {
	Struct(Vec<FieldDef>),
	Tuple(Vec<FieldDef>),
	Unit,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FieldDef {
	pub vis: Visibility,
	pub ident: Ident,
	pub ty: Type,
}

#[derive(PartialEq, Eq, Debug, Clone)]
/// TODO
pub struct Type {
	pub kind: TypeKind,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypeKind {
	Path(Path),
	ImplicitSelf,
	Unit,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Ident {
	pub span: Span,
}

impl Ident {
	pub fn empty() -> Self {
		Ident { span: 0..0 }
	}
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Visibility {
	pub kind: VisKind,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum VisKind {
	Public,
	Restricted { path: Path },
	Inherited,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Path {
	segments: Vec<PathSegment>,
}

pub enum PathStyle {
	Type,
	Mod,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PathSegment {
	pub ident: Ident,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UseTree {
	pub kind: UseTreeKind,
	pub prefix: Path,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UseTreeKind {
	Simple(Option<Ident>),
	Nested(Vec<UseTree>),
	Glob,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Block {
	stmts: Vec<Stmt>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Stmt {
	kind: StmtKind,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum StmtKind {
	Let(Let),
	Item(Item),
	Expr(Expr),
	Semi(Expr),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Let {
	lhs: Pattern,
	rhs: Option<Expr>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Expr {}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Pattern {}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Impl {
	pub trait_path: Option<Path>,
	pub ty: Type,
	pub items: Vec<Item<AssocItemKind>>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Fn {
	args: Vec<Arg>,
	ret_ty: Type,
	body: Option<Block>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypeAlias {
	left_ty: Type,
	right_ty: Option<Type>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Arg {
	ident: Ident,
	ty: Type,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ItemParseMode {
	Mod,
	Stmt,
}
