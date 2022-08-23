use super::{Block, EnumDef, Fn, Ident, Impl, Let, TypeAlias, UseTree, VariantData, Visibility};
use logos::Span;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Item<K = ItemKind> {
	pub kind: K,
	pub ident: Ident,
	pub vis: Visibility,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ItemKind {
	Enum(EnumDef),
	Struct(VariantData), // Struct data is the same as enum variant data
	Use(UseTree),
	Test(Span, Block),
	Impl(Impl),
	Let(Let),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AssocItemKind {
	Fn(Fn),
	TypeAlias(TypeAlias),
}
