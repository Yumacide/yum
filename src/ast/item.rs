use super::{EnumDef, Ident, UseTree, VariantData, Visibility};

#[derive(PartialEq, Debug, Clone)]
pub struct Item {
	pub kind: ItemKind,
	pub ident: Ident,
	pub vis: Visibility,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ItemKind {
	Enum(EnumDef),
	Struct(VariantData), // Struct data is the same as enum variant data
	Use(UseTree),
}
