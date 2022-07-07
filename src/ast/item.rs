use super::{EnumDef, Ident, VariantData};

#[derive(PartialEq, Debug, Clone)]
pub struct Item {
	pub kind: ItemKind,
	pub ident: Ident,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ItemKind {
	Enum(EnumDef),
	Struct(VariantData), // Struct data is the same as enum variant data
}
