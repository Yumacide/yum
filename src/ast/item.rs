use super::{EnumDef, Ident};

#[derive(PartialEq, Debug, Clone)]
pub struct Item {
	pub kind: ItemKind,
	pub ident: Ident,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ItemKind {
	Enum(EnumDef),
}
