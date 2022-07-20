use super::Parser;
use crate::ast::{
	item::{AssocItemKind, Item, ItemKind},
	lexer::Token,
	Block, EnumDef, FieldDef, Fn, Ident, Impl, Path, PathSegment, Type, TypeAlias, TypeKind,
	UseTree, UseTreeKind, Variant, VariantData, VisKind, Visibility,
};
use logos::Logos;

#[test]
fn parse_enum() {
	let src = "enum Result { Ok, Err }";
	let tokens = Token::lexer(src).spanned().collect();
	let mut parser = Parser::new(tokens, src);
	let item = parser.parse_item().unwrap().unwrap();
	assert_eq!(
		item,
		Item {
			kind: ItemKind::Enum(EnumDef {
				variants: vec![
					Variant {
						span: 14..16,
						vdata: VariantData::Unit
					},
					Variant {
						span: 18..21,
						vdata: VariantData::Unit
					}
				],
			}),
			ident: Ident { span: 5..11 },
			vis: Visibility {
				kind: VisKind::Inherited
			}
		}
	);
}

#[test]
fn parse_struct() {
	let src = "struct Foo { bar: number, baz: str }";
	let tokens = Token::lexer(src).spanned().collect();
	let mut parser = Parser::new(tokens, src);
	let kind = parser.parse_item_kind().unwrap().unwrap();
	assert_eq!(
		kind.1,
		ItemKind::Struct(VariantData::Struct(vec![
			FieldDef {
				vis: Visibility {
					kind: VisKind::Inherited
				},
				ident: Ident { span: 13..16 },
				ty: Type {
					kind: TypeKind::Path(Path {
						segments: vec![PathSegment {
							ident: Ident { span: 18..24 }
						}]
					})
				}
			},
			FieldDef {
				vis: Visibility {
					kind: VisKind::Inherited
				},
				ident: Ident { span: 26..29 },
				ty: Type {
					kind: TypeKind::Path(Path {
						segments: vec![PathSegment {
							ident: Ident { span: 31..34 }
						}]
					})
				}
			}
		]))
	);
}

#[test]
fn parse_visibility() {
	let vises = [
		Visibility {
			kind: VisKind::Restricted {
				path: Path {
					segments: vec![PathSegment {
						ident: Ident { span: 4..9 },
					}],
				},
			},
		},
		Visibility {
			kind: VisKind::Public,
		},
		Visibility {
			kind: VisKind::Inherited,
		},
		Visibility {
			kind: VisKind::Restricted {
				path: Path {
					segments: vec![
						PathSegment {
							ident: Ident { span: 7..12 },
						},
						PathSegment {
							ident: Ident { span: 14..17 },
						},
					],
				},
			},
		},
	];
	for (i, src) in ["pub(crate)", "pub", "", "pub(in crate::ast)"]
		.iter()
		.enumerate()
	{
		let tokens = Token::lexer(src).spanned().collect();
		let mut parser = Parser::new(tokens, src);
		assert_eq!(parser.parse_visibility().unwrap(), vises[i])
	}
}

#[test]
fn parse_use_item() {
	let items = [
		Item {
			kind: ItemKind::Use(UseTree {
				prefix: Path {
					segments: vec![PathSegment {
						ident: Ident { span: 8..11 },
					}],
				},
				kind: UseTreeKind::Simple(Some(Ident { span: 15..16 })),
			}),
			ident: Ident::empty(),
			vis: Visibility {
				kind: VisKind::Public,
			},
		},
		Item {
			kind: ItemKind::Use(UseTree {
				kind: UseTreeKind::Simple(None),
				prefix: Path {
					segments: vec![
						PathSegment {
							ident: Ident { span: 4..9 },
						},
						PathSegment {
							ident: Ident { span: 11..14 },
						},
					],
				},
			}),
			ident: Ident::empty(),
			vis: Visibility {
				kind: VisKind::Inherited,
			},
		},
		Item {
			kind: ItemKind::Use(UseTree {
				kind: UseTreeKind::Nested(vec![
					UseTree {
						kind: UseTreeKind::Simple(None),
						prefix: Path {
							segments: vec![PathSegment {
								ident: Ident { span: 10..14 },
							}],
						},
					},
					UseTree {
						kind: UseTreeKind::Simple(None),
						prefix: Path {
							segments: vec![PathSegment {
								ident: Ident { span: 16..22 },
							}],
						},
					},
				]),
				prefix: Path {
					segments: vec![PathSegment {
						ident: Ident { span: 4..7 },
					}],
				},
			}),
			ident: Ident::empty(),
			vis: Visibility {
				kind: VisKind::Inherited,
			},
		},
		Item {
			kind: ItemKind::Use(UseTree {
				kind: UseTreeKind::Glob,
				prefix: Path {
					segments: vec![PathSegment {
						ident: Ident { span: 4..7 },
					}],
				},
			}),
			ident: Ident::empty(),
			vis: Visibility {
				kind: VisKind::Inherited,
			},
		},
	];
	for (i, src) in [
		"pub use ast as a;",
		"use crate::ast;",
		"use ast::{item, parser};",
		"use ast::*;",
	]
	.iter()
	.enumerate()
	{
		let tokens = Token::lexer(src).spanned().collect();
		let mut parser = Parser::new(tokens, src);
		assert_eq!(parser.parse_item().unwrap().unwrap(), items[i])
	}
}

#[test]
fn parse_test() {
	let src = "test \"Ensure that things work\" {}";
	let tokens = Token::lexer(src).spanned().collect();
	let mut parser = Parser::new(tokens, src);
	let test = parser.parse_item_kind().unwrap().unwrap().1;

	assert_eq!(test, ItemKind::Test(5..30, Block { stmts: vec![] }));
}

#[test]
fn parse_impl() {
	let src = "impl Foo for Bar { type A = B; fn baz() {} }";
	let tokens = Token::lexer(src).spanned().collect();
	let mut parser = Parser::new(tokens, src);
	let kind = parser.parse_item_kind().unwrap().unwrap().1;
	assert_eq!(
		kind,
		ItemKind::Impl(Impl {
			trait_path: Some(Path {
				segments: vec![PathSegment {
					ident: Ident { span: 13..16 }
				}]
			}),
			ty: Type {
				kind: TypeKind::Path(Path {
					segments: vec![PathSegment {
						ident: Ident { span: 5..8 }
					}]
				})
			},
			items: vec![
				Item {
					kind: AssocItemKind::TypeAlias(TypeAlias {
						left_ty: Type {
							kind: TypeKind::Path(Path {
								segments: vec![PathSegment {
									ident: Ident { span: 24..25 }
								}]
							})
						},
						right_ty: Some(Type {
							kind: TypeKind::Path(Path {
								segments: vec![PathSegment {
									ident: Ident { span: 28..29 }
								}]
							})
						})
					}),
					ident: Ident::empty(),
					vis: Visibility {
						kind: VisKind::Inherited
					}
				},
				Item {
					kind: AssocItemKind::Fn(Fn {
						args: vec![],
						ret_ty: Type {
							kind: TypeKind::Unit
						},
						body: Some(Block { stmts: vec![] })
					}),
					ident: Ident { span: 34..37 },
					vis: Visibility {
						kind: VisKind::Inherited
					}
				},
			]
		})
	)
}
