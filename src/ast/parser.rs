use super::{
	item::{AssocItemKind, Item, ItemKind},
	lexer::Token,
	Arg, Block, EnumDef, FieldDef, Fn, Ident, Impl, Path, PathSegment, PathStyle, Stmt, Type,
	TypeAlias, TypeKind, UseTree, UseTreeKind, Variant, VariantData, VisKind, Visibility,
};
#[allow(unused_imports)] // logos::Logos implements Token::lexer
use logos::{Logos, Span};

pub struct Parser<'a> {
	pub token: Token,
	pub span: Span,
	pub cursor: usize,
	tokens: Vec<(Token, Span)>,
	src: &'a str,
}

impl<'a> Parser<'a> {
	pub fn new(tokens: Vec<(Token, Span)>, src: &'a str) -> Self {
		let mut parser = Self {
			token: Token::Eof,
			span: 0..0,
			cursor: 0,
			tokens,
			src,
		};
		parser.bump();
		parser
	}

	pub fn bump(&mut self) {
		let next = self.tokens.get(self.cursor);
		self.cursor += 1;
		if next.is_some() {
			let (tok, span) = next.unwrap().clone();
			self.token = tok;
			self.span = span;
		} else {
			self.token = Token::Eof;
			self.span = 0..0;
		}
	}

	pub fn slice(&self) -> &str {
		self.src.get(self.span.clone()).unwrap()
	}

	pub fn line_column(&self) -> String {
		format!(
			"line {} column {}",
			self.src
				.get(0..self.span.end)
				.unwrap()
				.matches("\n")
				.count() + 1,
			self.span.start
				- self
					.src
					.get(0..self.span.end)
					.unwrap()
					.match_indices("\n")
					.last()
					.unwrap_or((1, ""))
					.0
		)
	}

	pub fn expect(&mut self, token: Token) -> Result<(), String> {
		if self.token == token {
			self.bump();
			Ok(())
		} else {
			Err(format!(
				"Expected {:?}, found {:?} '{}'",
				token,
				self.token,
				self.slice()
			))
		}
	}

	pub fn expect_one_of(&mut self, tokens: &[Token]) -> Result<(), String> {
		for token in tokens {
			if self.token == *token {
				self.bump();
				return Ok(());
			}
		}
		Err(format!(
			"Expected one of {:?}, found {:?}",
			tokens, self.token
		))
	}

	pub fn consume(&mut self, token: Token) -> bool {
		let is_present = self.token == token;
		if is_present {
			self.bump();
		}
		is_present
	}

	pub fn check(&self, tok: Token) -> bool {
		self.token == tok
	}

	pub fn peek(&mut self, n: usize) -> &Token {
		let (tok, _) = self
			.tokens
			.get(self.cursor + n - 1)
			.unwrap_or(&(Token::Eof, 0..0));
		tok
	}

	pub fn is_ahead(&mut self, n: usize, toks: &[Token]) -> bool {
		let next_token = self.peek(n);
		for tok in toks {
			if next_token == tok {
				return true;
			}
		}
		return false;
	}

	pub fn parse_mod(&mut self) -> Result<Vec<Item>, String> {
		let mut items = vec![];
		while let Some(item) = self.parse_item()? {
			items.push(item);
		}
		Ok(items)
	}

	pub fn parse_item(&mut self) -> Result<Option<Item>, String> {
		let vis = self.parse_visibility()?;
		if let Some((ident, kind)) = self.parse_item_kind()? {
			Ok(Some(Item { ident, kind, vis }))
		} else {
			Ok(None)
		}
	}

	pub fn parse_item_kind(&mut self) -> Result<Option<(Ident, ItemKind)>, String> {
		if self.consume(Token::Enum) {
			Ok(Some(self.parse_enum()?))
		} else if self.consume(Token::Struct) {
			Ok(Some(self.parse_struct()?))
		} else if self.consume(Token::Use) {
			Ok(Some(self.parse_use()?))
		} else if self.consume(Token::Test) {
			Ok(Some(self.parse_test()?))
		} else if self.consume(Token::Impl) {
			Ok(Some(self.parse_impl()?))
		} else if self.token == Token::Eof {
			Ok(None)
		} else {
			Err(format!(
				"Expected item, found {:?} '{}' at {}",
				self.token,
				self.slice(),
				self.line_column()
			))
		}
	}

	pub fn parse_enum(&mut self) -> Result<(Ident, ItemKind), String> {
		let mut variants = vec![];
		let ident = self.parse_ident()?;
		self.expect(Token::LBrace)?;
		loop {
			let span = self.span.clone();
			if !self.consume(Token::Ident) {
				break;
			}

			let vdata = if self.check(Token::Comma) || self.check(Token::RBrace) {
				VariantData::Unit
			} else if self.consume(Token::LBrace) {
				let fields = self.parse_fields()?;
				self.expect(Token::RBrace)?;
				VariantData::Struct(fields)
			} else if self.consume(Token::LParen) {
				let fields = self.parse_fields_tuple()?;
				VariantData::Tuple(fields)
			} else {
				return Err(format!(
					"Expected enum variant, found {:?} '{}' at {}",
					self.token,
					self.slice(),
					self.line_column()
				));
			};
			variants.push(Variant { span, vdata });
			if !self.consume(Token::Comma) {
				break;
			}
		}
		self.expect(Token::RBrace)?;
		Ok((ident, ItemKind::Enum(EnumDef { variants })))
	}

	pub fn parse_struct(&mut self) -> Result<(Ident, ItemKind), String> {
		let ident = self.parse_ident()?;
		let vdata = if self.consume(Token::Semicolon) {
			VariantData::Unit
		} else if self.consume(Token::LBrace) {
			let fields = self.parse_fields()?;
			VariantData::Struct(fields)
		} else if self.consume(Token::LParen) {
			let fields = self.parse_fields_tuple()?;
			VariantData::Tuple(fields)
		} else {
			return Err(format!(
				"Expected struct definition, found {:?} '{}'",
				self.token,
				self.slice()
			));
		};
		self.expect(Token::RBrace)?;
		Ok((ident, ItemKind::Struct(vdata)))
	}

	pub fn parse_fields(&mut self) -> Result<Vec<FieldDef>, String> {
		let mut fields = vec![];
		loop {
			if self.consume(Token::RBrace) {
				break;
			}
			let vis = self.parse_visibility()?;
			let ident = self.parse_ident()?;
			self.expect(Token::Colon)?;
			let ty = self.parse_type()?;

			fields.push(FieldDef { vis, ident, ty });
			if !self.consume(Token::Comma) {
				break;
			}
		}
		Ok(fields)
	}

	pub fn parse_use(&mut self) -> Result<(Ident, ItemKind), String> {
		let tree = self.parse_use_tree()?;
		if !self.consume(Token::Semicolon) {
			return Err((match tree.kind {
				UseTreeKind::Glob => "the wildcard token must be last on the path",
				UseTreeKind::Nested(..) => "glob-like brace syntax must be last on the path",
				_ => "",
			})
			.to_string());
		};
		Ok((Ident::empty(), ItemKind::Use(tree)))
	}

	pub fn parse_use_tree(&mut self) -> Result<UseTree, String> {
		let mut prefix = Path {
			segments: Vec::new(),
		};
		let kind = if self.check(Token::LBrace) || self.check(Token::Star) {
			// `use *;` or `use ::*;` or `use {...};` or `use ::{...};`
			if self.consume(Token::DoubleColon) {
				prefix.segments.push(PathSegment {
					ident: Ident {
						span: self.span.clone(),
					},
				});
			}

			self.parse_use_tree_glob_or_nested()?
		} else {
			// `use path::*;` or `use path::{...};` or `use path;` or `use path as bar;`
			prefix = self.parse_path(PathStyle::Mod)?;

			if self.consume(Token::DoubleColon) {
				self.parse_use_tree_glob_or_nested()?
			} else {
				UseTreeKind::Simple(self.parse_rename()?)
			}
		};
		Ok(UseTree { prefix, kind })
	}

	pub fn parse_use_tree_glob_or_nested(&mut self) -> Result<UseTreeKind, String> {
		Ok(if self.consume(Token::Star) {
			UseTreeKind::Glob
		} else {
			UseTreeKind::Nested(self.parse_use_tree_list()?)
		})
	}

	pub fn parse_use_tree_list(&mut self) -> Result<Vec<UseTree>, String> {
		let mut list = vec![];
		self.expect(Token::LBrace)?;
		loop {
			list.push(self.parse_use_tree()?);
			if !self.consume(Token::Comma) {
				break;
			}
		}
		self.expect(Token::RBrace)?;
		Ok(list)
	}

	pub fn parse_fields_tuple(&mut self) -> Result<Vec<FieldDef>, String> {
		self.expect(Token::LParen)?;
		let mut fields = vec![];
		loop {
			if self.consume(Token::RParen) {
				break;
			}
			let vis = self.parse_visibility()?;
			let ty = self.parse_type()?;
			fields.push(FieldDef {
				vis,
				ident: Ident::empty(),
				ty,
			});
			if !self.consume(Token::Comma) {
				break;
			}
		}

		Ok(fields)
	}

	pub fn parse_test(&mut self) -> Result<(Ident, ItemKind), String> {
		let span = self.span.clone();
		self.expect(Token::String)?;
		let block = self.parse_block()?;
		Ok((Ident::empty(), ItemKind::Test(span, block)))
	}

	pub fn parse_impl(&mut self) -> Result<(Ident, ItemKind), String> {
		let ty = self.parse_type()?;
		let trait_path = if self.consume(Token::For) {
			Some(self.parse_path(PathStyle::Type)?)
		} else {
			None
		};
		let mut items = vec![];

		self.expect(Token::LBrace)?;
		while let Some(item) = self.parse_assoc_item(true)? {
			items.push(item);
		}
		self.expect(Token::RBrace)?;
		Ok((
			Ident::empty(),
			ItemKind::Impl(Impl {
				trait_path,
				ty,
				items,
			}),
		))
	}

	pub fn parse_assoc_item(
		&mut self,
		req_body: bool,
	) -> Result<Option<Item<AssocItemKind>>, String> {
		let vis = self.parse_visibility()?;
		let info = if self.consume(Token::Type) {
			Some(self.parse_type_alias(req_body)?)
		} else if self.consume(Token::Fn) {
			Some(self.parse_assoc_fn(req_body)?)
		} else if self.check(Token::RBrace) {
			return Ok(None);
		} else {
			return Err(format!(
				"Expected associated item, found {:?} '{}'",
				self.token,
				self.slice(),
			));
		};

		match info {
			Some((ident, kind)) => Ok(Some(Item { kind, ident, vis })),
			None => Ok(None),
		}
	}

	pub fn parse_type_alias(&mut self, right_req: bool) -> Result<(Ident, AssocItemKind), String> {
		let left_ty = self.parse_type()?;
		let right_ty = if right_req {
			self.expect(Token::Eq)?;
			Some(self.parse_type()?)
		} else {
			None
		};
		self.expect(Token::Semicolon)?;

		Ok((
			Ident::empty(),
			AssocItemKind::TypeAlias(TypeAlias { left_ty, right_ty }),
		))
	}

	/// TODO
	pub fn parse_assoc_fn(&mut self, req_body: bool) -> Result<(Ident, AssocItemKind), String> {
		let ident = self.parse_ident()?;
		let args = self.parse_args()?;
		let ret_ty = if self.consume(Token::Arrow) {
			self.parse_type()?
		} else {
			Type {
				kind: TypeKind::Unit,
			}
		};
		let body = if req_body {
			Some(self.parse_block()?)
		} else {
			None
		};

		Ok((ident, AssocItemKind::Fn(Fn { args, ret_ty, body })))
	}

	/// TODO
	pub fn parse_args(&mut self) -> Result<Vec<Arg>, String> {
		let mut args = vec![];
		self.expect(Token::LParen)?;
		loop {
			let ident = Ident {
				span: self.span.clone(),
			};
			let ty = if self.consume(Token::SelfLower) {
				Type {
					kind: TypeKind::ImplicitSelf,
				}
			} else if self.consume(Token::Ident) {
				self.expect(Token::Colon)?;
				self.parse_type()?
			} else if self.consume(Token::RParen) {
				break;
			} else {
				return Err(format!(
					"Expected function argument, found {:?} '{:?}'",
					self.token,
					self.slice()
				));
			};

			args.push(Arg { ident, ty });
			if !self.consume(Token::Comma) {
				self.expect(Token::RParen)?;
				break;
			}
		}
		Ok(args)
	}

	pub fn parse_block(&mut self) -> Result<Block, String> {
		self.expect(Token::LBrace)?;
		let mut stmts = vec![];
		while let Some(stmt) = self.parse_stmt()? {
			stmts.push(stmt);
		}
		self.expect(Token::RBrace)?;
		Ok(Block { stmts })
	}

	/// TODO
	pub fn parse_stmt(&mut self) -> Result<Option<Stmt>, String> {
		Ok(None)
	}

	pub fn parse_rename(&mut self) -> Result<Option<Ident>, String> {
		if self.consume(Token::As) {
			Ok(Some(self.parse_ident()?))
		} else {
			Ok(None)
		}
	}

	pub fn parse_visibility(&mut self) -> Result<Visibility, String> {
		if !self.consume(Token::Pub) {
			return Ok(Visibility {
				kind: VisKind::Inherited,
			});
		}

		if self.token == Token::LParen {
			let path = if self.is_ahead(1, &[Token::In]) {
				self.bump(); // (
				self.bump(); // in
				self.parse_path(PathStyle::Mod)?
			} else if self.is_ahead(1, &[Token::Crate, Token::SelfLower, Token::Super])
				&& self.is_ahead(2, &[Token::RParen])
			{
				self.bump(); // (
				self.parse_path(PathStyle::Mod)?
			} else {
				// A case like Struct(pub (crate::Type))
				return Ok(Visibility {
					kind: VisKind::Public,
				});
			};

			return Ok(Visibility {
				kind: VisKind::Restricted { path },
			});
		} else {
			return Ok(Visibility {
				kind: VisKind::Public,
			});
		}
	}

	pub fn parse_path(&mut self, style: PathStyle) -> Result<Path, String> {
		let mut segments = vec![];
		loop {
			let segment = self.parse_path_segment(&style)?;
			segments.push(segment);
			if self.is_ahead(1, &[Token::LBrace, Token::Star]) || !self.consume(Token::DoubleColon)
			{
				break;
			}
		}
		Ok(Path { segments })
	}

	// TODO: Parse expr and type paths
	pub fn parse_path_segment(&mut self, style: &PathStyle) -> Result<PathSegment, String> {
		Ok(PathSegment {
			ident: self.parse_path_segment_ident()?,
		})
	}

	pub fn parse_path_segment_ident(&mut self) -> Result<Ident, String> {
		let span = self.span.clone();
		self.expect_one_of(&[Token::Ident, Token::Crate, Token::SelfLower, Token::Super])?;
		Ok(Ident { span })
	}

	/// TODO
	pub fn parse_type(&mut self) -> Result<Type, String> {
		let kind = if self.check(Token::LParen) && self.is_ahead(1, &[Token::RParen]) {
			TypeKind::Unit
		} else if self.check(Token::Ident) {
			TypeKind::Path(self.parse_path(PathStyle::Type)?)
		} else {
			return Err(format!(
				"Expected type, found {:?} '{:?}'",
				self.token,
				self.slice()
			));
		};
		Ok(Type { kind })
	}

	pub fn parse_ident(&mut self) -> Result<Ident, String> {
		let span = self.span.clone();
		self.expect(Token::Ident)?;
		Ok(Ident { span })
	}
}

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
