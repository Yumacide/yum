use super::{
	item::{Item, ItemKind},
	lexer::Token,
	Block, EnumDef, FieldDef, Ident, Path, PathSegment, PathStyle, Stmt, UseTree, UseTreeKind,
	VariantData, VisKind, Visibility,
};
use crate::ast::{Type, Variant};
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
		let vis = self.parse_visiblity()?;
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
		} else if self.token == Token::Eof {
			Ok(None)
		} else {
			Err(format!(
				"Expected item, found {:?} '{}'",
				self.token,
				self.slice()
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
			let variant = Variant {
				span,
				data: VariantData::Unit,
			};
			variants.push(variant);
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
			self.expect(Token::RBrace)?;
			VariantData::Struct(fields)
		} else if self.consume(Token::LParen) {
			let fields = self.parse_fields()?;
			self.expect(Token::RParen)?;
			VariantData::Tuple(fields)
		} else {
			return Err(format!(
				"Expected struct definition, found {:?} '{}'",
				self.token,
				self.slice()
			));
		};

		Ok((ident, ItemKind::Struct(vdata)))
	}

	pub fn parse_fields(&mut self) -> Result<Vec<FieldDef>, String> {
		let mut fields = vec![];
		loop {
			let vis = self.parse_visiblity()?;
			let ident = if self.token == Token::Ident {
				let span = self.span.clone();
				self.bump();
				Ident { span }
			} else {
				break;
			};
			self.expect(Token::Colon)?;
			let ty = self.parse_type()?;
			if !self.consume(Token::Comma) {
				break;
			}
			fields.push(FieldDef { vis, ident, ty });
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

	pub fn parse_test(&mut self) -> Result<(Ident, ItemKind), String> {
		let span = self.span.clone();
		self.expect(Token::String)?;
		let block = self.parse_block()?;
		Ok((Ident::empty(), ItemKind::Test(span, block)))
	}

	pub fn parse_block(&mut self) -> Result<Block, String> {
		// TODO
		let mut stmts = vec![];
		while let Some(stmt) = self.parse_stmt()? {
			stmts.push(stmt);
		}
		Ok(Block { stmts })
	}

	pub fn parse_stmt(&mut self) -> Result<Option<Stmt>, String> {
		// TODO
		Ok(None)
	}

	pub fn parse_rename(&mut self) -> Result<Option<Ident>, String> {
		if self.consume(Token::As) {
			Ok(Some(self.parse_ident()?))
		} else {
			Ok(None)
		}
	}

	pub fn parse_visiblity(&mut self) -> Result<Visibility, String> {
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
		self.expect(Token::Ident)?;
		Ok(Type {})
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
						data: VariantData::Unit
					},
					Variant {
						span: 18..21,
						data: VariantData::Unit
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
	let src = "struct Foo { Bar: i32, Baz: String }";
	let tokens = Token::lexer(src).spanned().collect();
	let mut parser = Parser::new(tokens, src);
	let item = parser.parse_item().unwrap().unwrap();
	assert_eq!(
		item,
		Item {
			kind: ItemKind::Struct(VariantData::Struct(vec![FieldDef {
				vis: Visibility {
					kind: VisKind::Inherited
				},
				ident: Ident { span: 13..16 },
				ty: Type {}
			}])),
			ident: Ident { span: 7..10 },
			vis: Visibility {
				kind: VisKind::Inherited
			}
		}
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
		assert_eq!(parser.parse_visiblity().unwrap(), vises[i])
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
