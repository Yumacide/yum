use super::{
	item::{Item, ItemKind},
	lexer::Token,
	EnumDef, FieldDef, Ident, Path, PathSegment, PathStyle, VariantData, VisKind, Visibility,
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
		// TODO: Parse visibility
		if let Some((ident, kind)) = self.parse_item_kind()? {
			Ok(Some(Item { ident, kind }))
		} else {
			Ok(None)
		}
	}

	pub fn parse_item_kind(&mut self) -> Result<Option<(Ident, ItemKind)>, String> {
		if self.consume(Token::Enum) {
			let (ident, enum_def) = self.parse_enum()?;
			Ok(Some((ident, ItemKind::Enum(enum_def))))
		} else if self.consume(Token::Struct) {
			let (ident, vdata) = self.parse_struct()?;
			Ok(Some((ident, ItemKind::Struct(vdata))))
		} else if self.token == Token::Eof {
			Ok(None)
		} else {
			Err(format!(
				"Expected item, found {:?} '{:?}'",
				self.token,
				self.slice()
			))
		}
	}

	pub fn parse_enum(&mut self) -> Result<(Ident, EnumDef), String> {
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
		Ok((ident, EnumDef { variants }))
	}

	pub fn parse_struct(&mut self) -> Result<(Ident, VariantData), String> {
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

		Ok((ident, vdata))
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
			if !self.consume(Token::DoubleColon) {
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
				]
			}),
			ident: Ident { span: 5..11 }
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
			ident: Ident { span: 7..10 }
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
