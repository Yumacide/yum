// TODO: better errors

use super::{
	item::{Item, ItemKind},
	lexer::{PeekLexer, Token},
	EnumDef, FieldDef, Ident, VariantData,
};
use crate::ast::{Type, Variant};

pub struct Parser<'a> {
	pub token: Token,
	pub plexer: PeekLexer<'a>,
}

impl<'a> Parser<'a> {
	pub fn new(src: &'a str) -> Self {
		let mut parser = Self {
			token: Token::Eof,
			plexer: PeekLexer::new(src),
		};
		parser.bump();
		parser
	}

	pub fn bump(&mut self) {
		let next_token = self.plexer.next();
		if let Some(t) = next_token {
			self.token = t;
		} else {
			self.token = Token::Eof;
		}
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
				self.plexer.slice()
			))
		}
	}

	pub fn expect_one_of(&mut self, tokens: &[Token]) {
		for token in tokens {
			if self.token == *token {
				return;
			}
		}
		panic!("Expected one of {:?}, found {:?}", tokens, self.token);
	}

	pub fn eat(&mut self, token: Token) -> bool {
		let is_present = self.token == token;
		if is_present {
			self.bump();
		}
		is_present
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
		if self.eat(Token::Enum) {
			let (ident, enum_def) = self.parse_enum()?;
			Ok(Some((ident, ItemKind::Enum(enum_def))))
		} else if self.eat(Token::Struct) {
			let (ident, vdata) = self.parse_struct()?;
			Ok(Some((ident, ItemKind::Struct(vdata))))
		} else if self.token == Token::Eof {
			Ok(None)
		} else {
			Err(format!(
				"Expected item, found {:?} '{:?}'",
				self.token,
				self.plexer.slice()
			))
		}
	}

	pub fn parse_enum(&mut self) -> Result<(Ident, EnumDef), String> {
		let mut variants = vec![];
		let ident = self.parse_ident()?;
		self.expect(Token::LBrace)?;
		loop {
			let span = self.plexer.span();
			if !self.eat(Token::Ident) {
				break;
			}
			let variant = Variant {
				span,
				data: VariantData::Unit,
			};
			variants.push(variant);
			if !self.eat(Token::Comma) {
				break;
			}
		}
		self.expect(Token::RBrace)?;
		Ok((ident, EnumDef { variants }))
	}

	pub fn parse_struct(&mut self) -> Result<(Ident, VariantData), String> {
		let ident = self.parse_ident()?;
		let vdata = if self.eat(Token::Semicolon) {
			VariantData::Unit
		} else if self.eat(Token::LBrace) {
			let fields = self.parse_fields()?;
			self.expect(Token::RBrace)?;
			VariantData::Struct(fields)
		} else if self.eat(Token::LParen) {
			let fields = self.parse_fields()?;
			self.expect(Token::RParen)?;
			VariantData::Tuple(fields)
		} else {
			return Err(format!(
				"Expected struct definition, found {:?} '{}'",
				self.token,
				self.plexer.slice()
			));
		};

		Ok((ident, vdata))
	}

	pub fn parse_fields(&mut self) -> Result<Vec<FieldDef>, String> {
		let mut fields = vec![];
		loop {
			let ident = if self.token == Token::Ident {
				let span = self.plexer.span();
				self.bump();
				Ident { span }
			} else {
				break;
			};
			self.expect(Token::Colon)?;
			let ty = self.parse_type()?;
			if !self.eat(Token::Comma) {
				break;
			}
			fields.push(FieldDef { ident, ty });
		}
		Ok(fields)
	}

	/// TODO
	pub fn parse_type(&mut self) -> Result<Type, String> {
		self.expect(Token::Ident)?;
		Ok(Type {})
	}

	pub fn parse_ident(&mut self) -> Result<Ident, String> {
		let span = self.plexer.span();
		println!("{}", self.plexer.slice());
		self.expect(Token::Ident)?;
		Ok(Ident { span })
	}
}

#[test]
fn parse_enum() {
	let mut parser = Parser::new("enum Result { Ok, Err }");
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
	let mut parser = Parser::new("struct Foo { Bar: i32, Baz: String }");
	let item = parser.parse_item().unwrap().unwrap();
	assert_eq!(
		item,
		Item {
			kind: ItemKind::Struct(VariantData::Struct(vec![FieldDef {
				ident: Ident { span: 13..16 },
				ty: Type {}
			}])),
			ident: Ident { span: 7..10 }
		}
	);
}
