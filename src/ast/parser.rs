// TODO: better errors

use super::{
	item::{Item, ItemKind},
	lexer::{PeekLexer, Token},
	EnumDef, Ident,
};
use crate::ast::Variant;

pub struct Parser<'a> {
	pub token: Token,
	pub plexer: PeekLexer<'a>,
	index: usize,
}

impl<'a> Parser<'a> {
	pub fn new(src: &'a str) -> Self {
		let mut parser = Self {
			token: Token::Eof,
			plexer: PeekLexer::new(src),
			index: 0,
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
		self.index += 1;
	}

	pub fn expect(&mut self, token: Token) -> Result<(), String> {
		if self.token == token {
			self.bump();
			Ok(())
		} else {
			Err(format!("Expected {:?}, found {:?}", token, self.token))
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
			let variant = Variant { span };
			variants.push(variant);
			if !self.eat(Token::Comma) {
				break;
			}
		}
		self.expect(Token::RBrace)?;
		Ok((ident, EnumDef { variants }))
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
	let module = parser.parse_mod().unwrap();
	let item = module.get(0).unwrap();
	assert_eq!(
		*item,
		Item {
			kind: ItemKind::Enum(EnumDef {
				variants: vec![Variant { span: 14..16 }, Variant { span: 18..21 }]
			}),
			ident: Ident { span: 5..11 }
		}
	);
}
