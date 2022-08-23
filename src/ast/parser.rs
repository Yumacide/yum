use super::{
	item::{AssocItemKind, Item, ItemKind},
	lexer::Token,
	Arg, Block, EnumDef, Expr, FieldDef, Fn, Ident, Impl, ItemParseMode, Let, Path, PathSegment,
	PathStyle, Pattern, Stmt, StmtKind, Type, TypeAlias, TypeKind, UseTree, UseTreeKind, Variant,
	VariantData, VisKind, Visibility,
};
use anyhow::{bail, Result};
use logos::Span;

#[cfg(test)]
mod tests;

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
		if let Some(next) = next {
			let (tok, span) = next.clone();
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
		let lines: Vec<&str> = self
			.src
			.get(0..self.span.start + 1)
			.unwrap()
			.lines()
			.collect();
		format!("{}:{}", lines.len(), lines.last().unwrap().len())
	}

	pub fn expect(&mut self, token: Token) -> Result<()> {
		if self.token == token {
			self.bump();
			Ok(())
		} else {
			bail!(
				"Expected {:?}, found {:?} '{}' at {}",
				token,
				self.token,
				self.slice(),
				self.line_column()
			)
		}
	}

	pub fn expect_one_of(&mut self, tokens: &[Token]) -> Result<()> {
		for token in tokens {
			if self.token == *token {
				self.bump();
				return Ok(());
			}
		}
		bail!("Expected one of {:?}, found {:?}", tokens, self.token)
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
		false
	}

	pub fn parse_mod(&mut self) -> Result<Vec<Item>> {
		let mut items = vec![];
		while let Some(item) = self.parse_item(ItemParseMode::Mod)? {
			items.push(item);
		}
		Ok(items)
	}

	pub fn parse_item(&mut self, mode: ItemParseMode) -> Result<Option<Item>> {
		let vis = self.parse_visibility()?;
		if let Some((ident, kind)) = self.parse_item_kind(mode)? {
			Ok(Some(Item { ident, kind, vis }))
		} else {
			Ok(None)
		}
	}

	pub fn parse_item_kind(&mut self, mode: ItemParseMode) -> Result<Option<(Ident, ItemKind)>> {
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
		} else if self.consume(Token::Let) {
			Ok(Some(self.parse_let_item()?))
		} else if self.token == Token::Eof
			|| mode == ItemParseMode::Stmt
			|| (mode == ItemParseMode::Mod && self.check(Token::RBrace))
		{
			Ok(None)
		} else {
			bail!(
				"Expected item, found {:?} '{}' at {}",
				self.token,
				self.slice(),
				self.line_column()
			)
		}
	}

	pub fn parse_enum(&mut self) -> Result<(Ident, ItemKind)> {
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
				bail!(
					"Expected enum variant, found {:?} '{}' at {}",
					self.token,
					self.slice(),
					self.line_column()
				);
			};
			variants.push(Variant { span, vdata });
			if !self.consume(Token::Comma) {
				break;
			}
		}
		self.expect(Token::RBrace)?;
		Ok((ident, ItemKind::Enum(EnumDef { variants })))
	}

	pub fn parse_struct(&mut self) -> Result<(Ident, ItemKind)> {
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
			bail!(
				"Expected struct definition, found {:?} '{}'",
				self.token,
				self.slice()
			);
		};
		self.expect(Token::RBrace)?;
		Ok((ident, ItemKind::Struct(vdata)))
	}

	pub fn parse_fields(&mut self) -> Result<Vec<FieldDef>> {
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

	pub fn parse_use(&mut self) -> Result<(Ident, ItemKind)> {
		let tree = self.parse_use_tree()?;
		if !self.consume(Token::Semicolon) {
			bail!(match tree.kind {
				UseTreeKind::Glob => "the wildcard token must be last on the path",
				UseTreeKind::Nested(..) => "glob-like brace syntax must be last on the path",
				_ => "",
			});
		};
		Ok((Ident::empty(), ItemKind::Use(tree)))
	}

	pub fn parse_use_tree(&mut self) -> Result<UseTree> {
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

	pub fn parse_use_tree_glob_or_nested(&mut self) -> Result<UseTreeKind> {
		Ok(if self.consume(Token::Star) {
			UseTreeKind::Glob
		} else {
			UseTreeKind::Nested(self.parse_use_tree_list()?)
		})
	}

	pub fn parse_use_tree_list(&mut self) -> Result<Vec<UseTree>> {
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

	pub fn parse_fields_tuple(&mut self) -> Result<Vec<FieldDef>> {
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

	pub fn parse_test(&mut self) -> Result<(Ident, ItemKind)> {
		let span = self.span.clone();
		self.expect(Token::String)?;
		let block = self.parse_block()?;
		Ok((Ident::empty(), ItemKind::Test(span, block)))
	}

	pub fn parse_impl(&mut self) -> Result<(Ident, ItemKind)> {
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

	pub fn parse_assoc_item(&mut self, req_body: bool) -> Result<Option<Item<AssocItemKind>>> {
		let vis = self.parse_visibility()?;
		let info = if self.consume(Token::Type) {
			Some(self.parse_type_alias(req_body)?)
		} else if self.consume(Token::Fn) {
			Some(self.parse_assoc_fn(req_body)?)
		} else if self.check(Token::RBrace) {
			return Ok(None);
		} else {
			bail!(
				"Expected associated item, found {:?} '{}'",
				self.token,
				self.slice(),
			);
		};

		match info {
			Some((ident, kind)) => Ok(Some(Item { kind, ident, vis })),
			None => Ok(None),
		}
	}

	pub fn parse_type_alias(&mut self, right_req: bool) -> Result<(Ident, AssocItemKind)> {
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
	pub fn parse_assoc_fn(&mut self, req_body: bool) -> Result<(Ident, AssocItemKind)> {
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
	pub fn parse_args(&mut self) -> Result<Vec<Arg>> {
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
				bail!(
					"Expected function argument, found {:?} '{:?}'",
					self.token,
					self.slice()
				);
			};

			args.push(Arg { ident, ty });
			if !self.consume(Token::Comma) {
				self.expect(Token::RParen)?;
				break;
			}
		}
		Ok(args)
	}

	pub fn parse_block(&mut self) -> Result<Block> {
		self.expect(Token::LBrace)?;
		let mut stmts = vec![];
		while !(self.consume(Token::RBrace) || self.check(Token::Eof)) {
			stmts.push(self.parse_stmt()?);
		}
		Ok(Block { stmts })
	}

	pub fn parse_stmt(&mut self) -> Result<Stmt> {
		let kind = if self.consume(Token::Let) {
			StmtKind::Let(self.parse_let(false)?)
		} else if let Some(item) = self.parse_item(ItemParseMode::Stmt)? {
			StmtKind::Item(item) // This should probably exclude test items in the future
		} else {
			let expr = self.parse_expr()?;
			if self.consume(Token::Semicolon) {
				StmtKind::Semi(expr)
			} else if self.is_ahead(1, &[Token::RBrace]) {
				StmtKind::Expr(expr)
			} else {
				bail!("Missing semicolon at {}", self.line_column());
			}
		};

		Ok(Stmt { kind })
	}

	pub fn parse_let(&mut self, allow_refute: bool) -> Result<Let> {
		let lhs = self.parse_pattern(allow_refute)?;
		if self.consume(Token::Semicolon) {
			return Ok(Let { lhs, rhs: None });
		}
		self.expect(Token::Eq)?;
		let rhs = Some(self.parse_expr()?);
		self.expect(Token::Semicolon)?;
		Ok(Let { lhs, rhs })
	}

	pub fn parse_let_item(&mut self) -> Result<(Ident, ItemKind)> {
		let decl = self.parse_let(false)?;
		Ok((Ident::empty(), ItemKind::Let(decl)))
	}

	/// TODO
	pub fn parse_pattern(&mut self, _allow_refute: bool) -> Result<Pattern> {
		self.parse_ident()?;
		Ok(Pattern {})
	}

	// TODO
	pub fn parse_expr(&mut self) -> Result<Expr> {
		self.bump();
		Ok(Expr {})
	}

	pub fn parse_rename(&mut self) -> Result<Option<Ident>> {
		if self.consume(Token::As) {
			Ok(Some(self.parse_ident()?))
		} else {
			Ok(None)
		}
	}

	pub fn parse_visibility(&mut self) -> Result<Visibility> {
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

			Ok(Visibility {
				kind: VisKind::Restricted { path },
			})
		} else {
			Ok(Visibility {
				kind: VisKind::Public,
			})
		}
	}

	pub fn parse_path(&mut self, style: PathStyle) -> Result<Path> {
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
	pub fn parse_path_segment(&mut self, style: &PathStyle) -> Result<PathSegment> {
		match *style {
			PathStyle::Mod => Ok(PathSegment {
				ident: self.parse_path_segment_ident()?,
			}),
			PathStyle::Type => Ok(PathSegment {
				ident: self.parse_path_segment_ident()?,
			}),
		}
	}

	pub fn parse_path_segment_ident(&mut self) -> Result<Ident> {
		let span = self.span.clone();
		self.expect_one_of(&[Token::Ident, Token::Crate, Token::SelfLower, Token::Super])?;
		Ok(Ident { span })
	}

	/// TODO
	pub fn parse_type(&mut self) -> Result<Type> {
		let kind = if self.check(Token::LParen) && self.is_ahead(1, &[Token::RParen]) {
			TypeKind::Unit
		} else if self.check(Token::Ident) {
			TypeKind::Path(self.parse_path(PathStyle::Type)?)
		} else {
			bail!("Expected type, found {:?} '{:?}'", self.token, self.slice());
		};
		Ok(Type { kind })
	}

	pub fn parse_ident(&mut self) -> Result<Ident> {
		let span = self.span.clone();
		self.expect(Token::Ident)?;
		Ok(Ident { span })
	}
}
