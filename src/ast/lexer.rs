use std::ops::Range;

use logos::{Lexer, Logos, Source, Span};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
	/* Keywords */
	#[token("let")]
	Let,
	#[token("fn")]
	Fn,
	#[token("if")]
	If,
	#[token("else")]
	Else,
	#[token("return")]
	Return,
	#[token("while")]
	While,
	#[token("for")]
	For,
	#[token("loop")]
	Loop,
	#[token("in")]
	In,
	#[token("break")]
	Break,
	#[token("continue")]
	Continue,
	#[token("struct")]
	Struct,
	#[token("enum")]
	Enum,

	/* Binary operators */
	#[token("+")]
	Plus,
	#[token("-")]
	Minus,
	#[token("*")]
	Star,
	#[token("/")]
	Slash,
	#[token("%")]
	Percent,
	#[token("^")]
	Caret,
	#[token("==")]
	EqEq,
	#[token(">")]
	Gt,
	#[token(">=")]
	Ge,
	#[token("<")]
	Lt,
	#[token("<=")]
	Le,

	/* Assignment operators */
	#[token("=")]
	Eq,
	#[token("+=")]
	PlusEq,
	#[token("-=")]
	MinusEq,
	#[token("*=")]
	StarEq,
	#[token("/=")]
	SlashEq,

	/* Brackets */
	#[token("{")]
	LBrace,
	#[token("}")]
	RBrace,
	#[token("(")]
	LParen,
	#[token(")")]
	RParen,
	#[token("[")]
	LSquare,
	#[token("]")]
	RSquare,
	#[token("|")]
	Pipe,

	#[token(".")]
	Dot,
	#[token(",")]
	Comma,

	/* Literals */
	#[regex(r"\.\d[\d_]*([\+\-]?[\d_]+)?")]
	#[regex(r"[\d_]*(\.[\d_]*)?([\+\-]?[\d_]+)?")]
	Number,
	#[regex(r#""((\\")|[^"])*""#)]
	#[regex(r"'((\\')|[^'])*'")]
	String,

	#[regex(r"\s+", logos::skip)]
	Whitespace,
	#[regex("[a-zA-Z]+")]
	Ident,
	#[token(";")]
	Semicolon,
	#[regex(r"//.*\n?")]
	Comment,

	Eof,

	#[error]
	Error,
}

/// A thin wrapper around Logos' `Lexer` that allows peeking.
#[allow(dead_code)]
pub struct PeekLexer<'src> {
	pub lexer: Lexer<'src, Token>,
	peeked: Option<Option<Token>>,
	prev_span: Option<Span>,
	prev_slice: Option<&'src <str as Source>::Slice>,
}

impl<'src> PeekLexer<'src> {
	pub fn new(source: &'src str) -> Self {
		Self {
			lexer: Token::lexer(source),
			peeked: None,
			prev_span: None,
			prev_slice: None,
		}
	}

	pub fn peek(&mut self) -> &Option<Token> {
		if self.peeked.is_none() {
			self.prev_span = Some(self.lexer.span());
			self.prev_slice = Some(self.lexer.slice());
			self.peeked = Some(self.lexer.next());
		}
		self.peeked.as_ref().unwrap()
	}

	pub fn span(&self) -> Range<usize> {
		if self.peeked.is_none() {
			return self.lexer.span();
		}
		self.prev_span.clone().unwrap()
	}

	pub fn slice(&self) -> &'src <str as Source>::Slice {
		if self.peeked.is_none() {
			return self.lexer.slice();
		}
		self.prev_slice.clone().unwrap()
	}
}

impl<'src> Iterator for PeekLexer<'src> {
	type Item = Token;

	fn next(&mut self) -> Option<Token> {
		if let Some(peeked) = self.peeked.take() {
			peeked
		} else {
			self.lexer.next()
		}
	}
}
