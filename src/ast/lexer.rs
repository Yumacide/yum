use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
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

	/* Liter */
	#[regex(r"\s+")]
	Whitespace,
	#[regex("[a-zA-Z]+")]
	Ident,
	#[token(";")]
	Semicolon,

	#[error]
	Error,
}
