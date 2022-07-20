use logos::Logos;

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
	#[token("pub")]
	Pub,
	#[token("crate")]
	Crate,
	#[token("self")]
	SelfLower,
	#[token("Self")]
	SelfUpper,
	#[token("super")]
	Super,
	#[token("use")]
	Use,
	#[token("as")]
	As,
	#[token("test")]
	Test,
	#[token("impl")]
	Impl,
	#[token("type")]
	Type,

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
	#[token(":")]
	Colon,
	#[token("::")]
	DoubleColon,
	#[token("->")]
	Arrow,

	/* Literals */
	#[regex(r"\.\d[\d_]*([\+\-]?[\d_]+)?")]
	#[regex(r"[\d_]*(\.[\d_]*)?([\+\-]?[\d_]+)?")]
	Number,
	#[regex(r#""((\\")|[^"])*""#)]
	#[regex(r"'((\\')|[^'])*'")]
	String,

	#[regex(r"\s+", logos::skip)]
	Whitespace,
	#[regex(r"_*[\w]+[\d_]*")]
	Ident,
	#[token(";")]
	Semicolon,
	#[regex(r"//.*\n?")]
	Comment,

	Eof,

	#[error]
	Error,
}
