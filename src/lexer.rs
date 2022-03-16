use logos::Logos;


fn convert_string(s: &str) -> String {
    s
        .replace("\r", "")
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\\"", "\"")
        .replace("\\'", "'")
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {

    #[regex(r#"\d+(\.[\d]+)?"#, |lex| lex.slice().parse::<f64>())]
    Num(f64),

    #[regex(r#""(?:\\.|[^\\"])*"|' + r"'(?:\\.|[^\\'])*'"#, 
        |s| convert_string(&s.slice()[1..s.slice().len()-1])
    )]
    String(String),


    #[token("false")]
    False,
    #[token("true")]
    True,
    #[token("null")]
    Null,

    #[token("==")]
    Eq,
    #[token("!=")]
    NotEq,

    
    #[token("=>")]
    FatArrow,
    #[token("->")]
    Arrow,

    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    ExclMark,

    #[token("=")]
    Assign,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    MultEq,
    #[token("/=")]
    DivEq,
    #[token("%=")]
    ModEq,
    #[token("**=")]
    PowEq,
    

    #[token(">")]
    Greater,
    #[token("<")]
    Lesser,
    #[token(">=")]
    GreaterEq,
    #[token("<=")]
    LesserEq,

    #[token("**")]
    Pow,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mult,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    
    #[token("{")]
    LBracket,
    #[token("}")]
    RBracket,
    #[token("[")]
    LSqBracket,
    #[token("]")]
    RSqBracket,
    
    #[token("!{")]
    ExcLBracket,

    #[token("|")]
    Pipe,

    #[token(",")]
    Comma,
    #[token("..")]
    DoubleDot,
    #[token(".")]
    Dot,
    #[token("::")]
    DoubleColon,
    #[token(":")]
    Colon,

    #[token("$")]
    Dollar,

    #[token("if")]
    If,
    #[token("else")]
    Else,

    
    #[token("match")]
    Match,
    #[token("case")]
    Case,

    #[token("while")]
    While,
    #[token("loop")]
    Loop,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("as")]
    As,
    #[token("is")]
    Is,
    
    #[token("let")]
    Let,
    // #[token("mut")]
    // Mut,
    #[token("func")]
    Func,

    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("mod")]
    Module,

    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,

    #[token("export")]
    Export,
    #[token("import")]
    Import,
    #[token("extract")]
    Extract,

    // #[token("type")]
    // Type,
    #[token("impl")]
    Impl,

    #[token(";")]
    Eol,

    
    #[token(r#"\"#)]
    Backslash,
    #[token(r#"v\"#)]
    VectorSpecial,
    #[token(r#"id\"#)]
    IDSpecial,

    
    #[token(r#"~"#)]
    Tilde,
    #[token("^")]
    Caret,


    #[token("?")]
    QMark,

    // #[regex(r#"#[a-zA-Z_]\w*"#, |lex| lex.slice()[1..].to_string())]
    // TypeName(String),



    #[regex(r"[a-zA-Z_ඞ][a-zA-Z_0-9ඞ]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[error]
    #[regex(r"[ \t\f\n\r]+|/\*[^*]*\*(([^/\*][^\*]*)?\*)*/|//[^\n]*", logos::skip)]
    Error,

    Eof,
}

impl Eq for Token {
    
}


impl Token {
    pub fn name(&self) -> &str {
        match self {
            Token::Num(_) => "number",
            Token::String(_) => "string literal",
            Token::False => "false",
            Token::True => "true",
            Token::Null => "null",
            Token::Eq => "==",
            Token::NotEq => "!=",
            Token::FatArrow => "=>",
            Token::Arrow => "->",
            Token::And => "&&",
            Token::Or => "||",
            Token::ExclMark => "!",
            Token::Assign => "=",
            Token::PlusEq => "+=",
            Token::MinusEq => "-=",
            Token::MultEq => "*=",
            Token::DivEq => "/=",
            Token::ModEq => "%=",
            Token::PowEq => "^=",
            Token::Greater => ">",
            Token::Lesser => "<",
            Token::GreaterEq => ">=",
            Token::LesserEq => "<=",
            Token::Pow => "**",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Mult => "*",
            Token::Div => "/",
            Token::Mod => "%",
            Token::Caret => "^",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBracket => "{",
            Token::RBracket => "}",
            Token::LSqBracket => "[",
            Token::RSqBracket => "]",
            Token::ExcLBracket => "!{",
            Token::Pipe => "|",
            Token::Comma => ",",
            Token::DoubleColon => "::",
            Token::Colon => ":",
            Token::Dollar => "$",
            Token::Match => "match",
            Token::Case => "case",
            Token::If => "if",
            Token::Else => "else",
            Token::Let => "let",
            // Token::Mut => "mut",
            Token::Func => "func",
            Token::While => "while",
            Token::Loop => "loop",
            Token::For => "for",
            Token::In => "in",
            Token::As => "as",
            Token::Is => "is",
            Token::Return => "return",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::Struct => "struct",
            Token::Enum => "enum",
            Token::Module => "mod",
            Token::Export => "export",
            Token::Import => "export",
            Token::Extract => "extract",
            // Token::Type => "type",
            Token::Impl => "impl",
            Token::Eol => ";",
            // Token::TypeName(_) => "type name",
            Token::Ident(_) => "identifier",
            Token::Error => "what the fuck is that",
            Token::Eof => "end of file",
            Token::Dot => ".",
            Token::DoubleDot => "..",
            Token::Backslash => "\\",
            Token::VectorSpecial => "v\\",
            Token::IDSpecial => "id\\",
            Token::Tilde => "~",
            Token::QMark => "?",
            // Token::DoubleColon => "::",
        }
    }
}

