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

    // #[token("&&")]
    // And,
    // #[token("||")]
    // Or,
    // #[token("!")]
    // Not,

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
    #[token("^=")]
    PowEq,
    

    #[token(">")]
    Greater,
    #[token("<")]
    Lesser,
    #[token(">=")]
    GreaterEq,
    #[token("<=")]
    LesserEq,

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
    #[token("^")]
    Pow,

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

    #[token("|")]
    Pipe,

    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("::")]
    DoubleColon,
    #[token(":")]
    Colon,

    #[token("if")]
    If,
    #[token("else")]
    Else,

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

    #[token("return")]
    Return,
    #[token("break")]
    Break,

    #[token("export")]
    Export,
    #[token("import")]
    Import,

    // #[token("type")]
    // Type,
    #[token("impl")]
    Impl,

    #[token(";")]
    Eol,

    // #[regex(r#"#[a-zA-Z_]\w*"#, |lex| lex.slice()[1..].to_string())]
    // TypeName(String),



    #[regex(r"[a-zA-Z_ඞ][a-zA-Z_0-9ඞ]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[error]
    #[regex(r"[ \t\f\n\r]+", logos::skip)]
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
            // Token::And => "&&",
            // Token::Or => "||",
            // Token::Not => "!",
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
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Mult => "*",
            Token::Div => "/",
            Token::Mod => "%",
            Token::Pow => "^",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBracket => "{",
            Token::RBracket => "}",
            Token::LSqBracket => "[",
            Token::RSqBracket => "]",
            Token::Pipe => "|",
            Token::Comma => ",",
            Token::DoubleColon => "::",
            Token::Colon => ":",
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
            Token::Struct => "struct",
            Token::Export => "export",
            Token::Import => "export",
            // Token::Type => "type",
            Token::Impl => "impl",
            Token::Eol => ";",
            // Token::TypeName(_) => "type name",
            Token::Ident(_) => "identifier",
            Token::Error => "what the fuck is that",
            Token::Eof => "end of file",
            Token::Dot => ".",
            // Token::DoubleColon => "::",
        }
    }
}

