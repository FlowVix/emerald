use crate::{CodeArea, lexer::Token, value::Value, error::{SyntaxError, RuntimeError}, EmeraldSource, parser};



type Tokens = Vec<(Token, (usize, usize))>;

pub struct ParseInfo {
    source: EmeraldSource,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTNode {
    pub span: (usize, usize),
    pub node: NodeType,
}



#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Value { value: Value },
    Op { left: Box<ASTNode>, op: Token, right: Box<ASTNode> },
    Unary { op: Token, node: Box<ASTNode> },
    Declaration { var_name: String, mutable: bool, value: Box<ASTNode> },
    Var { var_name: String },
    StatementList { statements: Vec<ASTNode> },
    Block { code: Box<ASTNode>, not_safe: bool },
    If {cond: Box<ASTNode>, code: Box<ASTNode>, else_branch: Option<Box<ASTNode>>},
    While {cond: Box<ASTNode>, code: Box<ASTNode> },
    Loop { code: Box<ASTNode> },
    Call { base: Box<ASTNode>, args: Vec<ASTNode> },
    FuncDef { func_name: String, arg_names: Vec<String>, arg_areas: Vec<CodeArea>, header_area: CodeArea, code: Box<ASTNode> },
}

macro_rules! expected_err {
    ($exp:expr, $tok:expr, $area:expr, $info:expr) => {
        return Err( SyntaxError::Expected {
            expected: $exp.to_string(),
            found: $tok.name().to_string(),
            area: CodeArea {source: $info.source.clone(), range: $area}
        } )
    };
}

macro_rules! parse_util {
    ($tokens:expr, $pos:expr, $info:expr) => {
        #[allow(unused_macros)]
        macro_rules! tok {
            ($index:literal) => {
                &$tokens[{
                    let le_index = (($pos as i32) + $index);
                    if le_index < 0 {0} else {le_index}
                } as usize].0
            }
        }
        #[allow(unused_macros)]
        macro_rules! span {
            ($index:literal) => {
                $tokens[{
                    let le_index = (($pos as i32) + $index);
                    if le_index < 0 {0} else {le_index}
                } as usize].1
            }
        }
        #[allow(unused_macros)]
        macro_rules! ret {
            ($node_type:expr => $span:expr) => {
                return Ok((ASTNode { 
                    node: $node_type,
                    span: $span,
                 }, $pos))
            };
            ($node_type:expr => $start:expr, $end:expr) => {
                return Ok((ASTNode { 
                    node: $node_type,
                    span: ($start, $end),
                }, $pos))
            }
        }


        #[allow(unused_macros)]
        macro_rules! check_tok {
            ($token:ident else $expected:literal) => {
                if !matches!(tok!(0), Token::$token) {
                    expected_err!($expected, tok!(0), span!(0), $info)
                }
                $pos += 1;
            };
            ($token:ident($val:ident) else $expected:literal) => {
                let $val;
                if let Token::$token(v) = tok!(0) {
                    $val = v.clone();
                } else { expected_err!($expected, tok!(0), span!(0), $info) }
                $pos += 1;
            };
            ($token:ident($val:ident):$sp:ident else $expected:literal) => {
                let $val;
                let $sp;
                if let (Token::$token(v), sp) = (tok!(0), span!(0)) {
                    $val = v.clone();
                    $sp = sp.clone();
                } else { expected_err!($expected, tok!(0), span!(0), $info) }
                $pos += 1;
            };
        }

        
        #[allow(unused_macros)]
        macro_rules! skip_tok {
            ($token:ident) => {
                if matches!(tok!(0), Token::$token) {
                    $pos += 1;
                }
            };
        }
        #[allow(unused_macros)]
        macro_rules! while_tok {
            (== $token:ident: $code:block) => {
                loop {
                    match tok!(0) {
                        Token::$token => $code,
                        _ => break
                    }
                }
            };
            (!= $token:ident: $code:block) => {
                loop {
                    match tok!(0) {
                        Token::$token => break,
                        _ => $code
                    }
                }
                $pos += 1;
            };
        }
        #[allow(unused_macros)]
        macro_rules! if_tok {
            (== $token:ident: $code:block) => {
                match tok!(0) {
                    Token::$token => $code,
                    _ => (),
                }
            };
            (!= $token:ident: $code:block) => {
                match tok!(0) {
                    Token::$token => (),
                    _ => $code,
                }
            };
        }

        macro_rules! parse {
            ($fn:ident => let $var:ident) => {
                let parsed = $fn($tokens, $pos, $info)?;
                $pos = parsed.1;
                let $var = parsed.0;
            };
            ($fn:ident => $var:ident) => {
                let parsed = $fn($tokens, $pos, $info)?;
                $pos = parsed.1;
                $var = parsed.0;
            };
            ($fn:ident => let mut $var:ident) => {
                let parsed = $fn($tokens, $pos, $info)?;
                $pos = parsed.1;
                let mut $var = parsed.0;
            };
            ($fn:ident ($arg:expr) => let $var:ident) => {
                let parsed = $fn($tokens, $pos, $info, $arg)?;
                $pos = parsed.1;
                let $var = parsed.0;
            };
            ($fn:ident ($arg:expr) => $var:ident) => {
                let parsed = $fn($tokens, $pos, $info, $arg)?;
                $pos = parsed.1;
                $var = parsed.0;
            };
            ($fn:ident ($arg:expr) => let mut $var:ident) => {
                let parsed = $fn($tokens, $pos, $info, $arg)?;
                $pos = parsed.1;
                let mut $var = parsed.0;
            };
        }
    };
}

#[derive(PartialEq, Debug)]
enum OpType {
    LeftAssoc,
    RightAssoc,
    Unary
}

macro_rules! operators {
    (
        $(
            $optype:ident <== [$($tok:ident)+],
        )*
    ) => {
        fn infix_prec(tok: &Token) -> usize {
            let mut prec = 0;
            $(
                match tok {
                    $(
                        Token::$tok => if OpType::$optype != OpType::Unary {return prec},
                    )+
                    _ => (),
                };
                prec += 1;
            )*
            1000000
        }
        fn unary_prec(tok: &Token) -> usize {
            let mut prec = 0;
            $(
                match tok {
                    $(
                        Token::$tok => if OpType::$optype == OpType::Unary {return prec},
                    )+
                    _ => (),
                };
                prec += 1;
            )*
            1000000
        }
        fn is_unary(tok: &Token) -> bool {
            let mut utoks = vec![];
            $(
                if OpType::$optype == OpType::Unary {
                    $(
                        utoks.push( Token::$tok );
                    )+
                }
            )*
            return utoks.contains( tok );
        }
        fn prec_amount() -> usize {
            let mut amount = 0;
            $(
                amount += 1;
                OpType::$optype;
            )*
            amount
        }
        fn prec_type(mut prec: usize) -> OpType {
            $(
                if prec == 0 {
                    return OpType::$optype;
                }
                prec -= 1;
            )*
            unreachable!()
        }
    };
}


operators!(
    RightAssoc  <==  [ Assign PlusEq MinusEq MultEq DivEq ModEq PowEq ],
    // LeftAssoc   <==  [ And Or ],
    // Unary       <==  [ Not ],
    LeftAssoc   <==  [ Eq NotEq Greater GreaterEq Lesser LesserEq ],
    LeftAssoc   <==  [ Plus Minus ],
    Unary       <==  [ Plus Minus ],
    LeftAssoc   <==  [ Mult Div Mod ],
    RightAssoc  <==  [ Pow ],
);


pub fn parse_unit(
    tokens: &Tokens,
    mut pos: usize,
    info: &ParseInfo,
) -> Result<(ASTNode, usize), SyntaxError> {
    parse_util!(tokens, pos, info);

    let start = span!(0);
    match tok!(0) {
        Token::Num(n) => {
            pos += 1;
            ret!( NodeType::Value { value: Value::Number(*n) } => start );
        },
        Token::String(s) => {
            pos += 1;
            ret!( NodeType::Value { value: Value::String(s.clone()) } => start );
        },
        Token::True => {
            pos += 1;
            ret!( NodeType::Value { value: Value::Boolean(true) } => start );
        },
        Token::False => {
            pos += 1;
            ret!( NodeType::Value { value: Value::Boolean(false) } => start );
        },
        Token::Null => {
            pos += 1;
            ret!( NodeType::Value { value: Value::Null } => start );
        },
        Token::LParen => {
            pos += 1;
            parse!(parse_expr => let value);
            check_tok!(RParen else ")");
            ret!( value.node => start.0, span!(-1).1 );
        }
        Token::Let => {
            pos += 1;
            let mut mutable = false;
            if_tok!(== Mut: {
                mutable = true;
                pos += 1;
            });
            check_tok!(Ident(var_name) else "variable name");
            check_tok!(Assign else "=");
            parse!(parse_expr => let value);
            // println!("{} {}", var_name, mutable);
            ret!( NodeType::Declaration { var_name, mutable, value: Box::new(value) } => start.0, span!(-1).1 );
        }
        Token::Ident(name) => {
            pos += 1;
            ret!( NodeType::Var { var_name: name.clone() } => start );
        }
        Token::LBracket => {
            pos += 1;
            parse!(parse_statements => let statements);
            check_tok!(RBracket else "}");
            ret!( NodeType::Block { code: Box::new(statements), not_safe: false } => start.0, span!(-1).1 );
        }
        Token::If => {
            pos += 1;
            parse!(parse_expr => let cond);
            parse!(parse_expr => let code);
            let mut else_branch = None;
            if_tok!(== Else: {
                pos += 1;
                parse!(parse_expr => let temp); else_branch = Some(Box::new(temp));
            });
            ret!( NodeType::If { cond: Box::new(cond), code: Box::new(code), else_branch  } => start.0, span!(-1).1 );
        }
        Token::While => {
            pos += 1;
            parse!(parse_expr => let cond);
            parse!(parse_expr => let code);
            ret!( NodeType::While { cond: Box::new(cond), code: Box::new(code) } => start.0, span!(-1).1 );
        }
        Token::Loop => {
            pos += 1;
            parse!(parse_expr => let code);
            ret!( NodeType::Loop { code: Box::new(code) } => start.0, span!(-1).1 );
        }
        Token::Func => {
            pos += 1;
            check_tok!(Ident(func_name) else "function name");
            check_tok!(LParen else "(");
            let header_start = span!(-1).0;

            let mut arg_names = vec![];
            let mut arg_areas = vec![];
            while_tok!(!= RParen: {
                check_tok!(Ident(arg_name) else "argument name");
                arg_areas.push(CodeArea {
                    source: info.source.clone(),
                    range: span!(-1),
                });
                if let Some(id) = arg_names.clone().into_iter().position(|e| e == arg_name) {
                    return Err( SyntaxError::DuplicateArg {
                        arg_name,
                        first_used: arg_areas[id].clone(),
                        used_again: arg_areas.last().unwrap().clone(),
                    } )
                }
                arg_names.push(arg_name);
                if !matches!(tok!(0), Token::RParen | Token::Comma) {
                    expected_err!(") or ,", tok!(0), span!(0), info )
                }
                skip_tok!(Comma);
            });
            let header_end = span!(-1).1;
            let header_area = CodeArea {
                source: info.source.clone(),
                range: (header_start, header_end),
            };

            parse!(parse_expr => let code);
            ret!( NodeType::FuncDef { func_name, arg_names, arg_areas, header_area, code: Box::new(code) } => start.0, span!(-1).1 );
        }
        unary_op if is_unary(unary_op) => {
            pos += 1;
            let prec = unary_prec(unary_op);
            let mut next_prec = if prec + 1 < prec_amount() {prec + 1} else {1000000};
            while next_prec != 1000000 {
                if prec_type(next_prec) == OpType::Unary {
                    next_prec += 1
                } else {
                    break
                }
                if next_prec == prec_amount() { next_prec = 1000000 }
            }
            let value;
            if next_prec != 1000000 { parse!(parse_op (next_prec) => value); }
            else { parse!(parse_value => value); }
            ret!( NodeType::Unary {
                op: unary_op.clone(),
                node: Box::new(value),
            } => start.0, span!(-1).1 )
        },
        _ => expected_err!("value", tok!(0), span!(0), info)
    }
}

fn parse_value(
    tokens: &Tokens,
    mut pos: usize,
    info: &ParseInfo,
) -> Result<(ASTNode, usize), SyntaxError> {
    parse_util!(tokens, pos, info);
    
    parse!(parse_unit => let mut value);
    let start = value.span;

    while matches!(tok!(0), Token::LParen) {
        match tok!(0) {
            Token::LParen => {
                pos += 1;
                let mut args = vec![];
                while_tok!(!= RParen: {
                    parse!(parse_expr => let arg);
                    args.push(arg);
                    if !matches!(tok!(0), Token::RParen | Token::Comma) {
                        expected_err!(") or ,", tok!(0), span!(0), info )
                    }
                    skip_tok!(Comma);
                });
                value = ASTNode {
                    node: NodeType::Call {
                        args,
                        base: Box::new(value),
                    },
                    span: ( start.0, span!(-1).1 )
                }
            },
            _ => unreachable!(),
        }
    }

    Ok((value, pos))

}


fn parse_op(
    tokens: &Tokens,
    mut pos: usize,
    info: &ParseInfo,
    prec: usize,
) -> Result<(ASTNode, usize), SyntaxError> {
    parse_util!(tokens, pos, info);

    let mut next_prec = if prec + 1 < prec_amount() {prec + 1} else {1000000};
    while next_prec != 1000000 {
        if prec_type(next_prec) == OpType::Unary {
            next_prec += 1
        } else {
            break
        }
        if next_prec == prec_amount() {next_prec = 1000000};
    }
    let mut left;
    if next_prec != 1000000 { parse!(parse_op(next_prec) => left); }
    else { parse!(parse_value => left); }

    while infix_prec(tok!(0)) == prec {
        let op = tok!(0).clone();
        pos += 1;
        let right;
        if prec_type(prec) == OpType::LeftAssoc {
            if next_prec != 1000000 { parse!(parse_op(next_prec) => right); }
            else { parse!(parse_value => right); }
        } else {
            parse!(parse_op(prec) => right);
        }
        let (left_span, right_span) = (left.span, right.span);
        left = ASTNode {
            node: NodeType::Op {
                left: Box::new(left),
                right: Box::new(right),
                op,
            },
            span: ( left_span.0, right_span.1 )
        };
    }
    Ok((left, pos))

}

fn parse_expr(
    tokens: &Tokens,
    pos: usize,
    info: &ParseInfo,
) -> Result<(ASTNode, usize), SyntaxError> {
    return parse_op(tokens, pos, info, 0)
}


fn parse_statement(
    tokens: &Tokens,
    mut pos: usize,
    info: &ParseInfo,
) -> Result<(ASTNode, usize), SyntaxError> {
    parse_util!(tokens, pos, info);

    parse!(parse_expr => let value);
    // if !matches!(tok!(-1), Token::RBracket) {
        check_tok!(Eol else ';');
    // }

    Ok((value, pos))

}

fn parse_statements(
    tokens: &Tokens,
    mut pos: usize,
    info: &ParseInfo,
) -> Result<(ASTNode, usize), SyntaxError> {
    parse_util!(tokens, pos, info);

    let mut statements = vec![];
    let start = span!(0);

    while !matches!(tok!(0), Token::Eof | Token::RBracket) {
        parse!(parse_statement => let statement);
        statements.push(statement);
    };

    ret!( NodeType::StatementList {
        statements,
    } => start.0, span!(-1).1 )

}


pub fn parse(
    tokens: &Tokens,
    source: &EmeraldSource,
) -> Result<(ASTNode, usize), SyntaxError> {
    parse_util!(tokens, pos, info);
    let info = ParseInfo {
        source: source.clone(),
    };
    

    parse_statements(tokens, 0, &info)

}





