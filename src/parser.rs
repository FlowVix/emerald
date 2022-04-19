use std::hash::Hash;

use fnv::FnvHashMap;

use crate::{CodeArea, lexer::{Token, SelectorType}, value::{Value, CoordType, Range}, interpreter::{Globals}, error::{SyntaxError, RuntimeError}, EmeraldSource};



type Tokens = Vec<(Token, (usize, usize))>;

pub struct ParseInfo {
    source: EmeraldSource,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ASTNode {
    pub span: (usize, usize),
    pub node: NodeType,
}


#[derive(Debug, Clone, PartialEq, Hash)]
pub enum BlockType {
    Regular {not_safe: bool},
    McFunc,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Located<T> {
    pub inner: T,
    pub area: CodeArea,
}

impl<T> Located<T> {
    pub fn from(inner: T, area: CodeArea) -> Self {
        Located {
            inner, area
        }
    }
}



#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Value { value: Value },
    Op { left: Box<ASTNode>, op: Token, right: Box<ASTNode> },
    Unary { op: Token, node: Box<ASTNode> },

    Declaration { left: Box<ASTNode>, right: Box<ASTNode> },

    Var { var_name: String },
    StatementList { statements: Vec<ASTNode> },
    Block { code: Box<ASTNode>, typ: BlockType },
    If {cond: Box<ASTNode>, code: Box<ASTNode>, else_branch: Option<Box<ASTNode>>},
    While {cond: Box<ASTNode>, code: Box<ASTNode> },
    For { left: Box<ASTNode>, iter: Box<ASTNode>, code: Box<ASTNode> },
    Loop { code: Box<ASTNode> },
    Call { base: Box<ASTNode>, args: Vec<ASTNode>, args_area: CodeArea },
    FuncDef { func_name: String, args: Vec<(Located<String>, Option<ASTNode>, Option<ASTNode>, bool)>, header_area: CodeArea, code: Box<ASTNode> },
    Lambda { args: Vec<(Located<String>, Option<ASTNode>, Option<ASTNode>, bool)>, header_area: CodeArea, code: Box<ASTNode> },
    Array { elements: Vec<ASTNode> },
    Tuple { elements: Vec<ASTNode> },
    Index { base: Box<ASTNode>, args: Vec<ASTNode>, args_area: CodeArea },
    Dictionary { map: FnvHashMap<String, ASTNode> },
    Member { base: Box<ASTNode>, member: Located<String> },
    Return { node: Option<Box<ASTNode>> },
    Break { node: Option<Box<ASTNode>> },
    Throw { msg: Box<ASTNode> },
    Continue,

    UnboundedRange { base: Box<ASTNode> },

    StructDef {
        struct_name: String,
        fields: FnvHashMap<String, (ASTNode, Option<ASTNode>)>,
        field_areas: FnvHashMap<String, CodeArea>,
        def_area: CodeArea
    },
    StructInstance {
        base: Box<ASTNode>,
        field_areas: FnvHashMap<String, CodeArea>,
        fields: FnvHashMap<String, ASTNode>
    },

    Impl { type_var: (String, CodeArea), fields: Vec<(String, ASTNode)> },
    Associated { base: Box<ASTNode>, assoc: String },
    
    Match {value: Box<ASTNode>, arms: Vec<(MatchArm, ASTNode)>},

    EnumDef {
        enum_name: String,
        variants: FnvHashMap<String, VariantType>,
        variant_areas: FnvHashMap<String, CodeArea>,
        def_area: CodeArea,
    },
    EnumInstance {
        base: Box<ASTNode>,
        variant_name: String,
        variant_area: CodeArea,
        variant: VariantType,
    },

    ModuleDef {
        module_name: String,
        def_area: CodeArea
    },

    Export { name: String, value: Box<ASTNode> },
    Import { path: String, cached: bool },
    MCCall { base: Box<ASTNode> },
    CurrentMcId,
    McVector { x: CoordType<Box<ASTNode>>, y: CoordType<Box<ASTNode>>, z: CoordType<Box<ASTNode>>, rot: Option<(CoordType<Box<ASTNode>>, CoordType<Box<ASTNode>>)> },
    Extract { value: Box<ASTNode> },
    
    Selector { selector_type: SelectorType, args: Vec<(String, CodeArea, ASTNode)> },

}





#[derive(Debug, Clone, PartialEq, Hash)]
pub enum MatchArm {
    Pattern(ASTNode),
    Structure(ASTNode),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariantType {
    Unit,
    Tuple(Vec<ASTNode>),
    Struct { fields: FnvHashMap<String, ASTNode>, field_areas: FnvHashMap<String, CodeArea> }
}

macro_rules! selector_args {
    {

        ($value:ident, $globals:ident)

        $(
            $name:ident => $value_check:block else $expected:literal, Unique: $unique:expr;
        )+

    } => {
        pub fn valid_selector_arg(arg: String) -> bool {
            match &arg[..] {
                $(
                    stringify!($name) => true,
                )+
                _ => false,
            }
        }
        pub fn selector_arg_unique(arg: String) -> bool {
            match &arg[..] {
                $(
                    stringify!($name) => $unique,
                )+
                _ => unreachable!(),
            }
        }
        pub fn check_selector_type(arg: String, $globals: &Globals, value_area: CodeArea, $value: &Value) -> Result<(), RuntimeError> {
            match &arg[..] {
                $(
                    stringify!($name) => if $value_check {Ok(())} else {
                        Err( RuntimeError::IncorrectSelectorArgType {
                            expected: $expected.to_string(),
                            value_area,
                            arg_name: arg,
                        } )
                    },
                )+
                _ => unreachable!(),
            }
        }
        // pub fn selector_arg_unique
    };
}

selector_args!{

    (value, globals)

    x => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    y => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    z => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    distance => {
        match value {
            Value::Range(Range {step, ..}) => *step == 1.0,
            _ => false,
        }
    } else "range with step 1", Unique: true;
    dx => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    dy => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    dz => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    scores => {
        match value {
            Value::Dictionary(map) => {
                let mut valid = true;
                for (_, id) in map {
                    if !(matches!(globals.get(*id).value, Value::Range(_)) || matches!(globals.values.map[id].value, Value::Number(_))) {
                        valid = false;
                        break;
                    }
                }
                valid
            }
            _ => false,
        }
    } else "range or number dictionary", Unique: true;
    tag => { matches!(value, Value::String(_)) } else "string", Unique: false;
    team => { matches!(value, Value::String(_)) } else "string", Unique: true;

    name => { matches!(value, Value::String(_)) } else "string", Unique: true;
    type => { matches!(value, Value::String(_)) } else "string", Unique: true;
    predicate => { matches!(value, Value::String(_)) } else "string", Unique: true;

    x_rotation => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    y_rotation => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    nbt => { matches!(value, Value::Dictionary(_)) /* unfinished */ } else "valid nbt dictionary", Unique: false;

    level => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    gamemode => { matches!(value, Value::String(_)) } else "string", Unique: true;
    advancements => { matches!(value, Value::Dictionary(_)) /* unfinished */ } else "idfk", Unique: true;

    limit => { matches!(value, Value::Number(_)) } else "number", Unique: true;
    sort => { matches!(value, Value::String(_)) } else "string", Unique: true;
}


// #[derive(Debug, Clone, PartialEq)]
// pub enum ArrayDestrItem {
//     Single(Destructure),
//     Spread(Destructure),
// }


// #[derive(Debug, Clone, PartialEq)]
// pub enum DestrType {
//     Var(String),
//     Array(Vec<ArrayDestrItem>),
// }


// #[derive(Debug, Clone, PartialEq)]
// pub struct Destructure {
//     destr: DestrType,
//     span: (usize, usize),
// }



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
            ($index:expr) => {
                &$tokens[{
                    let le_index = (($pos as i32) + $index);
                    if le_index < 0 {0} else {le_index}
                } as usize].0
            }
        }
        #[allow(unused_macros)]
        macro_rules! span {
            ($index:expr) => {
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
            };
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
        macro_rules! skip_toks {
            ($token:ident) => {
                while matches!(tok!(0), Token::$token) {
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
            (== $token:ident: $code:block else $else_code:block) => {
                match tok!(0) {
                    Token::$token => $code,
                    _ => $else_code,
                }
            };
            (!= $token:ident: $code:block else $else_code:block) => {
                match tok!(0) {
                    Token::$token => $else_code,
                    _ => $code,
                }
            };
        }

        #[allow(unused_macros)]
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
    RightAssoc  <==  [ Assign ],
    RightAssoc  <==  [ PlusEq MinusEq MultEq DivEq ModEq PowEq EuclModEq ],
    LeftAssoc   <==  [ And Or ],
    LeftAssoc   <==  [ Pipe ],
    Unary       <==  [ ExclMark ],
    LeftAssoc   <==  [ Is Eq NotEq Greater GreaterEq Lesser LesserEq In ],
    LeftAssoc   <==  [ DoubleDot ],
    Unary       <==  [ DoubleDot ],
    Unary       <==  [ TripleDot ],
    LeftAssoc   <==  [ Plus Minus ],
    Unary       <==  [ Plus Minus ],
    LeftAssoc   <==  [ Mult Div Mod EuclMod ],
    RightAssoc  <==  [ Pow ],
    LeftAssoc   <==  [ As ],
);



// pub fn find_matching_offset(
//     start_tok: (Token, (usize, usize)),
//     end_tok: Token,
//     start_pos: usize,
//     tokens: &Tokens,
//     info: &ParseInfo,
// ) -> Result<i32, SyntaxError> {
//     let mut i = 0;
//     let mut depth = 1;

//     loop {
//         if tokens[start_pos + i].0 == start_tok.0 {
//             depth += 1;
//         } else if tokens[start_pos + i].0 == end_tok {
//             depth -= 1;
//             if depth == 0 {
//                 break;
//             }
//         } else if tokens[start_pos + i].0 == Token::Eof {
//             return Err( SyntaxError::UnmatchedChar {
//                 for_char: start_tok.0.name().to_string(),
//                 not_found: end_tok.name().to_string(),
//                 area: CodeArea {
//                     source: info.source.clone(),
//                     range: start_tok.1,
//                 }
//             } )
//         }
//         i += 1;
//     }

//     Ok( i as i32 )
// }



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


            if matches!(tok!(0), Token::RParen) && !matches!(tok!(1), Token::FatArrow) {
                pos += 1;
                ret!( NodeType::Tuple { elements: vec![] } => start.0, span!(-1).1 );
            }

            let mut i = 0;
            let mut depth = 1;

            loop {
                match tok!(i) {
                    Token::LParen => depth += 1,
                    Token::RParen => { depth -= 1; if depth == 0 {i += 1; break;} },
                    Token::Eof => return Err( SyntaxError::UnmatchedChar {
                        for_char: "(".to_string(),
                        not_found: ")".to_string(),
                        area: CodeArea {
                            source: info.source.clone(),
                            range: start,
                        }
                    } ),
                    _ => (),
                }
                i += 1;
            }

            match tok!(i) {
                Token::FatArrow => {
                    let header_start = span!(-1).0;

                    let mut args: Vec<(Located<String>, Option<ASTNode>, Option<ASTNode>, bool)> = vec![];
                    while_tok!(!= RParen: {
                        let mut is_ref = false;
                        if_tok!(== Ampersand: {
                            pos += 1;
                            is_ref = true;
                        });
                        check_tok!(Ident(arg_name) else "argument name");
                        let arg_area = CodeArea {
                            source: info.source.clone(),
                            range: span!(-1),
                        };
                        if let Some(id) = args.iter().position(|(Located {inner, ..}, ..)| inner == &arg_name) {
                            return Err( SyntaxError::DuplicateArg {
                                arg_name,
                                first_used: args[id].0.area.clone(),
                                used_again: arg_area,
                            } )
                        }
                        if arg_name == "self" && !args.is_empty() {
                            return Err( SyntaxError::SelfNotFirstArg {
                                area: arg_area,
                            } )
                        }
                        let mut pat = None;
                        if_tok!(== Colon: {
                            pos += 1;
                            parse!(parse_expr(true) => let temp); pat = Some(temp);
                        });
                        let mut default = None;
                        if_tok!(== Assign: {
                            pos += 1;
                            parse!(parse_expr(true) => let temp); default = Some(temp);
                        });
                        args.push((Located::from(arg_name, arg_area), pat, default, is_ref));
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
                    pos += 1;
                    parse!(parse_expr(false) => let code);
                    ret!( NodeType::Lambda { args, header_area, code: Box::new(code) } => start.0, span!(-1).1 );
                },
                _ => {
                    parse!(parse_expr(false) => let value);

                    if_tok!(== Comma: {
                        pos += 1;

                        let mut elements = vec![value];
                        while_tok!(!= RParen: {
                            parse!(parse_expr(false) => let elem);
                            elements.push( elem );
                            if !matches!(tok!(0), Token::RParen | Token::Comma) {
                                expected_err!(") or ,", tok!(0), span!(0), info )
                            }
                            skip_tok!(Comma);
                        });
                        ret!( NodeType::Tuple {
                            elements,
                        } => start.0, span!(-1).1 )
                    });

                    check_tok!(RParen else ")");
                    ret!( value.node => start.0, span!(-1).1 );
                }
            }


        }
        Token::Let => {
            pos += 1;
            parse!(parse_expr(true) => let left);
            check_tok!(Assign else "=");
            parse!(parse_expr(false) => let right);
            // println!("{} {}", var_name, mutable);
            ret!( NodeType::Declaration { left: Box::new(left), right: Box::new(right) } => start.0, span!(-1).1 );
        }
        Token::Ident(name) => {
            pos += 1;

            match tok!(0) {
                Token::FatArrow => {
                    pos += 1;
                    let arg_area = CodeArea {
                        source: info.source.clone(),
                        range: start,
                    };
                    parse!(parse_expr(false) => let code);
                    ret!( NodeType::Lambda { args: vec![(Located::from(name.to_string(), arg_area.clone()), None, None, false)], header_area: arg_area, code: Box::new(code) } => start.0, span!(-1).1 );
                }
                _ => (),
            }

            ret!( NodeType::Var { var_name: name.clone() } => start );
        }
        Token::FatArrow => {
            pos += 1;
            let header_area = CodeArea {
                source: info.source.clone(),
                range: start,
            };
            parse!(parse_expr(false) => let code);
            ret!( NodeType::Lambda { args: vec![], header_area, code: Box::new(code) } => start.0, span!(-1).1 );
        }
        Token::LBracket => {
            pos += 1;

            if matches!(tok!(0), Token::RBracket) {
                pos += 1;
                ret!( NodeType::Dictionary {map: FnvHashMap::default()} => start );
            }

            if matches!(tok!(0), Token::Ident(_)) && matches!(tok!(1), Token::Colon | Token::Comma | Token::RBracket) {
                let mut map = FnvHashMap::default();
                let mut areas: FnvHashMap<String, CodeArea> = FnvHashMap::default();
                while_tok!(!= RBracket: {
                    check_tok!(Ident(key) else "key name");
                    let last_area = CodeArea {
                        source: info.source.clone(),
                        range: span!(-1),
                    };
                    if map.contains_key(&key) {
                        return Err( SyntaxError::DuplicateKey {
                            first_used: areas[&key].clone(),
                            key_name: key,
                            used_again: last_area,
                        } )
                    }
                    areas.insert(key.clone(), last_area);
                    let mut value = ASTNode {
                        node: NodeType::Var {
                            var_name: key.clone(),
                        },
                        span: span!(-1),
                    };
                    if_tok!(== Colon: {
                        pos += 1;
                        parse!(parse_expr(false) => value);
                    });

                    map.insert(key, value);
                    if !matches!(tok!(0), Token::RBracket | Token::Comma) {
                        expected_err!("} or ,", tok!(0), span!(0), info )
                    }
                    skip_tok!(Comma);
                });
                ret!( NodeType::Dictionary {map} => start.0, span!(-1).1 );
            } else {
                parse!(parse_statements => let statements);
                check_tok!(RBracket else "}");
                ret!( NodeType::Block { code: Box::new(statements), typ: BlockType::Regular {not_safe: false} } => start.0, span!(-1).1 );
            }

        }
        Token::ExcLBracket => {
            pos += 1;
            parse!(parse_statements => let statements);
            check_tok!(RBracket else "}");
            ret!( NodeType::Block { code: Box::new(statements), typ: BlockType::McFunc } => start.0, span!(-1).1 );
        }
        Token::Impl => {
            pos += 1;
            check_tok!(Ident(type_var) else "type to impl on");
            let type_var_area = CodeArea {
                source: info.source.clone(),
                range: span!(-1),
            };
            check_tok!(LBracket else "{");

            let mut field_names = vec![];
            let mut fields = vec![];
            let mut areas: FnvHashMap<String, CodeArea> = FnvHashMap::default();
            while_tok!(!= RBracket: {
                check_tok!(Ident(field) else "field name");
                let last_area = CodeArea {
                    source: info.source.clone(),
                    range: span!(-1),
                };
                if field_names.contains(&field) {
                    return Err( SyntaxError::DuplicateFieldImpl {
                        first_used: areas[&field].clone(),
                        field_name: field,
                        used_again: last_area,
                    } )
                }
                areas.insert(field.clone(), last_area);
                check_tok!(Colon else ":");

                parse!(parse_expr(false) => let value);

                field_names.push(field.clone());
                fields.push((field, value));
                if !matches!(tok!(0), Token::RBracket | Token::Comma) {
                    expected_err!("} or ,", tok!(0), span!(0), info )
                }
                skip_tok!(Comma);
            });
            ret!( NodeType::Impl {type_var: (type_var, type_var_area), fields} => start );
            

        }
        Token::LSqBracket => {
            pos += 1;

            let mut elements = vec![];
            while_tok!(!= RSqBracket: {
                parse!(parse_expr(false) => let elem);
                elements.push( elem );
                if !matches!(tok!(0), Token::RSqBracket | Token::Comma) {
                    expected_err!("] or ,", tok!(0), span!(0), info )
                }
                skip_tok!(Comma);
            });
            ret!( NodeType::Array {
                elements,
            } => start.0, span!(-1).1 )
        }
        Token::If => {
            pos += 1;
            parse!(parse_expr(false) => let cond);
            parse!(parse_expr(false) => let code);
            let mut else_branch = None;
            if_tok!(== Else: {
                pos += 1;
                parse!(parse_expr(false) => let temp); else_branch = Some(Box::new(temp));
            });
            ret!( NodeType::If { cond: Box::new(cond), code: Box::new(code), else_branch  } => start.0, span!(-1).1 );
        }
        Token::Match => {
            pos += 1;
            parse!(parse_expr(false) => let value);
            check_tok!(LBracket else "{");

            let mut arms = vec![];
            while_tok!(!= RBracket: {

                let arm;
                if_tok!(== Case: {
                    pos += 1;
                    parse!(parse_expr(false) => let check);
                    arm = MatchArm::Structure(check);
                } else {
                    parse!(parse_expr(false) => let check);
                    arm = MatchArm::Pattern(check);
                });

                check_tok!(Arrow else "->");
                parse!(parse_expr(false) => let expr);

                arms.push( (arm, expr) );
                if !matches!(tok!(0), Token::RBracket | Token::Comma) {
                    expected_err!("} or ,", tok!(0), span!(0), info )
                }
                skip_tok!(Comma);
            });


            ret!( NodeType::Match { value: Box::new(value), arms  } => start.0, span!(-1).1 );
        }
        Token::While => {
            pos += 1;
            parse!(parse_expr(false) => let cond);
            parse!(parse_expr(false) => let code);
            ret!( NodeType::While { cond: Box::new(cond), code: Box::new(code) } => start.0, span!(-1).1 );
        }
        Token::Loop => {
            pos += 1;
            parse!(parse_expr(false) => let code);
            ret!( NodeType::Loop { code: Box::new(code) } => start.0, span!(-1).1 );
        }
        Token::For => {
            pos += 1;
            parse!(parse_expr(true) => let left);
            check_tok!(Of else "of");
            parse!(parse_expr(false) => let iter);
            parse!(parse_expr(false) => let code);
            ret!( NodeType::For { code: Box::new(code), iter: Box::new(iter), left: Box::new(left) } => start.0, span!(-1).1 );
        }
        Token::Func => {
            pos += 1;
            check_tok!(Ident(func_name) else "function name");
            check_tok!(LParen else "(");
            let header_start = span!(-1).0;

            let mut args: Vec<(Located<String>, Option<ASTNode>, Option<ASTNode>, bool)> = vec![];
            while_tok!(!= RParen: {
                let mut is_ref = false;
                if_tok!(== Ampersand: {
                    pos += 1;
                    is_ref = true;
                });
                check_tok!(Ident(arg_name) else "argument name");
                let arg_area = CodeArea {
                    source: info.source.clone(),
                    range: span!(-1),
                };
                if let Some(id) = args.iter().position(|(Located {inner, ..}, ..)| inner == &arg_name) {
                    return Err( SyntaxError::DuplicateArg {
                        arg_name,
                        first_used: args[id].0.area.clone(),
                        used_again: arg_area,
                    } )
                }
                if arg_name == "self" && !args.is_empty() {
                    return Err( SyntaxError::SelfNotFirstArg {
                        area: arg_area,
                    } )
                }
                let mut pat = None;
                if_tok!(== Colon: {
                    pos += 1;
                    parse!(parse_expr(true) => let temp); pat = Some(temp);
                });
                let mut default = None;
                if_tok!(== Assign: {
                    pos += 1;
                    parse!(parse_expr(true) => let temp); default = Some(temp);
                });
                args.push(( Located::from(arg_name, arg_area), pat, default, is_ref));
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

            parse!(parse_expr(false) => let code);
            ret!( NodeType::FuncDef { func_name, args, header_area, code: Box::new(code) } => start.0, span!(-1).1 );
        },
        Token::Return => {
            pos += 1;
            let mut node = None;
            if_tok!(!= Eol: {
                parse!(parse_expr(false) => let temp); node = Some(Box::new(temp));
            });
            ret!( NodeType::Return {
                node
            } => start.0, span!(-1).1 )
        },
        Token::Break => {
            pos += 1;
            let mut node = None;
            if_tok!(!= Eol: {
                parse!(parse_expr(false) => let temp); node = Some(Box::new(temp));
            });
            ret!( NodeType::Break {
                node
            } => start.0, span!(-1).1 )
        },
        Token::Continue => {
            pos += 1;
            ret!( NodeType::Continue => start.0, span!(-1).1 )
        },
        Token::Struct => {
            pos += 1;
            check_tok!(Ident(struct_name) else "struct name");
            check_tok!(LBracket else "{");

            let mut fields = FnvHashMap::default();
            let mut field_areas: FnvHashMap<String, CodeArea> = FnvHashMap::default();
            while_tok!(!= RBracket: {
                check_tok!(Ident(field) else "field name");
                let last_area = CodeArea {
                    source: info.source.clone(),
                    range: span!(-1),
                };
                if fields.contains_key(&field) {
                    return Err( SyntaxError::DuplicateField {
                        first_used: field_areas[&field].clone(),
                        field_name: field,
                        used_again: last_area,
                    } )
                }
                field_areas.insert(field.clone(), last_area);
                check_tok!(Colon else ":");

                let mut default = None;
                parse!(parse_expr(true) => let field_type);
                if_tok!(== Assign: {
                    pos += 1;
                    parse!(parse_expr(true) => let temp); default = Some(temp);
                });

                fields.insert(field, (field_type, default));
                if !matches!(tok!(0), Token::RBracket | Token::Comma) {
                    expected_err!("} or ,", tok!(0), span!(0), info )
                }
                skip_tok!(Comma);
            });
            
            let def_area = CodeArea {
                source: info.source.clone(),
                range: (start.0, span!(-1).1),
            };
            // println!("{}: {:#?}", struct_name, fields);
            ret!( NodeType::StructDef { struct_name, fields, field_areas, def_area } => start.0, span!(-1).1 );
        },
        Token::Enum => {
            pos += 1;
            check_tok!(Ident(enum_name) else "enum name");
            check_tok!(LBracket else "{");

            let mut variants = FnvHashMap::default();
            let mut variant_areas: FnvHashMap<String, CodeArea> = FnvHashMap::default();

            while_tok!(!= RBracket: {
                check_tok!(Ident(name) else "variant name");
                let last_area = CodeArea {
                    source: info.source.clone(),
                    range: span!(-1),
                };
                if variants.contains_key(&name) {
                    return Err( SyntaxError::DuplicateEnumVariant {
                        first_used: variant_areas[&name].clone(),
                        variant_name: name,
                        used_again: last_area,
                    } )
                }
                variant_areas.insert(name.clone(), last_area);

                match tok!(0) {
                    Token::Comma => {
                        variants.insert(name, VariantType::Unit);
                    }
                    Token::LParen => {
                        pos += 1;
                        let mut elements = vec![];
                        while_tok!(!= RParen: {
                            parse!(parse_expr(false) => let elem);
                            elements.push( elem );
                            if !matches!(tok!(0), Token::RParen | Token::Comma) {
                                expected_err!(") or ,", tok!(0), span!(0), info )
                            }
                            skip_tok!(Comma);
                        });
                        variants.insert(name, VariantType::Tuple(elements));
                    }
                    Token::LBracket => {
                        pos += 1;
                        let mut fields = FnvHashMap::default();
                        let mut field_areas: FnvHashMap<String, CodeArea> = FnvHashMap::default();
                        while_tok!(!= RBracket: {
                            check_tok!(Ident(field) else "field name");
                            let last_area = CodeArea {
                                source: info.source.clone(),
                                range: span!(-1),
                            };
                            if fields.contains_key(&field) {
                                return Err( SyntaxError::DuplicateFieldStructVariant {
                                    first_used: field_areas[&field].clone(),
                                    field_name: field,
                                    used_again: last_area,
                                } )
                            }
                            field_areas.insert(field.clone(), last_area);
                            check_tok!(Colon else ":");
                            
                            parse!(parse_expr(true) => let field_type);

                            fields.insert(field, field_type);
                            if !matches!(tok!(0), Token::RBracket | Token::Comma) {
                                expected_err!("b } or ,", tok!(0), span!(0), info )
                            }
                            skip_tok!(Comma);
                        });
                        variants.insert(name, VariantType::Struct{ fields, field_areas } );
                    }
                    _ => (),
                }
                if !matches!(tok!(0), Token::RBracket | Token::Comma) {
                    expected_err!("{, (, }, or ,", tok!(0), span!(0), info )
                }
                skip_tok!(Comma);
            });

            let def_area = CodeArea {
                source: info.source.clone(),
                range: (start.0, span!(-1).1),
            };
            // println!("{}: {:#?}", struct_name, fields);
            ret!( NodeType::EnumDef { enum_name, variants, variant_areas, def_area } => start.0, span!(-1).1 );
        },
        Token::Module => {
            pos += 1;
            check_tok!(Ident(module_name) else "module name");
            
            let def_area = CodeArea {
                source: info.source.clone(),
                range: (start.0, span!(-1).1),
            };

            ret!( NodeType::ModuleDef { module_name, def_area } => start.0, span!(-1).1 );
        },
        Token::Import => {
            pos += 1;
            let mut cached = false;
            if_tok!(== QMark: {
                pos += 1;
                cached = true;
            });
            check_tok!(String(path) else "string");

            ret!( NodeType::Import { path, cached } => start.0, span!(-1).1 );
        },
        Token::Extract => {
            pos += 1;
            parse!(parse_expr(false) => let value);

            ret!( NodeType::Extract { value: Box::new(value) } => start.0, span!(-1).1 );
        },
        Token::Throw => {
            pos += 1;
            parse!(parse_expr(false) => let msg);

            ret!( NodeType::Throw { msg: Box::new(msg) } => start.0, span!(-1).1 );
        },
        Token::Dollar => {
            pos += 1;

            ret!( NodeType::CurrentMcId => start.0, span!(-1).1 );
        },
        Token::VectorSpecial => {
            pos += 1;
            let mut is_caret_type = None;
            macro_rules! parse_coord {
                ($rot:expr) => {
                    match tok!(0) {
                        Token::Tilde => {
                            if !$rot {
                                match is_caret_type {
                                    Some(true) => return Err( SyntaxError::VectorMismatch {
                                        in_rot: false,
                                        area: CodeArea {
                                            source: info.source.clone(),
                                            range: span!(0),
                                        },
                                    } ),
                                    None => {is_caret_type = Some(false);},
                                    Some(false) => (),
                                }
                            }
        
                            pos += 1;
                            let value;
                            if !matches!(tok!(0), Token::Tilde | Token::Caret | Token::Comma | Token::Backslash) {
                                parse!(parse_expr(false) => value);
                            } else {
                                value = ASTNode {
                                    node: NodeType::Value {
                                        value: Value::Number(0.0),
                                    },
                                    span: span!(-1),
                                };
                            }
                            CoordType::Tilde(Box::new(value))
                        }
                        Token::Caret => {
        
                            if !$rot {
                                match is_caret_type {
                                    Some(false) => return Err( SyntaxError::VectorMismatch {
                                        in_rot: false,
                                        area: CodeArea {
                                            source: info.source.clone(),
                                            range: span!(0),
                                        },
                                    } ),
                                    None => {is_caret_type = Some(true);},
                                    Some(true) => (),
                                }
                            } else {
                                return Err( SyntaxError::VectorMismatch {
                                    in_rot: true,
                                    area: CodeArea {
                                        source: info.source.clone(),
                                        range: span!(0),
                                    },
                                } )
                            }
        
                            pos += 1;
                            let value;
                            if !matches!(tok!(0), Token::Tilde | Token::Caret | Token::Comma | Token::Backslash) {
                                parse!(parse_expr(false) => value);
                            } else {
                                value = ASTNode {
                                    node: NodeType::Value {
                                        value: Value::Number(0.0),
                                    },
                                    span: span!(-1),
                                };
                            }
                            CoordType::Caret(Box::new(value))
                        }
                        _ => {
        
                            if !$rot {
                                match is_caret_type {
                                    Some(true) => return Err( SyntaxError::VectorMismatch {
                                        in_rot: false,
                                        area: CodeArea {
                                            source: info.source.clone(),
                                            range: span!(0),
                                        },
                                    } ),
                                    None => {is_caret_type = Some(false);},
                                    Some(false) => (),
                                }
                            }
        
                            parse!(parse_expr(false) => let value);
                            CoordType::Absolute(Box::new(value))
                        }
                    }
                }
            }

            let x = parse_coord!(false);
            let y = parse_coord!(false);
            let z = parse_coord!(false);
            let mut rot = None;
            if_tok!(== Comma: {
                pos += 1;
                let h = parse_coord!(true);
                let v = parse_coord!(true);
                rot = Some((h, v))
            });
            check_tok!(Backslash else "\\");

            // println!("{:#?} {:#?} {:#?} {:#?}", x, y, z, rot);

            ret!( NodeType::McVector { x, y, z, rot } => start.0, span!(-1).1 );
        },
        Token::Selector(s) => {
            pos += 1;

            let mut args: Vec<(String, CodeArea, ASTNode)> = vec![];

            if_tok!(== LSqBracket: {
                pos += 1;

                let type_allowed = matches!(s, SelectorType::Executor | SelectorType::Entities);

                while_tok!(!= RSqBracket: {
                    check_tok!(Ident(arg_name) else "selector argument name or ]");
                    let arg_area = CodeArea {
                        source: info.source.clone(),
                        range: span!(-1),
                    };

                    if !valid_selector_arg(arg_name.clone()) {
                        return Err( SyntaxError::NonexistentSelectorArg {
                            arg_name,
                            used: arg_area,
                        } )
                    }
                    if !type_allowed && arg_name == "type" {
                        return Err( SyntaxError::TypeSelectorArg {
                            used: arg_area,
                        } )
                    };
                    
                    
                    if selector_arg_unique(arg_name.clone()) {
                        if let Some(id) = args.iter().position(|(e, _, _)| e == &arg_name) {
                            return Err( SyntaxError::DuplicateSelectorArg {
                                arg_name,
                                first_used: args[id].1.clone(),
                                used_again: arg_area,
                            } )
                        }
                    }

                    check_tok!(Assign else "=");
                    parse!(parse_expr(true) => let val);

                    args.push((arg_name, arg_area, val));

                    if !matches!(tok!(0), Token::RSqBracket | Token::Comma) {
                        expected_err!("] or ,", tok!(0), span!(0), info )
                    }
                    skip_tok!(Comma);
                });
            });

            ret!( NodeType::Selector { selector_type: s.clone(), args } => start.0, span!(-1).1 );
            
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
    
    // println!("{}", pos);
    while matches!(tok!(0), Token::LParen | Token::LSqBracket | Token::Dot | Token::DoubleColon | Token::ExclMark | Token::Colon | Token::TripleDot) {
        match tok!(0) {
            Token::LParen => {
                pos += 1;
                let area_start = span!(-1).0;
                let mut args = vec![];
                while_tok!(!= RParen: {
                    parse!(parse_expr(false) => let arg);
                    args.push(arg);
                    if !matches!(tok!(0), Token::RParen | Token::Comma) {
                        expected_err!(") or ,", tok!(0), span!(0), info )
                    }
                    skip_tok!(Comma);
                });
                let area_end = span!(-1).1;
                value = ASTNode {
                    node: NodeType::Call {
                        args,
                        base: Box::new(value),
                        args_area: CodeArea {
                            source: info.source.clone(),
                            range: (area_start, area_end),
                        }
                    },
                    span: ( start.0, span!(-1).1 )
                }
            },
            Token::LSqBracket => {
                pos += 1;
                let area_start = span!(-1).0;
                let mut args = vec![];
                while_tok!(!= RSqBracket: {
                    parse!(parse_expr(false) => let arg);
                    args.push(arg);
                    if !matches!(tok!(0), Token::RSqBracket | Token::Comma) {
                        expected_err!(") or ,", tok!(0), span!(0), info )
                    }
                    skip_tok!(Comma);
                });
                let area_end = span!(-1).1;
                value = ASTNode {
                    node: NodeType::Index {
                        args,
                        base: Box::new(value),
                        args_area: CodeArea {
                            source: info.source.clone(),
                            range: (area_start, area_end),
                        }
                    },
                    span: ( start.0, span!(-1).1 )
                }
            },
            Token::Dot => {
                pos += 1;
                check_tok!(Ident(member_name) else "member name");
                value = ASTNode {
                    node: NodeType::Member {
                        member: Located {
                            inner: member_name,
                            area: CodeArea {
                                source: info.source.clone(),
                                range: span!(-1),
                            },
                        },
                        
                        base: Box::new(value),
                    },
                    span: ( start.0, span!(-1).1 )
                }
            },
            Token::TripleDot => {
                pos += 1;
                value = ASTNode {
                    node: NodeType::UnboundedRange {
                        base: Box::new(value),
                    },
                    span: ( start.0, span!(-1).1 )
                }
            }
            Token::DoubleColon => {
                pos += 1;
                if matches!(tok!(0), Token::LBracket) {
                    pos += 1;

                    let mut fields = FnvHashMap::default();
                    let mut areas: FnvHashMap<String, CodeArea> = FnvHashMap::default();
                    while_tok!(!= RBracket: {
                        check_tok!(Ident(field) else "field name");
                        let last_area = CodeArea {
                            source: info.source.clone(),
                            range: span!(-1),
                        };
                        if fields.contains_key(&field) {
                            return Err( SyntaxError::DuplicateField {
                                first_used: areas[&field].clone(),
                                field_name: field,
                                used_again: last_area,
                            } )
                        }
                        areas.insert(field.clone(), last_area);
                        let mut value = ASTNode {
                            node: NodeType::Var {
                                var_name: field.clone(),
                            },
                            span: span!(-1),
                        };
                        if_tok!(== Colon: {
                            pos += 1;
                            parse!(parse_expr(false) => value);
                        });

                        fields.insert(field, value);
                        if !matches!(tok!(0), Token::RBracket | Token::Comma) {
                            expected_err!("} or ,", tok!(0), span!(0), info )
                        }
                        skip_tok!(Comma);
                    });
                    value = ASTNode {
                        node: NodeType::StructInstance {
                            base: Box::new(value),
                            field_areas: areas,
                            fields,
                        },
                        span: ( start.0, span!(-1).1 )
                    }

                } else {
                    check_tok!(Ident(assoc) else "identifier or {");
                    value = ASTNode {
                        node: NodeType::Associated {
                            base: Box::new(value),
                            assoc,
                        },
                        span: ( start.0, span!(-1).1 )
                    }
                }
            },
            Token::ExclMark => {
                pos += 1;
                value = ASTNode {
                    node: NodeType::MCCall {
                        base: Box::new(value),
                    },
                    span: ( start.0, span!(-1).1 )
                }
            },
            Token::Colon => {
                pos += 1;
                check_tok!(Ident(name) else "variant name");
                let variant_area = CodeArea {
                    source: info.source.clone(),
                    range: span!(-1),
                };

                match tok!(0) {
                    Token::LParen => {
                        pos += 1;
                        let mut elements = vec![];
                        while_tok!(!= RParen: {
                            parse!(parse_expr(false) => let arg);
                            elements.push(arg);
                            if !matches!(tok!(0), Token::RParen | Token::Comma) {
                                expected_err!(") or ,", tok!(0), span!(0), info )
                            }
                            skip_tok!(Comma);
                        });
                        value = ASTNode {
                            node: NodeType::EnumInstance {
                                base: Box::new(value),
                                variant_name: name,
                                variant: VariantType::Tuple(elements),
                                variant_area,
                            },
                            span: ( start.0, span!(-1).1 )
                        }
                    }
                    Token::LBracket => {
                        pos += 1;

                        let mut fields = FnvHashMap::default();
                        let mut areas: FnvHashMap<String, CodeArea> = FnvHashMap::default();
                        while_tok!(!= RBracket: {
                            check_tok!(Ident(field) else "field name");
                            let last_area = CodeArea {
                                source: info.source.clone(),
                                range: span!(-1),
                            };
                            if fields.contains_key(&field) {
                                return Err( SyntaxError::DuplicateFieldStructVariant {
                                    first_used: areas[&field].clone(),
                                    field_name: field,
                                    used_again: last_area,
                                } )
                            }
                            areas.insert(field.clone(), last_area);
                            let mut value = ASTNode {
                                node: NodeType::Var {
                                    var_name: field.clone(),
                                },
                                span: span!(-1),
                            };
                            if_tok!(== Colon: {
                                pos += 1;
                                parse!(parse_expr(false) => value);
                            });

                            fields.insert(field, value);
                            if !matches!(tok!(0), Token::RBracket | Token::Comma) {
                                expected_err!("} or ,", tok!(0), span!(0), info )
                            }
                            skip_tok!(Comma);
                        });
                        value = ASTNode {
                            node: NodeType::EnumInstance {
                                base: Box::new(value),
                                variant_name: name,
                                variant: VariantType::Struct { fields, field_areas: areas },
                                variant_area,
                            },
                            span: ( start.0, span!(-1).1 )
                        }
                    }
                    _ => {
                        value = ASTNode {
                            node: NodeType::EnumInstance {
                                base: Box::new(value),
                                variant_name: name,
                                variant: VariantType::Unit,
                                variant_area,
                            },
                            span: ( start.0, span!(-1).1 )
                        }
                    }
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
    skip_assignment: bool,
) -> Result<(ASTNode, usize), SyntaxError> {
    

    return parse_op(tokens, pos, info, if skip_assignment {1} else {0})
}


fn parse_statement(
    tokens: &Tokens,
    mut pos: usize,
    info: &ParseInfo,
) -> Result<(ASTNode, usize), SyntaxError> {
    parse_util!(tokens, pos, info);

    

    if_tok!(== Export: {
        let start = span!(0);
        pos += 1;
        let export_name;
        match tok!(0) {
            Token::Let | Token::Func | Token::Struct | Token::Enum | Token::Module => {
                match tok!(1) {
                    Token::Ident(name) => {
                        export_name = name.clone();
                        parse!(parse_expr(false) => let value);

                        if !matches!(tok!(-1), Token::RBracket) {
                            check_tok!(Eol else ';');
                        }
                        skip_toks!(Eol);

                        ret!( NodeType::Export {
                            name: export_name,
                            value: Box::new(value),
                        } => start.0, span!(-1).1 )
                    }

                    // cheeky thing to make me not have to write the same errors twice
                    _ => { parse!(parse_expr(false) => let _value); panic!() },
                }
            }
            _ => expected_err!("let, func, struct, mod, or enum", tok!(0), span!(0), info ),
        }
    } else {
        parse!(parse_expr(false) => let value);
        if !matches!(tok!(-1), Token::RBracket) {
            check_tok!(Eol else ';');
        }
        skip_toks!(Eol);
    
        Ok((value, pos))
    })



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




impl Hash for VariantType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            VariantType::Unit => core::mem::discriminant(self).hash(state),
            VariantType::Tuple(t) => t.hash(state),
            VariantType::Struct { fields, field_areas } => {
                for (k, v) in fields {
                    k.hash(state);
                    v.hash(state);
                }
                for (k, v) in field_areas {
                    k.hash(state);
                    v.hash(state);
                }
            },
        }
    }
}


impl Hash for NodeType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            NodeType::Value { value } => {
                value.hash(state, None);
            },
            NodeType::Op { left, op, right } => {
                left.hash(state);
                op.hash(state);
                right.hash(state);
            },
            NodeType::Unary { op, node } => {
                op.hash(state);
                node.hash(state);
            },
            NodeType::Declaration { left, right } => {
                left.hash(state);
                right.hash(state);
            },
            NodeType::Var { var_name } => {
                var_name.hash(state);
            },
            NodeType::StatementList { statements } => {
                statements.hash(state);
            },
            NodeType::Block { code, typ } => {
                code.hash(state);
                typ.hash(state);
            },
            NodeType::If { cond, code, else_branch } => {
                cond.hash(state);
                code.hash(state);
                else_branch.hash(state);
            },
            NodeType::While { cond, code } => {
                cond.hash(state);
                code.hash(state);
            },
            NodeType::For { left, iter, code } => {
                left.hash(state);
                iter.hash(state);
                code.hash(state);
            },
            NodeType::Loop { code } => {
                code.hash(state);
            },
            NodeType::Call { base, args, args_area } => {
                base.hash(state);
                args.hash(state);
                args_area.hash(state);
            },
            NodeType::FuncDef { func_name, args, header_area, code } => {
                func_name.hash(state);
                args.hash(state);
                header_area.hash(state);
                code.hash(state);
            },
            NodeType::Lambda { args, header_area, code } => {
                args.hash(state);
                header_area.hash(state);
                code.hash(state);
            },
            NodeType::Array { elements } => {
                elements.hash(state);
            },
            NodeType::Tuple { elements } => {
                elements.hash(state);
            },
            NodeType::Index { base, args, args_area } => {
                base.hash(state);
                args.hash(state);
                args_area.hash(state)             },
            NodeType::Dictionary { map } => {
                for (k, v) in map {
                    k.hash(state);
                    v.hash(state);
                }
            },
            NodeType::Member { base, member } => {
                base.hash(state);
                member.hash(state);
            },
            NodeType::Return { node } => {
                node.hash(state);
            },
            NodeType::Break { node } => {
                node.hash(state);
            },
            NodeType::Throw { msg } => {
                msg.hash(state);
            },
            NodeType::UnboundedRange { base } => {
                base.hash(state);
            },
            NodeType::StructDef { struct_name, fields, field_areas, def_area } => {
                struct_name.hash(state);
                for (k, v) in fields {
                    k.hash(state);
                    v.hash(state);
                }
                for (k, v) in field_areas {
                    k.hash(state);
                    v.hash(state);
                }
                def_area.hash(state);
            },
            NodeType::StructInstance { base, field_areas, fields } => {
                base.hash(state);
                for (k, v) in fields {
                    k.hash(state);
                    v.hash(state);
                }
                for (k, v) in field_areas {
                    k.hash(state);
                    v.hash(state);
                }
            },
            NodeType::Impl { type_var, fields } => {
                type_var.hash(state);
                fields.hash(state);
            },
            NodeType::Associated { base, assoc } => {
                base.hash(state);
                assoc.hash(state);
            },
            NodeType::Match { value, arms } => {
                value.hash(state);
                arms.hash(state);
            },
            NodeType::EnumDef { enum_name, variants, variant_areas, def_area } => {
                enum_name.hash(state);
                for (k, v) in variants {
                    k.hash(state);
                    v.hash(state);
                }
                for (k, v) in variant_areas {
                    k.hash(state);
                    v.hash(state);
                }
                def_area.hash(state);
            },
            NodeType::EnumInstance { base, variant_name, variant_area, variant } => {
                base.hash(state);
                variant_name.hash(state);
                variant_area.hash(state);
                variant.hash(state);
            },
            NodeType::ModuleDef { module_name, def_area } => {
                module_name.hash(state);
                def_area.hash(state);
            },
            NodeType::Export { name, value } => {
                name.hash(state);
                value.hash(state);
            },
            NodeType::Import { path, cached } => {
                path.hash(state);
                cached.hash(state);
            },
            NodeType::MCCall { base } => {
                base.hash(state);
            },
            NodeType::McVector { x, y, z, rot } => {
                x.hash(state);
                y.hash(state);
                z.hash(state);
                rot.hash(state);
            },
            NodeType::Extract { value } => {
                value.hash(state);
            },
            NodeType::Selector { selector_type, args } => {
                selector_type.hash(state);
                args.hash(state);
            },
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}
