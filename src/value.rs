use std::{collections::HashSet, hash::Hash};

use fnv::FnvHashMap;

use crate::{parser::{ASTNode, Located}, interpreter::{ScopePos, ValuePos, Globals, TypePos, CustomStruct, McFuncID, CustomEnum, Module}, CodeArea, builtins::{BuiltinType, builtin_type_str, Builtin, builtin_to_name}, error::RuntimeError, lexer::SelectorType};



#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Function {
    pub args: Vec<(Located<String>, Option<ValuePos>, Option<ValuePos>, bool)>,
    pub code: Box<ASTNode>,
    pub parent_scope: ScopePos,
    pub header_area: CodeArea,
    pub self_arg: Option<Box<ASTNode>>,
}


#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ValueType {
    Builtin(BuiltinType),
    CustomStruct(TypePos),
    CustomEnum(TypePos),
    Module(TypePos),
}

impl ValueType {
    pub fn to_str(&self, globals: &Globals) -> String {
        match self {
            ValueType::Builtin(b) => builtin_type_str(b.clone()),
            ValueType::CustomStruct(id) => match &globals.custom_structs.map[id] {
                CustomStruct { name, .. } => name.to_string()
            },
            ValueType::CustomEnum(id) => match &globals.custom_enums.map[id] {
                CustomEnum { name, .. } => name.to_string()
            },
            ValueType::Module(id) => match &globals.modules.map[id] {
                Module { name, .. } => name.to_string()
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Type(ValueType),

    Either(Box<Pattern>, Box<Pattern>),
    Not(Box<Pattern>),
    
    Any,

    Option(Box<Pattern>),

    Array(Box<Pattern>, Option<usize>),
    Tuple(Vec<Pattern>),
    Dict(FnvHashMap<String, Pattern>),
}

impl Pattern {
    pub fn to_str(&self, globals: &Globals) -> String {
        match self {
            Pattern::Any => format!("_"),
            Pattern::Type(t) => t.to_str(globals),
            Pattern::Either(a, b) => format!("({} | {})", a.to_str(globals), b.to_str(globals)),
            Pattern::Not(a) => format!("!{}", a.to_str(globals)),
            Pattern::Array(t, n) => match n {
                Some(n) => format!("array[{}, {}]", t.to_str(globals), n),
                None => format!("array[{}]", t.to_str(globals)),
            },
            Pattern::Option(o) => format!("#[{}]", o.to_str(globals)),
            Pattern::Tuple(arr) => format!("tuple[{}]", arr.iter().map(|el| el.to_str(globals)).collect::<Vec<String>>().join(", ")),
            Pattern::Dict(map) => format!("dict[{{{}}}]", map.iter().map(|(k, p)| format!("{}: {}", k, p.to_str(globals))).collect::<Vec<String>>().join(", ")),
        }
    }
}


impl Pattern {
    pub fn hash<H: std::hash::Hasher>(&self, state: &mut H, globals: &Globals) {
        match self {
            Pattern::Any => core::mem::discriminant(self).hash(state),
            Pattern::Type(t) => t.hash(state),
            Pattern::Either(a, b) => {
                a.hash(state, globals);
                b.hash(state, globals);
            },
            Pattern::Not(a) => a.hash(state, globals),
            Pattern::Option(o) => o.hash(state, globals),
            Pattern::Array(t, n) => {
                t.hash(state, globals);
                n.hash(state);
            }
            Pattern::Tuple(arr) => for i in arr {
                i.hash(state, globals)
            },
            Pattern::Dict(map) => {
                for (k, v) in map {
                    k.hash(state);
                    v.hash(state, globals);
                }
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum CoordType<T> {
    Absolute(T),
    Tilde(T),
    Caret(T),
}

impl<T> CoordType<T> {
    pub fn get_inner(&self) -> &T {
        match self {
            CoordType::Absolute(v) => v,
            CoordType::Tilde(v) => v,
            CoordType::Caret(v) => v,
        }
    }

    pub fn reapply<U>(&self, val: U) -> CoordType<U> {
        match self {
            CoordType::Absolute(_) => CoordType::Absolute(val),
            CoordType::Tilde(_) => CoordType::Tilde(val),
            CoordType::Caret(_) => CoordType::Caret(val),
        }
    }

    pub fn prefix(&self) -> &str {
        match self {
            CoordType::Absolute(_) => "",
            CoordType::Tilde(_) => "~",
            CoordType::Caret(_) => "^",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum McVector {
    Pos(CoordType<Box<Value>>, CoordType<Box<Value>>, CoordType<Box<Value>>),
    WithRot(CoordType<Box<Value>>, CoordType<Box<Value>>, CoordType<Box<Value>>, CoordType<Box<Value>>, CoordType<Box<Value>>),
}

impl McVector {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H, globals: &Globals) {
        match self {
            McVector::Pos(x, y, z) => {
                core::mem::discriminant(x).hash(state);
                x.get_inner().hash(state, Some(globals));
                core::mem::discriminant(y).hash(state);
                y.get_inner().hash(state, Some(globals));
                core::mem::discriminant(z).hash(state);
                z.get_inner().hash(state, Some(globals));
            },
            McVector::WithRot(x, y, z, h, v) => {
                core::mem::discriminant(x).hash(state);
                x.get_inner().hash(state, Some(globals));
                core::mem::discriminant(y).hash(state);
                y.get_inner().hash(state, Some(globals));
                core::mem::discriminant(z).hash(state);
                z.get_inner().hash(state, Some(globals));
                core::mem::discriminant(h).hash(state);
                h.get_inner().hash(state, Some(globals));
                core::mem::discriminant(v).hash(state);
                v.get_inner().hash(state, Some(globals));
            },
        }
    }
}



#[derive(Debug, Clone, PartialEq)]
pub struct Selector {
    pub selector_type: SelectorType,
    pub args: Vec<(String, ValuePos)>,
}



#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: Option<f64>,
    pub end: Option<f64>,
    pub step: f64,
}

impl Hash for Range {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.map(|v| v.to_bits()).hash(state);
        self.end.map(|v| v.to_bits()).hash(state);
        self.step.to_bits().hash(state);
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Option(Option<ValuePos>),
    Builtin(Builtin),
    Function(Function),
    Array(Vec<ValuePos>),
    Tuple(Vec<ValuePos>),
    Dictionary(FnvHashMap<String, ValuePos>),
    Type(ValueType),
    Pattern(Pattern),
    Range(Range),

    McFunc(McFuncID),

    McVector(McVector),
    Selector(Selector),

    StructInstance {
        struct_id: TypePos,
        fields: FnvHashMap<String, ValuePos>
    },
    EnumInstance {
        enum_id: TypePos,
        variant_name: String,
        variant: InstanceVariant,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstanceVariant {
    Unit,
    Tuple(Vec<ValuePos>),
    Struct { fields: FnvHashMap<String, ValuePos> }
}


impl Value {

    pub fn unit() -> Value {
        Value::Tuple(vec![])
    }
    pub fn some(i: ValuePos) -> Value {
        Value::Option(Some(i))
    }
    pub fn none() -> Value {
        Value::Option(None)
    }

    pub fn typ(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Builtin(BuiltinType::Number),
            Value::Boolean(_) => ValueType::Builtin(BuiltinType::Bool),
            Value::String(_) => ValueType::Builtin(BuiltinType::String),
            Value::Option(_) => ValueType::Builtin(BuiltinType::Option),
            Value::Builtin(_) => ValueType::Builtin(BuiltinType::Builtin),
            Value::Function{..} => ValueType::Builtin(BuiltinType::Function),
            Value::Array(_) => ValueType::Builtin(BuiltinType::Array),
            Value::Tuple(_) => ValueType::Builtin(BuiltinType::Tuple),
            Value::Dictionary(_) => ValueType::Builtin(BuiltinType::Dict),
            Value::Type(_) => ValueType::Builtin(BuiltinType::Type),
            Value::Pattern(_) => ValueType::Builtin(BuiltinType::Pattern),
            Value::Range(_) => ValueType::Builtin(BuiltinType::Range),

            Value::McFunc(_) => ValueType::Builtin(BuiltinType::McFunc),
            Value::McVector(_) => ValueType::Builtin(BuiltinType::McVector),
            Value::Selector(_) => ValueType::Builtin(BuiltinType::Selector),

            Value::StructInstance { struct_id, .. } => ValueType::CustomStruct(*struct_id),
            Value::EnumInstance { enum_id, .. } => ValueType::CustomEnum(*enum_id),
        }
    }

    pub fn type_str(&self, globals: &Globals) -> String {
        self.typ().to_str(globals)
    }

    pub fn to_str<'a>(&'a self, globals: &'a Globals, visited: &mut Vec<&'a Value>) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => format!("{}", s),
            Value::Option(o) => match o {
                Some(v) => format!("#({})", globals.get(*v).value.to_str(globals, &mut vec![])),
                None => format!("#"),
            },
            Value::Builtin(b) => format!("<builtin: {}>", builtin_to_name(*b)),
            Value::Function( Function {args, ..} )=> format!("({}) => ...", args.iter().map(|(Located {inner, ..}, ..)| inner.clone()).collect::<Vec<String>>().join(", ")),
            Value::Array(arr) => {
                if visited.contains(&self) {
                    return "[...]".to_string()
                }
                visited.push( self );
                let out_str = "[".to_string() + &arr.into_iter().map(
                    |i| globals.get(*i).value.to_str(globals, visited)
                ).collect::<Vec<String>>().join(", ") + "]";
                visited.pop();
                out_str
            },
            Value::Tuple(arr) => {
                if visited.contains(&self) {
                    return "(...)".to_string()
                }
                visited.push( self );
                let out_str = "(".to_string() + &arr.into_iter().map(
                    |i| globals.get(*i).value.to_str(globals, visited)
                ).collect::<Vec<String>>().join(", ") + ")";
                visited.pop();
                out_str
            },
            Value::Dictionary(map) => {
                if visited.contains(&self) {
                    return "{...}".to_string()
                }
                visited.push( self );
                let out_str = "{".to_string() + &map.into_iter().map(
                    |(k, v)| format!("{}: {}", k, globals.get(*v).value.to_str(globals, visited))
                ).collect::<Vec<String>>().join(", ") + "}";
                visited.pop();
                out_str
            },
            Value::Type(v) => match v {
                ValueType::Builtin(t) => format!("<type: {}>", builtin_type_str(t.clone())),
                ValueType::CustomStruct(pos) => match &globals.custom_structs.map[pos] {
                    CustomStruct { name, .. } => format!("<struct {}: {}>", pos, name)
                },
                ValueType::CustomEnum(pos) => match &globals.custom_enums.map[pos] {
                    CustomEnum { name, .. } => format!("<enum {}: {}>", pos, name)
                },
                ValueType::Module(pos) => match &globals.modules.map[pos] {
                    Module { name, .. } => format!("<mod {}: {}>", pos, name)
                },
            },
            Value::StructInstance { struct_id, fields } => {

                let name = match &globals.custom_structs.map[struct_id] {
                    CustomStruct { name, .. } => name,
                };

                if visited.contains(&self) {
                    return name.clone() + "::{...}"
                }
                visited.push( self );
                let out_str = "{".to_string() + &fields.into_iter().map(
                    |(k, v)| format!("{}: {}", k, globals.get(*v).value.to_str(globals, visited))
                ).collect::<Vec<String>>().join(", ") + "}";
                visited.pop();
                format!("{}::{}", name, out_str)
            },
            Value::EnumInstance {
                enum_id,
                variant_name,
                variant
            } => {
                let name = match &globals.custom_enums.map[enum_id] {
                    CustomEnum { name, .. } => name,
                };

                match variant {
                    InstanceVariant::Unit => format!("{}:{}", name, variant_name),
                    InstanceVariant::Tuple(arr) => {
                        if visited.contains(&self) {
                            return format!("{}:{} (...)", name, variant_name)
                        }
                        visited.push( self );
                        let out_str = "(".to_string() + &arr.into_iter().map(
                            |i| globals.get(*i).value.to_str(globals, visited)
                        ).collect::<Vec<String>>().join(", ") + ")";
                        visited.pop();
                        format!("{}:{} {}", name, variant_name, out_str)
                    },
                    InstanceVariant::Struct { fields } => {
                        if visited.contains(&self) {
                            return format!("{}:{} {{...}} ", name, variant_name)
                        }
                        visited.push( self );
                        let out_str = "{".to_string() + &fields.into_iter().map(
                            |(k, v)| format!("{}: {}", k, globals.get(*v).value.to_str(globals, visited))
                        ).collect::<Vec<String>>().join(", ") + "}";
                        visited.pop();
                        format!("{}:{} {}", name, variant_name, out_str)
                    },
                }

            }
            Value::Pattern(p) => p.to_str(globals),
            Value::Range(Range { start, end, step }) => {
                if *step != 1.0 {
                    format!(
                        "{}..{}..{}",
                        if let Some(n) = start {n.to_string()} else {".".to_string()},
                        step,
                        if let Some(n) = end {n.to_string()} else {".".to_string()}
                    )
                } else {
                    format!(
                        "{}..{}",
                        if let Some(n) = start {n.to_string()} else {".".to_string()},
                        if let Some(n) = end {n.to_string()} else {".".to_string()}
                    )
                }
            }
            Value::McFunc(_) => format!("!{{...}}"),
            Value::McVector(v) => {
                match v {
                    McVector::Pos(
                        x,
                        y,
                        z,
                    ) => format!("v\\{}{} {}{} {}{}\\",
                        x.prefix(), x.get_inner().to_str(globals, visited),
                        y.prefix(), y.get_inner().to_str(globals, visited),
                        z.prefix(), z.get_inner().to_str(globals, visited),
                    ),
                    McVector::WithRot(
                        x,
                        y,
                        z,
                        h,
                        v,
                    ) => format!("v\\{}{} {}{} {}{}, {}{} {}{}\\",
                        x.prefix(), x.get_inner().to_str(globals, visited),
                        y.prefix(), y.get_inner().to_str(globals, visited),
                        z.prefix(), z.get_inner().to_str(globals, visited),
                        h.prefix(), h.get_inner().to_str(globals, visited),
                        v.prefix(), v.get_inner().to_str(globals, visited),
                    ),
                }
            },
            Value::Selector(s) => {
                let mut out_str = "@".to_string() + match s.selector_type {
                    SelectorType::Players => "a",
                    SelectorType::Entities => "e",
                    SelectorType::Nearest => "p",
                    SelectorType::Random => "r",
                    SelectorType::Executor => "s",
                };
                if visited.contains(&self) {
                    return out_str + "[...]"
                }
                visited.push( self );
                if s.args.len() > 0 {
                    out_str += &("[".to_string() + &s.args.iter().map(
                        |(s, v)| format!("{} = {}", s, globals.get(*v).value.to_str(globals, visited))
                    ).collect::<Vec<String>>().join(", ") + "]")[..];
                }
                visited.pop();
                out_str
            },
        }
    }

    pub fn matches_pat(&self, p: &Pattern, globals: &Globals) -> Result<bool, RuntimeError> {
        match p {
            Pattern::Any => Ok( true ),
            Pattern::Type(t) => Ok( self.typ() == t.clone() ),
            Pattern::Either(a, b) => Ok( self.matches_pat(a, globals)? || self.matches_pat(b, globals)? ),
            Pattern::Not(a) => Ok( !self.matches_pat(a, globals)? ),
            Pattern::Array(inner, n) => {
                match self {
                    Value::Array(arr) => {
                        match n {
                            Some(n) => if *n != arr.len() {return Ok(false)}
                            None => (),
                        }
                        for i in arr {
                            if !globals.get(*i).value.matches_pat(inner, globals)? {
                                return Ok(false)
                            }
                        }
                        Ok(true)
                    },
                    _ => Ok(false),
                }
            },
            Pattern::Tuple(v) => {
                match self {
                    Value::Tuple(arr) => {
                        if arr.len() != v.len() {return Ok(false)}
                        for (i, p) in arr.iter().zip(v) {
                            if !globals.get(*i).value.matches_pat(p, globals)? {
                                return Ok(false)
                            }
                        }
                        Ok(true)
                    },
                    _ => Ok(false),
                }
            },
            Pattern::Dict(p_map) => {
                match self {
                    Value::Dictionary(map) => {
                        if p_map.len() != map.len() {return Ok(false)}
                        for (k, p) in p_map {
                            match map.get(k) {
                                Some(v) => {
                                    if !globals.get(*v).value.matches_pat(p, globals)? {
                                        return Ok(false)
                                    }
                                },
                                None => return Ok(false),
                            }
                        }
                        Ok(true)
                    },
                    _ => Ok(false),
                }
            },
            Pattern::Option(p) => {
                match self {
                    Value::Option(o) => match o {
                        None => Ok(true),
                        Some(id) => globals.get(*id).value.matches_pat(p, globals),
                    }
                    _ => Ok(false),
                }
            },
        }
    }


    pub fn get_references(&self, globals: &Globals, values: &mut HashSet<ValuePos>, scopes: &mut HashSet<ScopePos>) {
        match self {
            Value::Function(Function {
                args,
                parent_scope,
                ..
            }) => {
                scopes.insert(*parent_scope);
                for (_, t, d, _) in args {
                    if let Some(id) = t {
                        values.insert(*id);
                        globals.get(*id).value.get_references(globals, values, scopes);
                    }
                    if let Some(id) = d {
                        values.insert(*id);
                        globals.get(*id).value.get_references(globals, values, scopes);
                    }
                }
            },
            Value::Array(arr) => {
                for i in arr {
                    values.insert(*i);
                    globals.get(*i).value.get_references(globals, values, scopes);
                }
            },
            Value::Tuple(arr) => {
                for i in arr {
                    values.insert(*i);
                    globals.get(*i).value.get_references(globals, values, scopes);
                }
            },
            Value::Dictionary(map) => {
                for (_, i) in map {
                    values.insert(*i);
                    globals.get(*i).value.get_references(globals, values, scopes);
                }
            },
            Value::Option(o) => {
                if let Some(i) = o {
                    values.insert(*i);
                    globals.get(*i).value.get_references(globals, values, scopes);
                }
            },
            Value::StructInstance { fields, .. } => {
                for (_, i) in fields {
                    values.insert(*i);
                    globals.get(*i).value.get_references(globals, values, scopes);
                }
            },
            Value::EnumInstance { variant, .. } => {
                match variant {
                    InstanceVariant::Unit => (),
                    InstanceVariant::Tuple(arr) => {
                        for i in arr {
                            values.insert(*i);
                            globals.get(*i).value.get_references(globals, values, scopes);
                        }
                    },
                    InstanceVariant::Struct { fields } => {
                        for (_, i) in fields {
                            values.insert(*i);
                            globals.get(*i).value.get_references(globals, values, scopes);
                        }
                    },
                }
            },
            Value::Selector( Selector {args, .. }) => {
                for (_, i) in args{
                    values.insert(*i);
                    globals.get(*i).value.get_references(globals, values, scopes);
                }
            },
            _ => (),
        }
    }







}

#[derive(Clone)]
pub enum ValueIter {
    List(Vec<Value>, usize),
    Range {
        start: f64,
        step: f64,
        end: Option<f64>,
        current: f64,
    },
}

impl Iterator for ValueIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {

        match self {
            ValueIter::List(v, i) => {
                let ret = v.get(*i).map(|v| v.clone());
                *i += 1;
                ret
            },
            ValueIter::Range { start, step, end, current } => {
                let ret = *current;
                *current += *step;
                return if let Some(e) = *end {
                    if if *start <= e {
                        ret < e
                    } else {
                        ret > e
                    } { Some(Value::Number(ret)) } else {None}
                } else {
                    Some(Value::Number(ret))
                }
            },
        }

        // let ret = self.current;
        // self.current += self.step;
        // return if let Some(e) = self.end {
        //     if if self.start <= e {
        //         ret < e
        //     } else {
        //         ret > e
        //     } { Some(Value::Number(ret)) } else {None}
        // } else {
        //     Some(Value::Number(ret))
        // }
    }
}



pub mod value_ops {

    use fnv::FnvHashMap;

    use crate::{interpreter::{StoredValue, Globals}, CodeArea, value::{Value, ValueType, InstanceVariant}, error::RuntimeError, builtins::BuiltinType};

    use super::{Pattern, Range, ValueIter};

    pub fn to_bool(a: &StoredValue, area: CodeArea, globals: &Globals) -> Result<bool, RuntimeError> {
        match &a.value {
            Value::Boolean(b) => Ok(*b),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "bool".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn iter(a: &StoredValue, area: CodeArea, globals: &Globals) -> Result<ValueIter, RuntimeError> {
        let val = a.value.clone();
        match val {
            Value::Array(arr) => Ok(
                ValueIter::List(arr.iter().map( |e| globals.get(*e).value.clone() ).collect(), 0)
            ),
            Value::Dictionary(map) => Ok(
                ValueIter::List(map.iter().map( |(s, _)| Value::String(s.clone()) ).collect(), 0)
            ),
            
            Value::Range(Range { start, end, step }) => Ok(
                {
                    if let None = start {
                        return Err( RuntimeError::CannotIter {
                            typ: a.value.type_str(globals),
                            area,
                            reason: "This range has no start".to_string()
                        } );
                    }
                    ValueIter::Range { start: start.unwrap(), step, end, current: start.unwrap() }
                }
            ),
            Value::String(s) => Ok(
                ValueIter::List(s.chars().map( |c| Value::String(c.to_string()) ).collect(), 0)
            ),
        
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "array, range, string, or dict".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn convert(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {




            (v, Value::Type(ValueType::Builtin(BuiltinType::String))) => Ok(Value::String( v.to_str(globals, &mut vec![]) )),
            (v, Value::Type(ValueType::Builtin(BuiltinType::Type))) => Ok(Value::Type(v.typ())),

            (Value::String(s), Value::Type(ValueType::Builtin(BuiltinType::Array))) => {
                let mut v = vec![];
                for i in s.chars() {
                    v.push(globals.insert_value(
                        Value::String(i.to_string()),
                        area.clone(),
                    ))
                }
                Ok(Value::Array(v))
            },
            (Value::Range(Range { start, end, step }), Value::Type(t @ ValueType::Builtin(BuiltinType::Array))) => {
                match (start, end) {
                    (_, None) | (None, _) => Err( RuntimeError::CannotConvert {
                        type1: "range with undefined ends".to_string(),
                        type2: t.to_str(globals),
                        area,
                        area1: a.def_area.clone(),
                    } ),
                    _ => {
                        let mut v = vec![];
                        let iter = iter(a, area.clone(), globals)?;
                        for i in iter {
                            v.push(globals.insert_value(
                                i,
                                area.clone(),
                            ))
                        }
                        Ok(Value::Array(v))
                    }
                }
            },

            (Value::Tuple(v), Value::Type(ValueType::Builtin(BuiltinType::Array))) => {
                Ok(Value::Array(v.clone()))
            },
            (Value::Array(v), Value::Type(ValueType::Builtin(BuiltinType::Tuple))) => {
                Ok(Value::Tuple(v.clone()))
            },

            (value, Value::Type(t)) => {
                if value.type_str(globals) == t.to_str(globals) {
                    Ok( value.clone() )
                } else {
                    Err( RuntimeError::CannotConvert {
                        type1: value.type_str(globals),
                        type2: t.to_str(globals),
                        area,
                        area1: a.def_area.clone(),
                        // area2: b.def_area.clone(),
                    } )
                }
            }
            
            (_, value) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "type".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }

    
    pub fn is_op_raw(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<bool, RuntimeError> {
        // println!("aa {:?} {:?}", &a.value, &b.value);
        match (&a.value, &b.value) {
            (v, Value::Type(t)) => Ok( v.typ() == t.clone() ),

            (v, Value::Pattern(p)) => Ok( v.matches_pat(p, globals)? ),
            
            (_, value) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "type or pattern".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }

    pub fn is_op(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        Ok( Value::Boolean( is_op_raw(a, b, area, globals)? ) )
    }

    // pub fn overwrite(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
    //     match (&a.value, &b.value) {
    //         (_, b) => Ok( b.clone() )
    //     }
    // }


    pub fn plus(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(s1.to_string() + s2)),
            (Value::Array(a1), Value::Array(a2)) => {
                let mut new_vec = vec![];
                for i in a1 {
                    new_vec.push( globals.clone_value(*i, Some(area.clone())) );
                }
                for i in a2 {
                    new_vec.push( globals.clone_value(*i, Some(area.clone())) );
                }
                Ok(Value::Array(new_vec))
            },
            (Value::Tuple(a1), Value::Tuple(a2)) => {
                let mut new_vec = vec![];
                for i in a1 {
                    new_vec.push( globals.clone_value(*i, Some(area.clone())) );
                }
                for i in a2 {
                    new_vec.push( globals.clone_value(*i, Some(area.clone())) );
                }
                Ok(Value::Tuple(new_vec))
            },
            (Value::Dictionary(m1), Value::Dictionary(m2)) => {
                let mut new_map = FnvHashMap::default();
                for i in m1 {
                    new_map.insert(i.0.clone(),  globals.clone_value(*i.1, Some(area.clone())) );
                }
                for i in m2 {
                    new_map.insert(i.0.clone(),  globals.clone_value(*i.1, Some(area.clone())) );
                }
                Ok(Value::Dictionary(new_map))
            },
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number or string and string".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn minus(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn mult(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
            (Value::String(s), Value::Number(n)) |
            (Value::Number(n), Value::String(s)) => Ok(Value::String(s.repeat(
                if *n < 0.0 {0} else {*n as usize}
            ))),
            (Value::Array(arr), Value::Number(n)) |
            (Value::Number(n), Value::Array(arr)) => {
                let mut new_vec = vec![];
                if *n >= 0.0 {
                    for _ in 0..(*n as usize) {
                        for i in arr {
                            new_vec.push( globals.clone_value(*i, Some(area.clone())) );
                        }
                    }
                }
                Ok(Value::Array(new_vec))
            },
            (Value::Tuple(arr), Value::Number(n)) |
            (Value::Number(n), Value::Tuple(arr)) => {
                let mut new_vec = vec![];
                if *n >= 0.0 {
                    for _ in 0..(*n as usize) {
                        for i in arr {
                            new_vec.push( globals.clone_value(*i, Some(area.clone())) );
                        }
                    }
                }
                Ok(Value::Tuple(new_vec))
            },

            (Value::Range(r), Value::Number(mult)) |
            (Value::Number(mult), Value::Range(r)) => {
                let new_range = Range {
                    start: if let Some(n) = r.start {Some(n * mult)} else { None },
                    end: if let Some(n) = r.end {Some(n * mult)} else { None },
                    step: r.step * mult,
                };
                Ok(Value::Range(new_range))
            },
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number or string and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn div(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn modulo(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 % n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn eucl_modulo(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1.rem_euclid(*n2))),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn pow(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1.powf(*n2))),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }

    pub fn identity(a: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Number(n1) => Ok(Value::Number(*n1)),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), a.def_area.clone())],
                } )
            }
        }
    }
    pub fn negate(a: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Number(n) => Ok(Value::Number(-n)),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), a.def_area.clone())],
                } )
            }
        }
    }

    
    pub fn not(a: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            Value::Pattern(p) => Ok(Value::Pattern(Pattern::Not(Box::new(p.clone())))),
            Value::Type(t) => Ok(Value::Pattern(Pattern::Not(Box::new(Pattern::Type(t.clone()))))),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "bool, pattern, or type".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn eq(a: &StoredValue, b: &StoredValue, globals: &mut Globals) -> bool {

        macro_rules! eq_id {
            ($a:expr, $b:expr) => {
                eq(&globals.get(*$a).clone(), &globals.get(*$b).clone(), globals)
            }
        }

        match (&a.value, &b.value) {
            (Value::Number(v1), Value::Number(v2)) => v1 == v2,
            (Value::String(v1), Value::String(v2)) => v1 == v2,
            (Value::Boolean(v1), Value::Boolean(v2)) => v1 == v2,

            (Value::Option(None), Value::Option(None)) => true,
            (Value::Option(Some(v1)), Value::Option(Some(v2))) => eq_id!(v1, v2),
            
            (Value::Builtin(v1), Value::Builtin(v2)) => v1 == v2,
            (Value::Function(v1), Value::Function(v2)) => v1 == v2,

            (Value::Array(arr1), Value::Array(arr2)) | (Value::Tuple(arr1), Value::Tuple(arr2)) => {
                if arr1.len() != arr2.len() {
                    return false
                }

                for (i, j) in arr1.iter().zip(arr2) {
                    if !eq_id!(i, j) {
                        return false
                    }
                }
                true
                
            },
            (Value::Dictionary(d1), Value::Dictionary(d2)) => {
                if d1.len() != d2.len() {
                    return false
                }

                for (k, v1) in d1 {
                    match d2.get(k) {
                        Some(v2) => {
                            if !eq_id!(v1, v2) {
                                return false
                            }
                        },
                        None => return false,
                    }
                }
                true
            },

            (Value::Type(v1), Value::Type(v2)) => v1 == v2,
            (Value::Pattern(v1), Value::Pattern(v2)) => v1 == v2,
            (Value::Range(v1), Value::Range(v2)) => v1 == v2,

            (Value::McFunc(v1), Value::McFunc(v2)) => v1 == v2,
            (Value::McVector(v1), Value::McVector(v2)) => v1 == v2,

            (Value::Selector(v1), Value::Selector(v2)) => {
                if v1.selector_type != v2.selector_type {
                    return false
                }
                
                if v1.args.len() != v2.args.len() {
                    return false
                }

                for ((k1, i), (k2, j)) in v1.args.iter().zip(&v2.args) {
                    if k1 != k2 || !eq_id!(i, j) {
                        return false
                    }
                }
                true
                
            },


            (Value::StructInstance {
                struct_id: id1,
                fields: f1
            }, Value::StructInstance {
                struct_id: id2,
                fields: f2
            }) => {

                if id1 != id2 {
                    return false
                }

                for (k, v1) in f1 {
                    match f2.get(k) {
                        Some(v2) => {
                            if !eq_id!(v1, v2) {
                                return false
                            }
                        },
                        None => return false,
                    }
                }
                true
                
            },

            (Value::EnumInstance {
                enum_id: id1,
                variant_name: n1,
                variant: v1,
            }, Value::EnumInstance {
                enum_id: id2,
                variant_name: n2,
                variant: v2,
            }) => {

                if id1 != id2 {
                    return false
                }
                if n1 != n2 {
                    return false
                }
                
                match (v1, v2) {
                    (InstanceVariant::Unit, InstanceVariant::Unit) => true,
                    (InstanceVariant::Tuple(arr1), InstanceVariant::Tuple(arr2)) => {
                        if arr1.len() != arr2.len() {
                            return false
                        }
        
                        for (i, j) in arr1.iter().zip(arr2) {
                            if !eq_id!(i, j) {
                                return false
                            }
                        }
                        true
                    },
                    (InstanceVariant::Struct{
                        fields: f1,
                    }, InstanceVariant::Struct{
                        fields: f2,
                    }) => {
                        for (k, v1) in f1 {
                            match f2.get(k) {
                                Some(v2) => {
                                    if !eq_id!(v1, v2) {
                                        return false
                                    }
                                },
                                None => return false,
                            }
                        }
                        true
                    },
                    _ => false,

                }
                
            },

            (_, _) => false,
        }
    }


    pub fn unary_range(a: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Number(n) => Ok(Value::Range(Range{
                start: Some(0.0),
                end: Some(*n),
                step: 1.0,
            })),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), a.def_area.clone())],
                } )
            }
        }
    }
    pub fn unary_unbounded_range(a: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Number(n) => Ok(Value::Range(Range{
                start: None,
                end: Some(*n),
                step: 1.0,
            })),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn eq_op(a: &StoredValue, b: &StoredValue, _area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        Ok(Value::Boolean(eq(a, b, globals)))
    }
    pub fn neq_op(a: &StoredValue, b: &StoredValue, _area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        Ok(Value::Boolean(!eq(a, b, globals)))
    }
    pub fn greater(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 > n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn lesser(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 < n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn greater_eq(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 >= n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn lesser_eq(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 <= n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }

    
    pub fn either(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {

            (Value::Type(t1), Value::Type(t2)) => Ok( Value::Pattern(Pattern::Either(Box::new(Pattern::Type(t1.clone())), Box::new(Pattern::Type(t2.clone())))) ),

            (Value::Type(t), Value::Pattern(p)) |
            (Value::Pattern(p), Value::Type(t)) => Ok( Value::Pattern(Pattern::Either(Box::new(Pattern::Type(t.clone())), Box::new(p.clone()))) ),

            (Value::Pattern(p1), Value::Pattern(p2)) => Ok( Value::Pattern(Pattern::Either(Box::new(p1.clone()), Box::new(p2.clone()))) ),


            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "type and type, type and pattern or pattern and pattern".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }

    pub fn range(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Range(Range{
                start: Some(*n1),
                end: Some(*n2),
                step: 1.0,
            })),
            (Value::Range(Range{start, end: Some(e), step}), Value::Number(n)) if *step == 1.0 => Ok(Value::Range(Range{
                start: *start,
                end: Some(*n),
                step: *e,
            })),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number or range and number".to_string(),
                    found: format!("{} and {}", value1.type_str(globals), value2.type_str(globals)),
                    area,
                    defs: vec![(value1.type_str(globals), a.def_area.clone()), (value2.type_str(globals), b.def_area.clone())],
                } )
            }
        }
    }

    pub fn in_op(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (_, Value::Array(r)) => {
                for i in r {
                    let s = globals.get(*i).clone();
                    if eq(a, &s, globals) {
                        return Ok(Value::Boolean(true));
                    }
                }
                Ok(Value::Boolean(false))
            },

            (Value::String(s), Value::Dictionary(map)) => {
                Ok(Value::Boolean(map.contains_key(s)))
            },
            (Value::String(s1), Value::String(s2)) => {
                Ok(Value::Boolean( s2.contains(s1.as_str()) ))
            },

            (value, Value::Dictionary(_) | Value::String(_)) => Err( RuntimeError::TypeMismatch {
                expected: "string".to_string(),
                found: format!("{}", value.type_str(globals)),
                area: a.def_area.clone(),
                defs: vec![(value.type_str(globals), a.def_area.clone())],
            } ),


            (v1, v2) => Err( RuntimeError::TypeMismatch {
                expected: "_ and array, string and dictionary, or string and string".to_string(),
                found: format!("{} and {}", v1.type_str(globals), v2.type_str(globals)),
                area,
                defs: vec![(v1.type_str(globals), a.def_area.clone()), (v2.type_str(globals), b.def_area.clone())],
            } ),
        }
    }

}

impl Value {
    pub fn hash<H: std::hash::Hasher>(&self, state: &mut H, globals: Option<&Globals>) {
        match self {
            Value::Number(v) => v.to_bits().hash(state),
            Value::Boolean(v) => v.hash(state),
            Value::String(v) => v.hash(state),
            Value::Option(v) => match v {
                Some(v) => globals.unwrap().get(*v).value.hash(state, globals),
                None => "#".hash(state),
            },
            Value::Builtin(v) => v.hash(state),
            Value::Function(v) => v.hash(state),
            Value::Array(v) => {
                for i in v {
                    globals.unwrap().get(*i).value.hash(state, globals)
                }
            },
            Value::Tuple(v) => {
                for i in v {
                    globals.unwrap().get(*i).value.hash(state, globals)
                }
            },
            Value::Dictionary(v) => {
                for (k, v) in v {
                    k.hash(state);
                    globals.unwrap().get(*v).value.hash(state, globals)
                }
            },
            Value::Type(v) => v.hash(state),
            Value::Pattern(v) => v.hash(state, globals.unwrap()),
            Value::Range(v) => v.hash(state),
            Value::McFunc(v) => v.hash(state),
            Value::McVector(v) => v.hash(state, globals.unwrap()),
            Value::Selector(v) => {
                v.selector_type.hash(state);
                for (k, v) in &v.args {
                    k.hash(state);
                    globals.unwrap().get(*v).value.hash(state, globals)
                }
            },
            Value::StructInstance { struct_id, fields } => {
                struct_id.hash(state);
                for (k, v) in fields {
                    k.hash(state);
                    globals.unwrap().get(*v).value.hash(state, globals)
                }
            },
            Value::EnumInstance { enum_id, variant_name, variant } => {
                enum_id.hash(state);
                variant_name.hash(state);
                match variant {
                    InstanceVariant::Unit => core::mem::discriminant(variant).hash(state),
                    InstanceVariant::Tuple(v) => {
                        for i in v {
                            globals.unwrap().get(*i).value.hash(state, globals)
                        }
                    },
                    InstanceVariant::Struct { fields } => {
                        for (k, v) in fields {
                            k.hash(state);
                            globals.unwrap().get(*v).value.hash(state, globals)
                        }
                    },
                }
            },
        }
    }
}


