use std::collections::{HashMap, HashSet};

use crate::{parser::ASTNode, interpreter::{ScopePos, ValuePos, Globals, TypePos, CustomStruct, McFuncID, CustomEnum}, CodeArea, builtins::{BuiltinType, builtin_type_str}, error::RuntimeError};



#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub args: Vec<(String, CodeArea, Option<ValuePos>)>,
    pub code: Box<ASTNode>,
    pub parent_scope: ScopePos,
    pub header_area: CodeArea,
    pub self_arg: Option<Box<ASTNode>>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Builtin(BuiltinType),
    CustomStruct(TypePos),
    CustomEnum(TypePos),
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Type(ValueType),
    Any,
    Either(Box<Pattern>, Box<Pattern>),
}

impl Pattern {
    pub fn to_str(&self, globals: &Globals) -> String {
        match self {
            Pattern::Any => format!("any"),
            Pattern::Type(t) => t.to_str(globals),
            Pattern::Either(a, b) => format!("({} | {})", a.to_str(globals), b.to_str(globals)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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


#[derive(Debug, Clone, PartialEq)]
struct Range {
    start: isize,
    end: isize,
    step: isize,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Null,
    Builtin(String),
    Function(Function),
    Array(Vec<ValuePos>),
    Tuple(Vec<ValuePos>),
    Dictionary(HashMap<String, ValuePos>),
    Type(ValueType),
    Pattern(Pattern),
    Range(Range),

    McFunc(McFuncID),

    McVector(McVector),

    StructInstance { struct_id: TypePos, fields: HashMap<String, ValuePos> },
}

impl Value {

    pub fn typ(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Builtin(BuiltinType::Number),
            Value::Boolean(_) => ValueType::Builtin(BuiltinType::Bool),
            Value::String(_) => ValueType::Builtin(BuiltinType::String),
            Value::Null => ValueType::Builtin(BuiltinType::Nulltype),
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

            Value::StructInstance { struct_id, .. } => ValueType::CustomStruct(*struct_id),
        }
    }

    pub fn type_str(&self, globals: &Globals) -> String {
        self.typ().to_str(globals)
    }

    pub fn to_str<'a>(&'a self, globals: &'a Globals, visited: &mut Vec<&'a Value>) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::Null => "null".to_string(),
            Value::Builtin(name) => format!("<builtin: {}>", name),
            Value::Function( Function {args, ..} )=> format!("({}) => ...", args.iter().map(|(e, _, _)| e.clone()).collect::<Vec<String>>().join(", ")),
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
                    CustomStruct { name, .. } => format!("<struct: {}>", name)
                },
                ValueType::CustomEnum(pos) => match &globals.custom_enums.map[pos] {
                    CustomEnum { name, .. } => format!("<enum: {}>", name)
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
            Value::Pattern(p) => p.to_str(globals),
            Value::Range(Range { start, end, step }) => 
                format!("{}..{}", start, end) + &(if *step != 1 {format!("..{}", step)} else {"".to_string() }),
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
        }
    }

    pub fn matches_pat(&self, p: Pattern, globals: &Globals) -> Result<bool, RuntimeError> {
        match p {
            Pattern::Any => Ok( true ),
            Pattern::Type(t) => Ok( self.typ() == t.clone() ),
            Pattern::Either(a, b) => Ok( self.matches_pat(*a, globals)? || self.matches_pat(*b, globals)? )
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
                for (_, _, d) in args {
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
            Value::StructInstance { fields, .. } => {
                for (_, i) in fields {
                    values.insert(*i);
                    globals.get(*i).value.get_references(globals, values, scopes);
                }
            },
            _ => (),
        }
    }







}

pub mod value_ops {

    use crate::{interpreter::{StoredValue, Globals}, CodeArea, value::{Value, ValueType}, error::RuntimeError, builtins::BuiltinType};

    use super::{Pattern, Range};

    pub fn to_bool(a: &StoredValue, area: CodeArea, globals: &Globals) -> Result<bool, RuntimeError> {
        match &a.value {
            Value::Boolean(b) => Ok(*b),
            Value::Null => Ok(false),
            
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

    pub fn iter(a: &StoredValue, area: CodeArea, globals: &Globals) -> Result<Vec<Value>, RuntimeError> {
        match &a.value {
            Value::Array(arr) => Ok( arr.iter().map(|e| globals.get(*e).value.clone() ).collect::<Vec<Value>>() ),
            Value::Dictionary(map) => Ok( map.iter().map(|(s, _)| Value::String(s.clone()) ).collect::<Vec<Value>>() ),
            Value::Range(Range { start, end, step }) => Ok(
                {
                    let mut v = vec![];
                    let mut i = *start;
                    while i < *end {
                        v.push( Value::Number( i as f64 ) );
                        i += *step;
                    }
                    v
                }
            ),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "array, range, or dict".to_string(),
                    found: format!("{}", value.type_str(globals)),
                    area,
                    defs: vec![(value.type_str(globals), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn convert(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n), Value::Type(ValueType::Builtin(BuiltinType::String))) => Ok(Value::String(n.to_string())),
            (v, Value::Type(ValueType::Builtin(BuiltinType::Type))) => Ok(Value::Type(v.typ())),

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
        match (&a.value, &b.value) {
            (v, Value::Type(t)) => Ok( v.typ() == t.clone() ),

            (v, Value::Pattern(p)) => Ok( v.matches_pat(p.clone(), globals)? ),
            
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

    pub fn overwrite(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (_, b) => Ok( b.clone() )
        }
    }


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
            (Value::String(s), Value::Number(n)) => Ok(Value::String(s.repeat(
                if *n < 0.0 {0} else {*n as usize}
            ))),
            
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
            Value::Number(n1) => Ok(Value::Number(-n1)),
            
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

    pub fn eq(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<bool, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(n1 == n2),
            (Value::String(s1), Value::String(s2)) => Ok(s1 == s2),
            (Value::Boolean(b1), Value::Boolean(b2)) => Ok(b1 == b2),

            (_, _) => Ok(false),
        }
    }



    pub fn eq_op(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        Ok(Value::Boolean(eq(a, b, area, globals)?))
    }
    pub fn neq_op(a: &StoredValue, b: &StoredValue, area: CodeArea, globals: &mut Globals) -> Result<Value, RuntimeError> {
        Ok(Value::Boolean(!eq(a, b, area, globals)?))
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
                start: n1.floor() as isize,
                end: n2.floor() as isize,
                step: 1isize,
            })),
            
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

}


