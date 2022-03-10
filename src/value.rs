use std::collections::HashMap;

use crate::{parser::ASTNode, interpreter::{ScopePos, MemoryPos, Memory, TypePos, CustomStruct}, CodeArea, builtins::{BuiltinType, builtin_type_str}, error::RuntimeError};



#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub args: Vec<(String, CodeArea, Option<MemoryPos>)>,
    pub code: Box<ASTNode>,
    pub parent_scope: ScopePos,
    pub header_area: CodeArea,
}


#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Builtin(BuiltinType),
    CustomStruct(TypePos),
}

impl ValueType {
    pub fn to_str(&self, memory: &Memory) -> String {
        match self {
            ValueType::Builtin(b) => builtin_type_str(b.clone()),
            ValueType::CustomStruct(id) => match &memory.custom_structs[id] {
                CustomStruct { name, .. } => name.to_string()
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
    pub fn to_str(&self, memory: &Memory) -> String {
        match self {
            Pattern::Any => format!("any"),
            Pattern::Type(t) => t.to_str(memory),
            Pattern::Either(a, b) => format!("({} | {})", a.to_str(memory), b.to_str(memory)),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Null,
    Builtin(String),
    Function(Function),
    Array(Vec<MemoryPos>),
    Dictionary(HashMap<String, MemoryPos>),
    Type(ValueType),
    Pattern(Pattern),

    StructInstance { struct_id: TypePos, fields: HashMap<String, MemoryPos> },
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
            Value::Dictionary(_) => ValueType::Builtin(BuiltinType::Dict),
            Value::Type(_) => ValueType::Builtin(BuiltinType::Type),
            Value::Pattern(_) => ValueType::Builtin(BuiltinType::Pattern),

            Value::StructInstance { struct_id, .. } => ValueType::CustomStruct(*struct_id),
        }
    }

    pub fn type_str(&self, memory: &Memory) -> String {
        self.typ().to_str(memory)
    }

    pub fn to_str<'a>(&'a self, memory: &'a Memory, visited: &mut Vec<&'a Value>) -> String {
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
                    |i| memory.get(*i).value.to_str(memory, visited)
                ).collect::<Vec<String>>().join(", ") + "]";
                visited.pop();
                out_str
            },
            Value::Dictionary(map) => {
                if visited.contains(&self) {
                    return "{...}".to_string()
                }
                visited.push( self );
                let out_str = "{".to_string() + &map.into_iter().map(
                    |(k, v)| format!("{}: {}", k, memory.get(*v).value.to_str(memory, visited))
                ).collect::<Vec<String>>().join(", ") + "}";
                visited.pop();
                out_str
            },
            Value::Type(v) => match v {
                ValueType::Builtin(t) => format!("<type: {}>", builtin_type_str(t.clone())),
                ValueType::CustomStruct(pos) => match memory.custom_structs.get(pos).unwrap() {
                    CustomStruct { name, .. } => format!("<struct: {}>", name)
                },
            },
            Value::StructInstance { struct_id, fields } => {

                let name = match &memory.custom_structs[struct_id] {
                    CustomStruct { name, .. } => name,
                };

                if visited.contains(&self) {
                    return name.clone() + "::{...}"
                }
                visited.push( self );
                let out_str = "{".to_string() + &fields.into_iter().map(
                    |(k, v)| format!("{}: {}", k, memory.get(*v).value.to_str(memory, visited))
                ).collect::<Vec<String>>().join(", ") + "}";
                visited.pop();
                format!("{}::{}", name, out_str)
            },
            Value::Pattern(p) => p.to_str(memory)
        }
    }

    pub fn matches_pat(&self, p: Pattern, memory: &Memory) -> Result<bool, RuntimeError> {
        match p {
            Pattern::Any => Ok( true ),
            Pattern::Type(t) => Ok( self.typ() == t.clone() ),
            Pattern::Either(a, b) => Ok( self.matches_pat(*a, memory)? || self.matches_pat(*b, memory)? )
        }
    }


}

pub mod value_ops {

    use crate::{interpreter::{StoredValue, Memory}, CodeArea, value::{Value, ValueType}, error::RuntimeError, builtins::BuiltinType};

    use super::Pattern;

    pub fn to_bool(a: &StoredValue, area: CodeArea, memory: &Memory) -> Result<bool, RuntimeError> {
        match &a.value {
            Value::Boolean(b) => Ok(*b),
            Value::Null => Ok(false),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "bool".to_string(),
                    found: format!("{}", value.type_str(memory)),
                    area,
                    defs: vec![(value.type_str(memory), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn convert(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n), Value::Type(ValueType::Builtin(BuiltinType::String))) => Ok(Value::String(n.to_string())),

            (value, Value::Type(t)) => {
                if value.type_str(memory) == t.to_str(memory) {
                    Ok( value.clone() )
                } else {
                    Err( RuntimeError::CannotConvert {
                        type1: value.type_str(memory),
                        type2: t.to_str(memory),
                        area,
                        area1: a.def_area.clone(),
                        // area2: b.def_area.clone(),
                    } )
                }
            }
            
            (_, value) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "type".to_string(),
                    found: format!("{}", value.type_str(memory)),
                    area,
                    defs: vec![(value.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }

    pub fn is_op(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<bool, RuntimeError> {
        match (&a.value, &b.value) {
            (v, Value::Type(t)) => Ok( v.typ() == t.clone() ),

            (v, Value::Pattern(p)) => Ok( v.matches_pat(p.clone(), memory)? ),
            
            (_, value) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "type or pattern".to_string(),
                    found: format!("{}", value.type_str(memory)),
                    area,
                    defs: vec![(value.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }



    pub fn plus(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(s1.to_string() + s2)),
            (Value::Array(a1), Value::Array(a2)) => {
                let mut new_vec = vec![];
                for i in a1 {
                    new_vec.push( memory.clone_id(*i, Some(area.clone())) );
                }
                for i in a2 {
                    new_vec.push( memory.clone_id(*i, Some(area.clone())) );
                }
                Ok(Value::Array(new_vec))
            },
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number or string and string".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn minus(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn mult(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
            (Value::String(s), Value::Number(n)) => Ok(Value::String(s.repeat(
                if *n < 0.0 {0} else {*n as usize}
            ))),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number or string and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn div(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn modulo(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 % n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn pow(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1.powf(*n2))),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }

    pub fn identity(a: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Number(n1) => Ok(Value::Number(*n1)),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: format!("{}", value.type_str(memory)),
                    area,
                    defs: vec![(value.type_str(memory), a.def_area.clone())],
                } )
            }
        }
    }
    pub fn negate(a: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Number(n1) => Ok(Value::Number(-n1)),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: format!("{}", value.type_str(memory)),
                    area,
                    defs: vec![(value.type_str(memory), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn eq(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 == n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::Boolean(s1 == s2)),
            (Value::Boolean(b1), Value::Boolean(b2)) => Ok(Value::Boolean(b1 == b2)),

            (_, _) => Ok(Value::Boolean(false)),
        }
    }
    pub fn neq(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 != n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::Boolean(s1 != s2)),
            (Value::Boolean(b1), Value::Boolean(b2)) => Ok(Value::Boolean(b1 != b2)),

            (_, _) => Ok(Value::Boolean(true)),
        }
    }
    pub fn greater(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 > n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn lesser(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 < n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn greater_eq(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 >= n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn lesser_eq(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 <= n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }

    
    pub fn either(a: &StoredValue, b: &StoredValue, area: CodeArea, memory: &mut Memory) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {

            (Value::Type(t1), Value::Type(t2)) => Ok( Value::Pattern(Pattern::Either(Box::new(Pattern::Type(t1.clone())), Box::new(Pattern::Type(t2.clone())))) ),

            (Value::Type(t), Value::Pattern(p)) |
            (Value::Pattern(p), Value::Type(t)) => Ok( Value::Pattern(Pattern::Either(Box::new(Pattern::Type(t.clone())), Box::new(p.clone()))) ),

            (Value::Pattern(p1), Value::Pattern(p2)) => Ok( Value::Pattern(Pattern::Either(Box::new(p1.clone()), Box::new(p2.clone()))) ),


            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "type and type, type and pattern or pattern and pattern".to_string(),
                    found: format!("{} and {}", value1.type_str(memory), value2.type_str(memory)),
                    area,
                    defs: vec![(value1.type_str(memory), a.def_area.clone()), (value2.type_str(memory), b.def_area.clone())],
                } )
            }
        }
    }

}


