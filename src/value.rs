use crate::{parser::ASTNode, interpreter::ScopePos, CodeArea};



#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Null,
    Builtin(String),
    Function { arg_names: Vec<String>, code: Box<ASTNode>, parent_scope: ScopePos, arg_areas: Vec<CodeArea>, header_area: CodeArea }
}

impl Value {

    pub fn type_str(&self) -> String {
        match self {
            Value::Number(_) => "number".to_string(),
            Value::Boolean(_) => "bool".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Null => "null".to_string(),
            Value::Builtin(_) => "builtin".to_string(),
            Value::Function{..} => "function".to_string(),
        }
    }

    pub fn to_str(&self) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => s.clone(),
            Value::Null => "null".to_string(),
            Value::Builtin(name) => format!("<builtin: {}>", name),
            Value::Function {arg_names, ..} => format!("({}) => ...", arg_names.join(", ")),
        }
    }


}

pub mod value_ops {

    use crate::{interpreter::StoredValue, CodeArea, value::Value, error::RuntimeError};

    pub fn to_bool(a: &StoredValue, area: CodeArea) -> Result<bool, RuntimeError> {
        match &a.value {
            Value::Boolean(b) => Ok(*b),
            Value::Null => Ok(false),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "bool".to_string(),
                    found: format!("{}", value.type_str()),
                    area,
                    defs: vec![(value.type_str(), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn plus(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(s1.to_string() + s2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number or string and string".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn minus(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn mult(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
            (Value::String(s), Value::Number(n)) => Ok(Value::String(s.repeat(
                if *n < 0.0 {0} else {*n as usize}
            ))),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number or string and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn div(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn modulo(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 % n2)),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn pow(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1.powf(*n2))),
            
            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }

    pub fn identity(a: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Number(n1) => Ok(Value::Number(*n1)),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: format!("{}", value.type_str()),
                    area,
                    defs: vec![(value.type_str(), a.def_area.clone())],
                } )
            }
        }
    }
    pub fn negate(a: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Number(n1) => Ok(Value::Number(-n1)),
            
            value => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: format!("{}", value.type_str()),
                    area,
                    defs: vec![(value.type_str(), a.def_area.clone())],
                } )
            }
        }
    }

    pub fn eq(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 == n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::Boolean(s1 == s2)),
            (Value::Boolean(b1), Value::Boolean(b2)) => Ok(Value::Boolean(b1 == b2)),

            (_, _) => Ok(Value::Boolean(false)),
        }
    }
    pub fn neq(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 != n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::Boolean(s1 != s2)),
            (Value::Boolean(b1), Value::Boolean(b2)) => Ok(Value::Boolean(b1 != b2)),

            (_, _) => Ok(Value::Boolean(true)),
        }
    }
    pub fn greater(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 > n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn lesser(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 < n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn greater_eq(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 >= n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }
    pub fn lesser_eq(a: &StoredValue, b: &StoredValue, area: CodeArea) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Boolean(n1 <= n2)),

            (value1, value2) => {
                Err( RuntimeError::TypeMismatch {
                    expected: "number and number".to_string(),
                    found: format!("{} and {}", value1.type_str(), value2.type_str()),
                    area,
                    defs: vec![(value1.type_str(), a.def_area.clone()), (value2.type_str(), b.def_area.clone())],
                } )
            }
        }
    }

}


