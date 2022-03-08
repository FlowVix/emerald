use std::collections::HashMap;

use crate::{value::{Value, value_ops}, parser::{ASTNode, NodeType}, EmeraldSource, error::RuntimeError, lexer::Token, CodeArea};



// enum Exit {
//     Return(Value, (usize, usize)),
//     Break(Value, (usize, usize)),
// }

pub struct RunInfo {
    pub source: EmeraldSource,
    // exits: Vec<Exit>,
}

pub type MemoryPos = usize;
pub type ScopePos = usize;

#[derive(Debug)]
pub struct StoredValue {
    pub value: Value,
    pub mutable: bool,
    pub def_area: CodeArea,
}

pub struct Memory {
    counter: MemoryPos,
    pub register: HashMap<MemoryPos, StoredValue>,
    // protected: Vec<Vec<MemoryPos>>,
    // custom_types: Vec<String>,
    // impls: HashMap<String, HashMap<String, MemoryPos>>,
}




struct Scope {
    vars: HashMap<String, MemoryPos>,
    parent_id: Option<ScopePos>,
}

pub struct ScopeList {
    counter: ScopePos,
    register: HashMap<ScopePos, Scope>,
}


impl Memory {

    pub fn new() -> Self {
        Self {
            counter: 0,
            register: HashMap::new(),
            // protected: vec![],
            // custom_types: vec![],
            // impls: HashMap::new()
        }
    }

    // fn push_protected(&mut self) {
    //     self.protected.push( vec![] );
    // }
    // fn pop_protected(&mut self) {
    //     self.protected.pop();
    // }
    pub fn insert(
        &mut self,
        value: Value,
        mutable: bool,
        def_area: CodeArea,
    ) -> MemoryPos {
        self.counter += 1;
        self.register.insert( self.counter, StoredValue {
            value,
            mutable,
            def_area,
        } );
        self.counter
    }
    pub fn set(
        &mut self,
        id: MemoryPos,
        value: Value,
        mutable: Option<bool>,
        def_area: Option<CodeArea>,
    ) {
        let (v_m, v_a) = {
            let val = self.get(id);
            (val.mutable, val.def_area.clone())
        };
        self.register.insert( id, StoredValue {
            value,
            mutable: if let Some(m) = mutable {m} else {v_m},
            def_area: if let Some(m) = def_area {m} else {v_a},
        } );
    }
    pub fn redef(
        &mut self,
        id: MemoryPos,
        def_area: CodeArea,
    ) {
        self.register.get_mut(&id).unwrap().def_area = def_area;
    }
    pub fn clone_id(
        &mut self,
        id: MemoryPos,
        mutable: Option<bool>,
        def_area: Option<CodeArea>,
    ) -> MemoryPos {
        let (v_v, v_m, v_a) = {
            let val = self.get(id);
            (val.value.clone(), val.mutable, val.def_area.clone())
        };
        self.insert(
            v_v,
            if let Some(m) = mutable {m} else {v_m},
            if let Some(a) = def_area {a} else {v_a},
        )
    }
    pub fn remut(
        &mut self,
        id: MemoryPos,
        mutable: bool,
    ) {
        self.register.get_mut(&id).unwrap().mutable = mutable;
    }
    pub fn get(&self, id: MemoryPos) -> &StoredValue {
        return match self.register.get(&id) {
            Some(v) => v,
            None => panic!("bad value fuck you"),
        }
    }
    // pub fn protect(&mut self, value: Value) -> MemoryPos {
    //     let pos = self.insert( value );
    //     self.protected
    //         .last_mut()
    //         .unwrap()
    //         .push(pos);
    //     pos
    // }
}


impl ScopeList {

    pub fn new() -> Self {
        let mut new = Self {
            counter: 0,
            register: HashMap::new(),
        };
        new.register.insert(0, Scope { vars: HashMap::new(), parent_id: None });
        new
    }

    fn get(&self, scope_id: ScopePos) -> &Scope {
        self.register.get(&scope_id).unwrap()
    }
    fn get_mut(&mut self, scope_id: ScopePos) -> &mut Scope {
        self.register.get_mut(&scope_id).unwrap()
    }
    fn derive(&mut self, scope_id: ScopePos) -> ScopePos {
        self.counter += 1;
        self.register.insert(self.counter, Scope {
            vars: HashMap::new(),
            parent_id: Some(scope_id),
        });
        self.counter
    }
    pub fn set_var(
        &mut self,
        scope_id: ScopePos,
        name: String,
        val_id: MemoryPos,
    ) {
        self.get_mut(scope_id).vars.insert(name, val_id);
    }
}

macro_rules! interpreter_util {
    ($memory:expr, $scopes:expr, $info:expr) => {
        macro_rules! execute {
            ($node:expr => $scope_id:expr) => {
                execute($node, $scope_id, $memory, $scopes, $info)?
            }
        }
    };
}

macro_rules! area {
    ($source:expr, $area:expr) => {
        CodeArea {source: $source, range: $area}
    };
}


pub fn execute(
    node: &ASTNode,
    scope_id: ScopePos,
    memory: &mut Memory,
    scopes: &mut ScopeList,
    info: &mut RunInfo
) -> Result<MemoryPos, RuntimeError> {
    interpreter_util!(memory, scopes, info);

    let start_node = node;
    
    match &node.node {
        NodeType::Value { value } => Ok(memory.insert(
            value.clone(),
            false,
            CodeArea {source: info.source.clone(), range: node.span},
        )),
        NodeType::Op { left, op, right } => {
            match op {
                Token::Plus | Token::Minus | Token::Mult | Token::Div | Token::Mod | Token::Pow | Token::Eq | Token::NotEq | Token::Greater | Token::GreaterEq | Token::Lesser | Token::LesserEq => {
                    let left = execute!(left => scope_id);
                    let right = execute!(right => scope_id);
                    match op {
                        Token::Plus => Ok(memory.insert(
                            value_ops::plus(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::Minus => Ok(memory.insert(
                            value_ops::minus(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::Mult => Ok(memory.insert(
                            value_ops::mult(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::Div => Ok(memory.insert(
                            value_ops::div(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::Mod => Ok(memory.insert(
                            value_ops::modulo(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::Pow => Ok(memory.insert(
                            value_ops::pow(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::Eq => Ok(memory.insert(
                            value_ops::eq(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::NotEq => Ok(memory.insert(
                            value_ops::neq(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::Greater => Ok(memory.insert(
                            value_ops::greater(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::GreaterEq => Ok(memory.insert(
                            value_ops::greater_eq(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::Lesser => Ok(memory.insert(
                            value_ops::lesser(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        Token::LesserEq => Ok(memory.insert(
                            value_ops::lesser_eq(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                            false,
                            area!(info.source.clone(), node.span),
                        )),
                        _ => unreachable!()
                    }
                }
                Token::Assign | Token::PlusEq | Token::MinusEq | Token::MultEq | Token::DivEq | Token::ModEq | Token::PowEq => {
                    let left = execute!(left => scope_id);
                    if !memory.get(left).mutable {
                        return Err(RuntimeError::ModifyImmutable {
                            def_area: memory.get(left).def_area.clone(),
                            modify_area: area!(info.source.clone(), start_node.span),
                        })
                    }

                    let mut right = execute!(right => scope_id);
                    match op {
                        Token::PlusEq => {
                            right = memory.insert(
                                value_ops::plus(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                                false,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::MinusEq => {
                            right = memory.insert(
                                value_ops::minus(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                                false,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::MultEq => {
                            right = memory.insert(
                                value_ops::mult(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                                false,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::DivEq => {
                            right = memory.insert(
                                value_ops::div(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                                false,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::ModEq => {
                            right = memory.insert(
                                value_ops::modulo(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                                false,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::PowEq => {
                            right = memory.insert(
                                value_ops::pow(memory.get(left), memory.get(right), area!(info.source.clone(), node.span))?,
                                false,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        _ => (),
                    }
                    memory.set(
                        left,
                        memory.get(right).value.clone(),
                        None,
                        Some(area!(info.source.clone(), start_node.span))
                    );
                    Ok(right)
                }
                _ => unreachable!()
            }
        },
        NodeType::Unary { op, node } => {
            let value = execute!(node => scope_id);
            match op {
                Token::Plus => Ok(memory.insert(
                    value_ops::identity(memory.get(value), area!(info.source.clone(), node.span))?,
                    false,
                    area!(info.source.clone(), node.span),
                )),
                Token::Minus => Ok(memory.insert(
                    value_ops::negate(memory.get(value), area!(info.source.clone(), node.span))?,
                    false,
                    area!(info.source.clone(), node.span),
                )),
                _ => unreachable!()
            }
        },
        NodeType::Declaration { var_name, mutable, value } => {
            let initial_id = execute!(value => scope_id);
            let value_id = memory.clone_id(
                initial_id,
                Some(*mutable),
                Some(area!(info.source.clone(), start_node.span))
            );
            scopes.set_var(scope_id, var_name.to_string(), value_id);
            Ok(initial_id)
        }
        NodeType::Var { var_name } => {
            let mut current_scope = scope_id;
            loop {
                match scopes.get(current_scope).vars.get(var_name) {
                    Some(id) => return Ok(*id),
                    None => match scopes.get(current_scope).parent_id {
                        Some(p_id) => current_scope = p_id,
                        None => break,
                    },
                }
            }
            Err(
                RuntimeError::UndefinedVar {
                    var_name: var_name.to_string(),
                    area: area!(info.source.clone(), start_node.span),
                }
            )
            
        }
        NodeType::StatementList { statements } => {
            let mut ret_id = memory.insert(
                Value::Null,
                false,
                area!(info.source.clone(), start_node.span)
            );
            for i in statements {
                ret_id = execute!(i => scope_id);
            }
            Ok(ret_id)
        }
        NodeType::Block { code, not_safe } => {
            let derived = scopes.derive(scope_id);
            let value = execute!(code => derived);
            memory.redef(value, area!(info.source.clone(), start_node.span));
            Ok( value )
        }
        NodeType::If { cond, code, else_branch } => {
            let cond_value = execute!(cond => scope_id);
            let mut ret_id = memory.insert(
                Value::Null,
                false,
                area!(info.source.clone(), start_node.span)
            );
            if value_ops::to_bool(memory.get(cond_value), area!(info.source.clone(), cond.span))? {
                ret_id = execute!(code => scope_id)
            } else if let Some(b) = else_branch {
                ret_id = execute!(b => scope_id)
            }
            memory.redef(ret_id, area!(info.source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::While { cond, code } => {
            let mut ret_id = memory.insert(
                Value::Null,
                false,
                area!(info.source.clone(), start_node.span)
            );
            loop {
                let cond_value = execute!(cond => scope_id);
                if value_ops::to_bool(memory.get(cond_value), area!(info.source.clone(), cond.span))? {
                    ret_id = execute!(code => scope_id)
                } else {
                    break;
                }
            }
            memory.redef(ret_id, area!(info.source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::Loop { code } => {
            let mut ret_id = memory.insert(
                Value::Null,
                false,
                area!(info.source.clone(), start_node.span)
            );
            loop {
                ret_id = execute!(code => scope_id)
            }
            memory.redef(ret_id, area!(info.source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::FuncDef { func_name, arg_names, code, arg_areas, header_area } => {
            let value_id = memory.insert(
                Value::Function {
                    arg_names: arg_names.clone(),
                    code: code.clone(),
                    parent_scope: scope_id,
                    header_area: header_area.clone(),
                    arg_areas: arg_areas.clone()
                },
                false,
                area!(info.source.clone(), start_node.span)
            );
            scopes.set_var(scope_id, func_name.to_string(), value_id);
            Ok( memory.clone_id(
                value_id,
                None,
                None,
            ) )
        }
        NodeType::Lambda { arg_names, code, arg_areas, header_area } => {
            Ok( memory.insert(
                Value::Function {
                    arg_names: arg_names.clone(),
                    code: code.clone(),
                    parent_scope: scope_id,
                    header_area: header_area.clone(),
                    arg_areas: arg_areas.clone()
                },
                false,
                area!(info.source.clone(), start_node.span)
            ) )
        }
        NodeType::Call { base, args } => {
            let base_id = execute!(base => scope_id);
            
            match &memory.get(base_id).value.clone() {
                Value::Builtin(name) => {
                    match &name[..] {
                        "print" => {
                            let mut out_str = String::new();
                            for i in args {
                                let arg_id = execute!(i => scope_id);
                                out_str += &memory.get(arg_id).value.to_str();
                            }
                            println!("{}", out_str);
                            Ok (memory.insert(
                                Value::Null,
                                false,
                                area!(info.source.clone(), start_node.span)
                            ) )
                        }
                        _ => unreachable!(),
                    }
                }
                Value::Function { arg_names, code, parent_scope, header_area, arg_areas } => {
                    if arg_names.len() != args.len() {
                        return Err(
                            RuntimeError::IncorrectArgumentCount {
                                provided: args.len(),
                                takes: arg_names.len(),
                                header_area: header_area.clone(),
                                call_area: area!(info.source.clone(), start_node.span)
                            }
                        )
                    }

                    let derived = scopes.derive(*parent_scope);
                    for ((arg, name), area) in args.iter().zip(arg_names).zip(arg_areas) {
                        let arg_id = execute!(arg => scope_id);
                        let arg_id = memory.clone_id(arg_id, Some(false), Some(area.clone()));
                        scopes.set_var(derived, name.clone(), arg_id);
                    }
                    let ret_id = execute!(code => derived);
                    Ok( memory.clone_id(ret_id, Some(false), Some(area!(info.source.clone(), start_node.span))) )
                }
                other => return Err( RuntimeError::TypeMismatch {
                    expected: "function or builtin".to_string(),
                    found: format!("{}", other.type_str()),
                    area: area!(info.source.clone(), base.span),
                    defs: vec![(other.type_str(), memory.get(base_id).def_area.clone())],
                } )
            }


        }
    }

}


