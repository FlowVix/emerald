

use std::collections::{HashMap, HashSet};

use crate::{value::{Value, value_ops, Function, ValueType}, parser::{ASTNode, NodeType}, EmeraldSource, error::RuntimeError, lexer::Token, CodeArea, builtins::{Builtin, BuiltinType}};
use crate::builtins::{run_builtin, name_to_builtin};

#[derive(Debug)]
pub enum Exit {
    Return(MemoryPos, (usize, usize)),
    Break(MemoryPos, (usize, usize)),
}

#[derive(Debug)]
pub struct RunInfo {
    pub source: EmeraldSource,
    pub exits: Vec<Exit>,
    pub trace: Vec<CodeArea>,
}

pub type MemoryPos = usize;
pub type ScopePos = usize;
pub type TypePos = usize;

#[derive(Debug, Clone)]
pub struct StoredValue {
    pub value: Value,
    pub def_area: CodeArea,
}

#[derive(Debug, Clone)]
pub struct CustomStruct {
    pub name: String,
    pub fields: HashMap<String, (MemoryPos, Option<MemoryPos>)>,
    pub field_areas: HashMap<String, CodeArea>,
    pub def_area: CodeArea,
}


#[derive(Debug, Clone)]
pub struct ImplData {
    pub members: HashMap<String, MemoryPos>,
    pub methods: Vec<String>,
}

impl ImplData {
    pub fn new() -> Self {
        ImplData {
            members: HashMap::new(),
            methods: Vec::new(),
        }
    }
}



#[derive(Debug)]
pub struct Collector {
    pub marked_values: HashSet<MemoryPos>,
    pub marked_scopes: HashSet<ScopePos>,
}

impl Collector {
    pub fn new(memory: &Memory) -> Self {
        let mut marked_scopes = HashSet::new();
        let mut marked_values = HashSet::new();
        for (i, _) in &memory.values.map {
            marked_values.insert(*i);
        }
        for (i, _) in &memory.scopes.map {
            marked_scopes.insert(*i);
        }
        Self {
            marked_values,
            marked_scopes,
        }
    }
}


#[derive(Debug, Clone)]
pub struct Protector {
    pub values: Vec<MemoryPos>,
    pub scopes: Vec<ScopePos>,
}

impl Protector {
    pub fn new() -> Self {
        Protector {
            values: vec![],
            scopes: vec![],
        }
    }
}



pub struct Register<K, V> {
    pub counter: K,
    pub map: HashMap<K, V>,
}



pub struct Memory {
    pub values: Register<MemoryPos, StoredValue>,
    pub scopes: Register<ScopePos, Scope>,
    pub custom_structs: Register<TypePos, CustomStruct>,
    pub last_amount: usize,
    
    pub protected: Vec<Protector>,

    pub impls: HashMap<TypePos, ImplData>,
    pub builtin_impls: HashMap<BuiltinType, ImplData>,

}




pub struct Scope {
    vars: HashMap<String, MemoryPos>,
    parent_id: Option<ScopePos>,
}



impl Memory {

    pub fn new() -> Self {
        let mut scopes = Register {
            map: HashMap::new(),
            counter: 0,
        };
        scopes.map.insert(0, Scope { vars: HashMap::new(), parent_id: None });
        Self {
            values: Register {
                map: HashMap::new(),
                counter: 0,
            },
            scopes,
            custom_structs: Register {
                map: HashMap::new(),
                counter: 0,
            },
            last_amount: 0,
            protected: vec![],
            // custom_types: vec![],
            impls: HashMap::new(),
            builtin_impls: HashMap::new()
        }
    }

    fn push_protected(&mut self) {
        self.protected.push( Protector::new() );
    }
    fn pop_protected(&mut self) {
        self.protected.pop();
    }


    pub fn new_struct(
        &mut self,
        typ: CustomStruct,
    ) -> MemoryPos {
        self.custom_structs.counter += 1;
        self.custom_structs.map.insert( self.custom_structs.counter, typ );
        self.custom_structs.counter
    }



    pub fn insert(
        &mut self,
        value: Value,
        def_area: CodeArea,
    ) -> MemoryPos {
        self.values.counter += 1;
        self.values.map.insert( self.values.counter, StoredValue {
            value,
            def_area,
        } );
        self.values.counter
    }
    pub fn set(
        &mut self,
        id: MemoryPos,
        value: Value,
        def_area: Option<CodeArea>,
    ) {
        let v_a = {
            let val = self.get(id);
            val.def_area.clone()
        };
        self.values.map.insert( id, StoredValue {
            value,
            def_area: if let Some(m) = def_area {m} else {v_a},
        } );
    }
    pub fn redef(
        &mut self,
        id: MemoryPos,
        def_area: CodeArea,
    ) {
        self.values.map.get_mut(&id).unwrap().def_area = def_area;
    }
    pub fn clone_id(
        &mut self,
        id: MemoryPos,
        def_area: Option<CodeArea>,
    ) -> MemoryPos {
        let (v_v, v_a) = {
            let val = self.get(id);
            (val.value.clone(), val.def_area.clone())
        };
        self.insert(
            v_v,
            if let Some(a) = def_area {a} else {v_a},
        )
    }
    pub fn get(&self, id: MemoryPos) -> &StoredValue {
        return match self.values.map.get(&id) {
            Some(v) => v,
            None => panic!("bad value fuck you"),
        }
    }

    // pub fn propagate_def(
    //     &mut self,
    //     id: MemoryPos,
    // ) {
    //     let def = self.get(id).def_area.clone();
    //     match self.get(id).value.clone() {
    //         Value::Array(arr) => {
    //             for i in arr {
    //                 self.redef(i, def.clone());
    //                 self.propagate_def(i);
    //             }
    //         }
    //         _ => (),
    //     }
    // }
    pub fn protect_memory(&mut self, pos: MemoryPos) {
        self.protected
            .last_mut()
            .unwrap()
            .values
            .push(pos);
    }
    pub fn protect_scope(&mut self, pos: ScopePos) {
        self.protected
            .last_mut()
            .unwrap()
            .scopes
            .push(pos);
    }


    fn get_scope(&self, scope_id: ScopePos) -> &Scope {
        self.scopes.map.get(&scope_id).unwrap()
    }
    fn get_scope_mut(&mut self, scope_id: ScopePos) -> &mut Scope {
        self.scopes.map.get_mut(&scope_id).unwrap()
    }

    fn derive_scope(&mut self, scope_id: ScopePos) -> ScopePos {
        self.scopes.counter += 1;
        self.scopes.map.insert(self.scopes.counter, Scope {
            vars: HashMap::new(),
            parent_id: Some(scope_id),
        });
        self.protect_scope(self.scopes.counter);
        self.scopes.counter
    }

    pub fn set_var(
        &mut self,
        scope_id: ScopePos,
        name: String,
        val_id: MemoryPos,
    ) {
        self.get_scope_mut(scope_id).vars.insert(name, val_id);
    }

    pub fn get_var(
        &self,
        scope_id: ScopePos,
        name: &String,
    ) -> Option<MemoryPos> {
        let mut current_scope = scope_id;
        loop {
            match self.get_scope(current_scope).vars.get(name) {
                Some(id) => return Some(*id),
                None => match self.get_scope(current_scope).parent_id {
                    Some(p_id) => current_scope = p_id,
                    None => return None,
                },
            }
        }
    }


    pub fn collect(&mut self, scope_id: ScopePos) {
        let mut collector = Collector::new(self);

        self.mark_scope(scope_id, &mut collector);

        for prot in &self.protected {
            for id in &prot.values {
                if collector.marked_values.remove(id) {
                    let mut value_ids: HashSet<MemoryPos> = HashSet::new();
                    let mut scope_ids: HashSet<ScopePos> = HashSet::new();
                    self.get(*id).value.get_references(self, &mut value_ids, &mut scope_ids);
                    for i in scope_ids {
                        if collector.marked_scopes.contains(&i) {
                            self.mark_scope(i, &mut collector);
                        }
                    }
                    for i in value_ids {
                        collector.marked_values.remove(&i);
                    }
                }
            }
            for id in &prot.scopes {
                if collector.marked_scopes.contains(id) {
                    self.mark_scope(*id, &mut collector);
                }
            }
        }

        for i in collector.marked_scopes {
            self.scopes.map.remove(&i);
        }
        for i in collector.marked_values {
            self.values.map.remove(&i);
        }

        self.last_amount = self.values.map.len();

    }

    pub fn mark_scope(&self, scope_id: ScopePos, collector: &mut Collector) {
        let mut var_checks = Vec::new();
        collector.marked_scopes.remove(&scope_id);
        for (_, id) in &self.get_scope(scope_id).vars {
            var_checks.push(*id);
        }
        for id in var_checks {
            if collector.marked_values.remove(&id) {
                let mut value_ids: HashSet<MemoryPos> = HashSet::new();
                let mut scope_ids: HashSet<ScopePos> = HashSet::new();
                self.get(id).value.get_references(self, &mut value_ids, &mut scope_ids);
                for i in scope_ids {
                    if collector.marked_scopes.contains(&i) {
                        self.mark_scope(i, collector);
                    }
                }
                for i in value_ids {
                    collector.marked_values.remove(&i);
                }
            }
        }
        match self.get_scope(scope_id).parent_id {
            Some(id) => if collector.marked_scopes.contains(&id) { self.mark_scope(id, collector) },
            None => (),
        }
    }


}




macro_rules! interpreter_util {
    ($memory:expr, $info:expr) => {
        macro_rules! execute {
            ($node:expr => $scope_id:expr) => {
                {
                    let id = execute($node, $scope_id, $memory, $info)?;
                    $memory.clone_id(
                        id,
                        None,
                    )
                }
            }
        }
        macro_rules! execute_raw {
            ($node:expr => $scope_id:expr) => {
                {
                    execute($node, $scope_id, $memory, $info)?
                }
            }
        }
        macro_rules! protecute {
            ($node:expr => $scope_id:expr) => {
                {
                    let id = execute($node, $scope_id, $memory, $info)?;
                    let out = $memory.clone_id(
                        id,
                        None,
                    );
                    $memory.protect_memory(id);
                    $memory.protect_memory(out);
                    out
                }
            }
        }
        macro_rules! protecute_raw {
            ($node:expr => $scope_id:expr) => {
                {
                    let id = execute($node, $scope_id, $memory, $info)?;
                    $memory.protect_memory(id);
                    id
                }
            }
        }
        macro_rules! clone {
            ($id:expr => @ ) => {
                {
                    let new_id = $memory.clone_id(
                        $id,
                        None,
                    );
                    new_id
                }
            };
            ($id:expr => $area:expr ) => {
                {
                    let new_id = $memory.clone_id(
                        $id,
                        Some($area),
                    );
                    new_id
                }
            };
        }
        macro_rules! proteclone {
            ($id:expr => @ ) => {
                {
                    let new_id = $memory.clone_id(
                        $id,
                        None,
                    );
                    $memory.protect_memory(new_id);
                    new_id
                }
            };
            ($id:expr => $area:expr ) => {
                {
                    let new_id = $memory.clone_id(
                        $id,
                        Some($area),
                    );
                    $memory.protect_memory(new_id);
                    new_id
                }
            };
        }
        macro_rules! ret {
            ($thing:expr) => {
                {
                    $memory.pop_protected();
                    return $thing;
                }
            };
        }
    };
}

macro_rules! area {
    ($source:expr, $area:expr) => {
        CodeArea {source: $source, range: $area}
    };
    ($source:expr, $start:expr, $end:expr) => {
        CodeArea {source: $source, range: ($start, $end)}
    };
}







pub fn execute(
    node: &ASTNode,
    scope_id: ScopePos,
    memory: &mut Memory,
    info: &mut RunInfo
) -> Result<MemoryPos, RuntimeError> {
    interpreter_util!(memory, info);

    let start_node = node;

    if memory.values.map.len() > 50000 + memory.last_amount {
        // println!("{}", memory.values.map.len());
        memory.collect(scope_id);
        // println!("{} {}\n", memory.values.map.len(), memory.last_amount);
    }

    memory.push_protected();
    
    let exec_result = match &node.node {
        NodeType::Value { value } => Ok(memory.insert(
            value.clone(),
            CodeArea {source: info.source.clone(), range: node.span},
        )),
        NodeType::Op { left, op, right } => {
            match op {
                Token::Plus | Token::Minus | Token::Mult | Token::Div | Token::Mod | Token::Pow | Token::Eq | Token::NotEq | Token::Greater | Token::GreaterEq | Token::Lesser | Token::LesserEq | Token::As | Token::Is | Token::Pipe => {
                    let left = protecute!(left => scope_id);
                    let right = protecute!(right => scope_id);
                    match op {
                        Token::Plus => {
                            let result = value_ops::plus(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Minus => {
                            let result = value_ops::minus(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Mult => {
                            let result = value_ops::mult(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Div => {
                            let result = value_ops::div(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Mod => {
                            let result = value_ops::modulo(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Pow => {
                            let result = value_ops::pow(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Eq => {
                            let result = value_ops::eq(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::NotEq => {
                            let result = value_ops::neq(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Greater => {
                            let result = value_ops::greater(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::GreaterEq => {
                            let result = value_ops::greater_eq(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Lesser => {
                            let result = value_ops::lesser(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::LesserEq => {
                            let result = value_ops::lesser_eq(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::As => {
                            let result = value_ops::convert(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Is => {
                            let result = value_ops::is_op(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                Value::Boolean(result),
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        Token::Pipe => {
                            let result = value_ops::either(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            Ok(memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            ))
                        },
                        _ => unreachable!()
                    }
                }
                Token::Assign | Token::PlusEq | Token::MinusEq | Token::MultEq | Token::DivEq | Token::ModEq | Token::PowEq => {
                    let left = execute_raw!(left => scope_id);
                    

                    let mut right = protecute!(right => scope_id);
                    match op {
                        Token::PlusEq => {
                            let result = value_ops::plus(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            right = memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::MinusEq => {
                            let result = value_ops::minus(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            right = memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::MultEq => {
                            let result = value_ops::mult(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            right = memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::DivEq => {
                            let result = value_ops::div(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            right = memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::ModEq => {
                            let result = value_ops::modulo(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            right = memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        Token::PowEq => {
                            let result = value_ops::pow(&memory.get(left).clone(), &memory.get(right).clone(), area!(info.source.clone(), node.span), memory)?;
                            right = memory.insert(
                                result,
                                area!(info.source.clone(), node.span),
                            )
                        },
                        _ => (),
                    }
                    memory.set(
                        left,
                        memory.get(right).value.clone(),
                        Some(area!(info.source.clone(), start_node.span))
                    );
                    Ok(right)
                }
                _ => unreachable!()
            }
        },
        NodeType::Unary { op, node } => {
            let value = protecute!(node => scope_id);
            match op {
                Token::Plus => {
                    let result = value_ops::identity(&memory.get(value).clone(), area!(info.source.clone(), node.span), memory)?;
                    Ok(memory.insert(
                        result,
                        area!(info.source.clone(), node.span),
                    ))
                },
                Token::Minus => {
                    let result = value_ops::negate(&memory.get(value).clone(), area!(info.source.clone(), node.span), memory)?;
                    Ok(memory.insert(
                        result,
                        area!(info.source.clone(), node.span),
                    ))
                },
                _ => unreachable!()
            }
        },
        NodeType::Declaration { var_name, value } => {
            let initial_id = execute!(value => scope_id);
            // let value_id = clone!(initial_id => area!(info.source.clone(), start_node.span));
            memory.redef(initial_id, area!(info.source.clone(), start_node.span));
            // memory.propagate_def(value_id);

            memory.set_var(scope_id, var_name.to_string(), initial_id);
            Ok(initial_id)
        }
        NodeType::Var { var_name } => {
            match memory.get_var(scope_id, var_name) {
                Some(i) => Ok( i ),
                None => Err(
                    RuntimeError::UndefinedVar {
                        var_name: var_name.to_string(),
                        area: area!(info.source.clone(), start_node.span),
                    }
                ),
            }
        }
        NodeType::StatementList { statements } => {
            let mut ret_id = memory.insert(
                Value::Null,
                area!(info.source.clone(), start_node.span)
            );
            for i in statements {
                ret_id = execute!(i => scope_id);
                if info.exits.len() > 0 {
                    ret!( Ok( ret_id ) );
                }
            }
            Ok(ret_id)
        }
        NodeType::Block { code, not_safe } => {
            let derived = memory.derive_scope(scope_id);
            let value = execute!(code => derived);
            memory.redef(value, area!(info.source.clone(), start_node.span));
            Ok( value )
        }
        NodeType::If { cond, code, else_branch } => {
            let cond_value = execute!(cond => scope_id);
            let mut ret_id = memory.insert(
                Value::Null,
                area!(info.source.clone(), start_node.span)
            );
            if value_ops::to_bool(memory.get(cond_value), area!(info.source.clone(), cond.span), memory)? {
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
                area!(info.source.clone(), start_node.span)
            );
            loop {
                let cond_value = execute!(cond => scope_id);
                if !(value_ops::to_bool(memory.get(cond_value), area!(info.source.clone(), cond.span), memory)?) {
                    break;
                }
                ret_id = execute!(code => scope_id);
                match info.exits.last() {
                    Some(
                        Exit::Break(v, _)
                    ) => {
                        ret_id = *v;
                        info.exits.pop();
                        ret!(Ok( ret_id ));
                    },
                    Some(
                        Exit::Return(_, _)
                    ) => {
                        ret!(Ok( ret_id ));
                    },
                    None => (),
                }
            }
            memory.redef(ret_id, area!(info.source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::For { var: (name, var_area), code, iter } => {
            let mut ret_id = memory.insert(
                Value::Null,
                area!(info.source.clone(), start_node.span)
            );
            let iter_id = execute!(iter => scope_id);
            for i in value_ops::iter(&memory.get(iter_id), area!(info.source.clone(), iter.span), memory)? {
                let derived = memory.derive_scope(scope_id);
                {
                    let temp = memory.insert(
                        i,
                        var_area.clone(),
                    );
                    memory.set_var(derived, name.clone(), temp);
                }
                ret_id = execute!(code => derived);
                match info.exits.last() {
                    Some(
                        Exit::Break(v, _)
                    ) => {
                        ret_id = *v;
                        info.exits.pop();
                        ret!(Ok( ret_id ));
                    },
                    Some(
                        Exit::Return(_, _)
                    ) => {
                        ret!(Ok( ret_id ));
                    },
                    None => (),
                }
            }
            memory.redef(ret_id, area!(info.source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::Loop { code } => {
            let mut ret_id = memory.insert(
                Value::Null,
                area!(info.source.clone(), start_node.span)
            );
            loop {
                ret_id = execute!(code => scope_id);
                match info.exits.last() {
                    Some(
                        Exit::Break(v, _)
                    ) => {
                        ret_id = *v;
                        info.exits.pop();
                        ret!(Ok( ret_id ));
                    },
                    Some(
                        Exit::Return(_, _)
                    ) => {
                        ret!(Ok( ret_id ));
                    },
                    None => (),
                }
            }
            // memory.redef(ret_id, area!(info.source.clone(), start_node.span));
            // Ok( ret_id )
        }
        NodeType::FuncDef { func_name, args, code, header_area } => {
            let mut args_exec = vec![];
            for (s, c, t) in args {
                match t {
                    Some(n) => args_exec.push( (s.clone(), c.clone(), {
                        let _n = execute!(n => scope_id);
                        Some( proteclone!( _n => area!(info.source.clone(), n.span) ) )
                    }) ),
                    None => args_exec.push( (s.clone(), c.clone(), None) ),
                }
            }
            let value_id = memory.insert(
                Value::Function( Function {
                    args: args_exec,
                    code: code.clone(),
                    parent_scope: scope_id,
                    header_area: header_area.clone(),
                    self_arg: None,
                } ),
                area!(info.source.clone(), start_node.span)
            );
            memory.set_var(scope_id, func_name.to_string(), value_id);
            Ok( clone!(value_id => @) )
        }
        NodeType::Lambda { args, code, header_area } => {
            let mut args_exec = vec![];
            for (s, c, t) in args {
                match t {
                    Some(n) => args_exec.push( (s.clone(), c.clone(), {
                        let _n = execute!(n => scope_id);
                        Some( proteclone!( _n => area!(info.source.clone(), n.span) ) )
                    }) ),
                    None => args_exec.push( (s.clone(), c.clone(), None) ),
                }
            }
            Ok( memory.insert(
                Value::Function( Function {
                    args: args_exec,
                    code: code.clone(),
                    parent_scope: scope_id,
                    header_area: header_area.clone(),
                    self_arg: None,
                } ),
                area!(info.source.clone(), start_node.span)
            ) )
        }
        NodeType::Array { elements } => {
            let mut ids = vec![];
            for i in elements {
                ids.push( protecute!(i => scope_id) );
            }
            Ok( memory.insert(
                Value::Array(ids),
                area!(info.source.clone(), start_node.span)
            ) )
        }
        NodeType::Dictionary { map } => {
            let mut id_map = HashMap::new();
            for (k, v) in map {
                let id = match v {
                    Some(n) => protecute!(n => scope_id),
                    None => match memory.get_var(scope_id, k) {
                        Some(i) => proteclone!(i => @),
                        None => ret!(Err(
                            RuntimeError::UndefinedVar {
                                var_name: k.to_string(),
                                area: area!(info.source.clone(), start_node.span),
                            }
                        ))
                    },
                };
                id_map.insert( k.clone(), id );
            }
            Ok( memory.insert(
                Value::Dictionary(id_map),
                area!(info.source.clone(), start_node.span)
            ) )
        }
        NodeType::Index { base, index } => {
            let base_id = protecute!(base => scope_id);
            match &memory.get(base_id).value.clone() {
                Value::Array(arr) => {
                    let index_id = execute!(index => scope_id);
                    match &memory.get(index_id).value.clone() {
                        Value::Number(n) => {
                            let mut id = *n as isize;
                            id = if id < 0 { arr.len() as isize + id } else {id};
                            match arr.get(id as usize) {
                                Some(i) => Ok(*i),
                                None => ret!(Err( RuntimeError::IndexOutOfBounds {
                                    index: id,
                                    length: arr.len(),
                                    area: area!(info.source.clone(), start_node.span),
                                } ))
                            }
                        }
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "number".to_string(),
                            found: format!("{}", other.type_str(memory)),
                            area: area!(info.source.clone(), index.span),
                            defs: vec![(other.type_str(memory), memory.get(index_id).def_area.clone())],
                        } ))
                    }
                },
                Value::Dictionary(map) => {
                    let index_id = execute!(index => scope_id);
                    match &memory.get(index_id).value.clone() {
                        Value::String(s) => {
                            match map.get(s) {
                                Some(i) => Ok(*i),
                                None => ret!(Err( RuntimeError::NonexistentKey {
                                    key: s.clone(),
                                    area: area!(info.source.clone(), start_node.span),
                                } ))
                            }
                        }
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "string".to_string(),
                            found: format!("{}", other.type_str(memory)),
                            area: area!(info.source.clone(), index.span),
                            defs: vec![(other.type_str(memory), memory.get(index_id).def_area.clone())],
                        } ))
                    }
                }
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "array or dict".to_string(),
                    found: format!("{}", other.type_str(memory)),
                    area: area!(info.source.clone(), base.span),
                    defs: vec![(other.type_str(memory), memory.get(base_id).def_area.clone())],
                } ))
            }
        }
        NodeType::Member { base, member } => {
            let base_id = execute!(base => scope_id);
            let typ = memory.get(base_id).value.typ();
            if let Some(data) = match typ {
                ValueType::Builtin(b) => memory.builtin_impls.get(&b),
                ValueType::CustomStruct(id) => memory.impls.get(&id),
            } {
                if data.methods.contains(member) {
                    let func = *data.members.get(member).unwrap();
                    match &memory.get(func).value {
                        Value::Function(f) => {
                            let f_v = Value::Function( Function {
                                args: f.args.clone(),
                                code: f.code.clone(),
                                parent_scope: f.parent_scope,
                                header_area: f.header_area.clone(),
                                self_arg: Some(base.clone()),
                            } );
                            let f_a = memory.get(func).def_area.clone();
                            ret!(Ok( memory.insert(
                                f_v,
                                f_a
                            ) ))
                        },
                        _ => unreachable!(),
                    }
                }
            }
            match &memory.get(base_id).value.clone() {
                Value::Dictionary(map) => {
                    match map.get(member) {
                        Some(i) => Ok(*i),
                        None => ret!(Err( RuntimeError::NonexistentKey {
                            key: member.clone(),
                            area: area!(info.source.clone(), start_node.span),
                        } ))
                    }
                }
                Value::StructInstance { fields, .. } => {
                    match fields.get(member) {
                        Some(i) => Ok(*i),
                        None => ret!(Err( RuntimeError::NonexistentField {
                            field: member.clone(),
                            area: area!(info.source.clone(), start_node.span),
                        } ))
                    }
                }
                _ => ret!(Err( RuntimeError::NonexistentField {
                    field: member.clone(),
                    area: area!(info.source.clone(), start_node.span),
                } ))
            }
        }
        NodeType::Return { node: ret } => {
            // println!("{:?}", info);
            let ret_value = match ret {
                Some(n) => execute!(n => scope_id),
                None => memory.insert(
                    Value::Null,
                    area!(info.source.clone(), start_node.span)
                ),
            };
            info.exits.push( Exit::Return(ret_value, node.span) );
            Ok( clone!(ret_value => @) )
        }
        NodeType::Break { node: ret } => {
            // println!("{:?}", info);
            let ret_value = match ret {
                Some(n) => execute!(n => scope_id),
                None => memory.insert(
                    Value::Null,
                    area!(info.source.clone(), start_node.span)
                ),
            };
            info.exits.push( Exit::Break(ret_value, node.span) );
            Ok( clone!(ret_value => @) )
        }
        NodeType::StructDef { struct_name, fields, field_areas, def_area } => {
            let mut fields_exec = HashMap::new();

            for (k, (t, d)) in fields {
                let _t = protecute!(t => scope_id);
                let t = clone!( _t => area!(info.source.clone(), t.span) );
                let d = if let Some(n) = d {
                    let temp = protecute!(n => scope_id);
                    Some(temp)
                } else { None };
                fields_exec.insert(
                    k.clone(),
                    (t, d)
                );
            }

            let type_id = memory.new_struct( CustomStruct {
                name: struct_name.clone(),
                fields: fields_exec,
                def_area: def_area.clone(),
                field_areas: field_areas.clone(),
            });
            let value_id = memory.insert(
                Value::Type(ValueType::CustomStruct(type_id)),
                area!(info.source.clone(), start_node.span)
            );
            memory.set_var(scope_id, struct_name.to_string(), value_id);
            Ok( value_id )
        }
        NodeType::StructInstance { fields, base, field_areas } => {
            let base_id = protecute!(base => scope_id);
            match &memory.get(base_id).value.clone() {
                Value::Type(ValueType::CustomStruct(id)) => {
                    let struct_type = memory.custom_structs.map[id].clone();

                    let mut id_map = HashMap::new();

                    for (f, (_, d)) in struct_type.fields.clone() {
                        match d {
                            Some(v) => { id_map.insert(f.clone(), v); },
                            None => (),
                        }
                    }
                    for (k, v) in fields {
                        if !struct_type.fields.contains_key(k) {
                            ret!(Err( RuntimeError::NoStructField {
                                field_name: k.clone(),
                                used: field_areas[k].clone(),
                                struct_def: struct_type.def_area.clone(),
                            } ))
                        }
                        let id = match v {
                            Some(n) => protecute!(n => scope_id),
                            None => match memory.get_var(scope_id, k) {
                                Some(i) => proteclone!(i => @),
                                None => ret!(Err(
                                    RuntimeError::UndefinedVar {
                                        var_name: k.to_string(),
                                        area: area!(info.source.clone(), start_node.span),
                                    }
                                ))
                            },
                        };
                        id_map.insert( k.clone(), id );
                    }
                    for (k, v) in &id_map {
                        let typ = struct_type.fields[k].0;
                        let result = value_ops::is_op(&memory.get(*v).clone(), &memory.get(typ).clone(), area!(info.source.clone(), node.span), memory)?;
                        if !result {
                            ret!(Err( RuntimeError::PatternMismatch {
                                typ: memory.get(*v).value.type_str(memory),
                                pattern: match &memory.get(typ).value {
                                    Value::Type(t) => t.to_str(memory),
                                    Value::Pattern(p) => p.to_str(memory),
                                    _ => unreachable!(),
                                },
                                pattern_area: memory.get(typ).def_area.clone(),
                                type_area: memory.get(*v).def_area.clone(),
                            } ))
                        }
                    }
                    if id_map.len() != memory.custom_structs.map[id].fields.len() {
                        let mut missing = vec![];
                        for (k, _) in &memory.custom_structs.map[id].fields {
                            if !id_map.contains_key(k) { missing.push(k.clone()) }
                        }
                        ret!(Err( RuntimeError::MissingStructFields {
                            fields: missing,
                            area: area!(info.source.clone(), start_node.span),
                            struct_def: memory.custom_structs.map[id].def_area.clone(),
                        } ))
                    }
                    Ok( memory.insert(
                        Value::StructInstance { struct_id: *id, fields: id_map },
                        area!(info.source.clone(), start_node.span)
                    ) )
                },
                _ => ret!(Err( RuntimeError::InstanceNonStruct {
                    area: area!(info.source.clone(), base.span),
                } ))
            }
        }
        NodeType::Impl { type_var: (name, name_area), fields } => {

            let mut current_scope = scope_id;
            loop {
                match memory.get_scope(current_scope).vars.get(name) {
                    Some(id) => {
                        match &memory.get(*id).value.clone() {
                            Value::Type(t) => {
                                match t {
                                    ValueType::Builtin(b) => {
                                        if !memory.builtin_impls.contains_key(b) {
                                            memory.builtin_impls.insert(b.clone(), ImplData::new());
                                        }
                                        for (k, v) in fields {
                                            let temp = protecute!(v => scope_id);
                                            memory.builtin_impls.get_mut(b).unwrap().members.insert( k.clone(), temp );
                                            if let ASTNode {
                                                node: NodeType::Lambda { args, .. },
                                                ..
                                            } = v.clone() {
                                                if let Some((s, _, _)) = args.get(0) {
                                                    if s == "self" { memory.builtin_impls.get_mut(b).unwrap().methods.push(k.clone()) }
                                                }
                                            }
                                        }
                                        // memory.builtin_impls.get_mut(b).unwrap().members.insert(k, v)
                                    },
                                    ValueType::CustomStruct(id) => {
                                        if !memory.impls.contains_key(id) {
                                            memory.impls.insert(*id, ImplData::new());
                                        }
                                        for (k, v) in fields {
                                            let temp = protecute!(v => scope_id);
                                            memory.impls.get_mut(id).unwrap().members.insert( k.clone(), temp );
                                            if let ASTNode {
                                                node: NodeType::Lambda { args, .. },
                                                ..
                                            } = v.clone() {
                                                if let Some((s, _, _)) = args.get(0) {
                                                    if s == "self" { memory.impls.get_mut(id).unwrap().methods.push(k.clone()) }
                                                }
                                            }
                                        }
                                        // memory.builtin_impls.get_mut(b).unwrap().members.insert(k, v)
                                    },
                                };
                                ret!(Ok(memory.insert(
                                    Value::Null,
                                    area!(info.source.clone(), start_node.span)
                                )))
                            },
                            other => {
                                let temp = memory.get(*id).def_area.clone();
                                ret!(Err( RuntimeError::TypeMismatch {
                                    expected: "type".to_string(),
                                    found: format!("{}", other.type_str(memory)),
                                    area: name_area.clone(),
                                    defs: vec![(other.type_str(memory), temp)],
                                } ))
                            }
                        }
                    },
                    None => match memory.get_scope(current_scope).parent_id {
                        Some(p_id) => current_scope = p_id,
                        None => break,
                    },
                }
            }
            Err(
                RuntimeError::UndefinedVar {
                    var_name: name.to_string(),
                    area: area!(info.source.clone(), start_node.span),
                }
            )
        },
        NodeType::Associated { base, assoc } => {
            let base_id = protecute!(base => scope_id);
            match &memory.get(base_id).value.clone() {
                Value::Type(t) => {
                    let impld = match t {
                        ValueType::Builtin(b) => memory.builtin_impls.get(b).clone(),
                        ValueType::CustomStruct(id) => memory.impls.get(id).clone(),
                    };
                    match impld {
                        None => Err( RuntimeError::NoAssociatedMember {
                            assoc: assoc.clone(),
                            area: area!(info.source.clone(), start_node.span),
                        } ),
                        Some(fields) => {
                            match fields.members.get(assoc) {
                                Some(i) => Ok(*i),
                                None => ret!(Err( RuntimeError::NoAssociatedMember {
                                    assoc: assoc.clone(),
                                    area: area!(info.source.clone(), start_node.span),
                                } ))
                            }
                        }
                    }
                }
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "type".to_string(),
                    found: format!("{}", other.type_str(memory)),
                    area: area!(info.source.clone(), base.span),
                    defs: vec![(other.type_str(memory), memory.get(base_id).def_area.clone())],
                } ))
            }
        },
        NodeType::Call { base, args } => {
            let base_id = protecute!(base => scope_id);
            
            match &memory.get(base_id).value.clone() {
                Value::Builtin(name) => run_builtin(
                    name_to_builtin(name),
                    start_node,
                    args,
                    scope_id,
                    memory,
                    info
                ),
                
                Value::Function( Function { args: func_args, code, parent_scope, header_area, self_arg} ) => {
                    let mut args = args.clone();
                    if let Some(n) = self_arg {
                        args.insert(0, *n.clone());
                    }
                    if func_args.len() != args.len() {
                        ret!(Err(
                            RuntimeError::IncorrectArgumentCount {
                                provided: args.len(),
                                takes: func_args.len(),
                                header_area: header_area.clone(),
                                call_area: area!(info.source.clone(), start_node.span)
                            }
                        ))
                    }

                    let derived = memory.derive_scope(*parent_scope);
                    for (arg, (name, a, d)) in args.iter().zip(func_args) {
                        let arg_id = if name == "self" { protecute_raw!(arg => scope_id) } else { protecute!(arg => scope_id) };
                        
                        match d {
                            Some(typ) => {
                                let result = value_ops::is_op(&memory.get(arg_id).clone(), &memory.get(*typ).clone(), area!(info.source.clone(), node.span), memory)?;
                                if !result {
                                    ret!(Err( RuntimeError::PatternMismatch {
                                        typ: memory.get(arg_id).value.type_str(memory),
                                        pattern: match &memory.get(*typ).value {
                                            Value::Type(t) => t.to_str(memory),
                                            _ => unreachable!(),
                                        },
                                        pattern_area: memory.get(*typ).def_area.clone(),
                                        type_area: memory.get(arg_id).def_area.clone(),
                                    } ))
                                }
                            },
                            None => (),
                        }
                        let arg_id = clone!(arg_id => a.clone());
                        memory.set_var(derived, name.clone(), arg_id);
                    }
                    
                    info.trace.push( area!(info.source.clone(), start_node.span) );
                    let ret_id = execute!(code => derived);
                    match info.exits.last() {
                        Some(
                            Exit::Return(v, _)
                        ) => {
                            let ret_id = *v;
                            info.exits.pop();
                            ret!( Ok( ret_id ) )
                        },
                        Some(
                            Exit::Break(_, span)
                        ) => {
                            ret!(Err(
                                RuntimeError::BreakUsedOutside {
                                    break_area: area!(info.source.clone(), *span),
                                    outer_area: area!(info.source.clone(), code.span),
                                }
                            ))
                        },
                        None => (),
                    }
                    info.trace.pop();
                    memory.redef(ret_id, area!(info.source.clone(), start_node.span));
                    Ok( ret_id )
                },

                Value::Type(_) => {
                    if args.len() != 1 {
                        ret!(Err(
                            RuntimeError::TypeArgCount {
                                provided: args.len(),
                                call_area: area!(info.source.clone(), start_node.span)
                            }
                        ))
                    }
                    let arg_id = execute!(&args[0] => scope_id);
                    let result = value_ops::convert(&memory.get(arg_id).clone(), &memory.get(base_id).clone(), area!(info.source.clone(), node.span), memory)?;
                    Ok(memory.insert(
                        result,
                        area!(info.source.clone(), node.span),
                    ))
                }
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "function, builtin or type".to_string(),
                    found: format!("{}", other.type_str(memory)),
                    area: area!(info.source.clone(), base.span),
                    defs: vec![(other.type_str(memory), memory.get(base_id).def_area.clone())],
                } ))
            }


        }
    };

    memory.pop_protected();

    exec_result

}


