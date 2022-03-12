

use std::{collections::{HashMap, HashSet}, fs, path::PathBuf};

use logos::Logos;

use crate::{value::{Value, value_ops, Function, ValueType, Pattern}, parser::{ASTNode, NodeType}, EmeraldSource, error::RuntimeError, lexer::{Token, self}, CodeArea, builtins::{BuiltinType, builtin_names, builtin_type_names, builtin_type_from_str}, EmeraldCache};
use crate::builtins::{run_builtin, name_to_builtin};

#[derive(Debug, Clone)]
pub enum Exit {
    Return(ValuePos, (usize, usize)),
    Break(ValuePos, (usize, usize)),
}


pub type ValuePos = usize;
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
    pub fields: HashMap<String, (ValuePos, Option<ValuePos>)>,
    pub field_areas: HashMap<String, CodeArea>,
    pub def_area: CodeArea,
}


#[derive(Debug, Clone)]
pub struct ImplData {
    pub members: HashMap<String, ValuePos>,
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
    pub marked_values: HashSet<ValuePos>,
    pub marked_scopes: HashSet<ScopePos>,
}

impl Collector {
    pub fn new(globals: &Globals) -> Self {
        let mut marked_scopes = HashSet::new();
        let mut marked_values = HashSet::new();
        for (i, _) in &globals.values.map {
            marked_values.insert(*i);
        }
        for (i, _) in &globals.scopes.map {
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
    pub values: Vec<ValuePos>,
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



pub struct Globals {
    pub values: Register<ValuePos, StoredValue>,
    pub scopes: Register<ScopePos, Scope>,
    pub custom_structs: Register<TypePos, CustomStruct>,



    pub last_amount: usize,
    pub protected: Vec<Protector>,

    pub impls: HashMap<TypePos, ImplData>,
    pub builtin_impls: HashMap<BuiltinType, ImplData>,

    
    pub exits: Vec<Exit>,
    pub trace: Vec<CodeArea>,
    pub import_trace: Vec<CodeArea>,

    pub exports: Vec<HashMap<String, ValuePos>>,

}




pub struct Scope {
    vars: HashMap<String, ValuePos>,
    parent_id: Option<ScopePos>,
    extra_prot: Vec<ScopePos>,
}



impl Globals {

    pub fn new() -> Self {
        let mut scopes = Register {
            map: HashMap::new(),
            counter: 0,
        };
        scopes.map.insert(0, Scope { vars: HashMap::new(), parent_id: None, extra_prot: vec![] });
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
            builtin_impls: HashMap::new(),
            exits: vec![],
            trace: vec![],
            import_trace: vec![],
            exports: vec![],
        }
    }

    pub fn init_global(&mut self, scope_id: ScopePos, source: EmeraldSource) {
        for i in builtin_names() {
            let id = self.insert_value(
                Value::Builtin(i.clone()),
                CodeArea {
                    source: source.clone(),
                    range: (0, 0)
                },
            );
            self.set_var(scope_id, i, id);
        }

        for i in builtin_type_names() {
            let id = self.insert_value(
                Value::Type(ValueType::Builtin(builtin_type_from_str(&i))),
                CodeArea {
                    source: source.clone(),
                    range: (0, 0)
                },
            );
            self.set_var(scope_id, i.to_lowercase().to_string(), id);
        }
        {
            let id = self.insert_value(
                Value::Pattern(Pattern::Any),
                CodeArea {
                    source: source.clone(),
                    range: (0, 0)
                },
            );
            self.set_var(scope_id, "any".to_string(), id);
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
    ) -> ValuePos {
        self.custom_structs.counter += 1;
        self.custom_structs.map.insert( self.custom_structs.counter, typ );
        self.custom_structs.counter
    }



    pub fn insert_value(
        &mut self,
        value: Value,
        def_area: CodeArea,
    ) -> ValuePos {
        self.values.counter += 1;
        self.values.map.insert( self.values.counter, StoredValue {
            value,
            def_area,
        } );
        self.values.counter
    }
    pub fn set_value(
        &mut self,
        id: ValuePos,
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
    pub fn redef_value(
        &mut self,
        id: ValuePos,
        def_area: CodeArea,
    ) {
        self.values.map.get_mut(&id).unwrap().def_area = def_area;
    }
    pub fn clone_value(
        &mut self,
        id: ValuePos,
        def_area: Option<CodeArea>,
    ) -> ValuePos {
        let (v_v, v_a) = {
            let val = self.get(id);
            (val.value.clone(), val.def_area.clone())
        };
        self.insert_value(
            v_v,
            if let Some(a) = def_area {a} else {v_a},
        )
    }
    pub fn get(&self, id: ValuePos) -> &StoredValue {
        return match self.values.map.get(&id) {
            Some(v) => v,
            None => panic!("bad value fuck you"),
        }
    }

    // pub fn propagate_def(
    //     &mut self,
    //     id: ValuePos,
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
    pub fn protect_value(&mut self, pos: ValuePos) {
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


    pub fn insert_scope(
        &mut self,
        scope: Scope,
    ) -> ValuePos {
        self.scopes.counter += 1;
        self.scopes.map.insert( self.scopes.counter, scope );
        self.scopes.counter
    }
    fn derive_scope(&mut self, scope_id: ScopePos) -> ScopePos {
        self.scopes.counter += 1;
        self.scopes.map.insert(self.scopes.counter, Scope {
            vars: HashMap::new(),
            parent_id: Some(scope_id),
            extra_prot: vec![],
        });
        self.protect_scope(self.scopes.counter);
        self.scopes.counter
    }

    pub fn set_var(
        &mut self,
        scope_id: ScopePos,
        name: String,
        val_id: ValuePos,
    ) {
        self.get_scope_mut(scope_id).vars.insert(name, val_id);
    }

    pub fn get_var(
        &self,
        scope_id: ScopePos,
        name: &String,
    ) -> Option<ValuePos> {
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
                self.mark_value(*id, &mut collector);
            }
            for id in &prot.scopes {
                if collector.marked_scopes.contains(id) {
                    self.mark_scope(*id, &mut collector);
                }
            }
        }

        for (_, s) in &self.custom_structs.map {
            for (_, (t, d)) in &s.fields {
                self.mark_value(*t, &mut collector);
                if let Some(d) = d {
                    self.mark_value(*d, &mut collector);
                }
            }
        }

        for (_, ImplData { members, .. }) in &self.impls {
            for (_, v) in members {
                self.mark_value(*v, &mut collector);
            }
        }
        for (_, ImplData { members, .. }) in &self.builtin_impls {
            for (_, v) in members {
                self.mark_value(*v, &mut collector);
            }
        }
        for e in &self.exports {
            for (_, v) in e {
                self.mark_value(*v, &mut collector);
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

    pub fn mark_value(&self, value_id: ValuePos, collector: &mut Collector) {
        if collector.marked_values.remove(&value_id) {
            let mut value_ids: HashSet<ValuePos> = HashSet::new();
            let mut scope_ids: HashSet<ScopePos> = HashSet::new();
            self.get(value_id).value.get_references(self, &mut value_ids, &mut scope_ids);
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

    pub fn mark_scope(&self, scope_id: ScopePos, collector: &mut Collector) {
        let mut var_checks = Vec::new();
        collector.marked_scopes.remove(&scope_id);
        for (_, id) in &self.get_scope(scope_id).vars {
            var_checks.push(*id);
        }
        for id in var_checks {
            self.mark_value(id, collector);
        }
        match self.get_scope(scope_id).parent_id {
            Some(id) => if collector.marked_scopes.contains(&id) { self.mark_scope(id, collector) },
            None => (),
        }
        for i in &self.get_scope(scope_id).extra_prot {
            if collector.marked_scopes.contains(&i) { self.mark_scope(*i, collector) }
        }
    }


}




macro_rules! interpreter_util {
    ($globals:expr, $source:expr) => {
        macro_rules! execute {
            ($node:expr => $scope_id:expr) => {
                {
                    let id = execute($node, $scope_id, $globals, $source)?;
                    $globals.clone_value(
                        id,
                        None,
                    )
                }
            }
        }
        macro_rules! execute_raw {
            ($node:expr => $scope_id:expr) => {
                {
                    execute($node, $scope_id, $globals, $source)?
                }
            }
        }
        macro_rules! protecute {
            ($node:expr => $scope_id:expr) => {
                {
                    let id = execute($node, $scope_id, $globals, $source)?;
                    let out = $globals.clone_value(
                        id,
                        None,
                    );
                    $globals.protect_value(id);
                    $globals.protect_value(out);
                    out
                }
            }
        }
        macro_rules! protecute_raw {
            ($node:expr => $scope_id:expr) => {
                {
                    let id = execute($node, $scope_id, $globals, $source)?;
                    $globals.protect_value(id);
                    id
                }
            }
        }
        macro_rules! clone {
            ($id:expr => @ ) => {
                {
                    let new_id = $globals.clone_value(
                        $id,
                        None,
                    );
                    new_id
                }
            };
            ($id:expr => $area:expr ) => {
                {
                    let new_id = $globals.clone_value(
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
                    let new_id = $globals.clone_value(
                        $id,
                        None,
                    );
                    $globals.protect_value(new_id);
                    new_id
                }
            };
            ($id:expr => $area:expr ) => {
                {
                    let new_id = $globals.clone_value(
                        $id,
                        Some($area),
                    );
                    $globals.protect_value(new_id);
                    new_id
                }
            };
        }
        macro_rules! ret {
            ($thing:expr) => {
                {
                    $globals.pop_protected();
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
    globals: &mut Globals,
    source: EmeraldSource,
) -> Result<ValuePos, RuntimeError> {
    interpreter_util!(globals, source.clone());

    let start_node = node;

    // if globals.values.map.len() > 50000 + globals.last_amount {
    if true {
        // println!("{}", globals.values.map.len());
        globals.collect(scope_id);
        // println!("{} {}\n", globals.values.map.len(), globals.last_amount);
    }

    globals.push_protected();
    
    let exec_result = match &node.node {
        NodeType::Value { value } => Ok(globals.insert_value(
            value.clone(),
            CodeArea {source: source.clone(), range: node.span},
        )),
        NodeType::Op { left, op, right } => {
            match op {
                Token::Plus | Token::Minus | Token::Mult | Token::Div | Token::Mod | Token::Pow | Token::Eq | Token::NotEq | Token::Greater | Token::GreaterEq | Token::Lesser | Token::LesserEq | Token::As | Token::Is | Token::Pipe => {
                    let left = protecute!(left => scope_id);
                    let right = protecute!(right => scope_id);
                    match op {
                        Token::Plus => {
                            let result = value_ops::plus(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Minus => {
                            let result = value_ops::minus(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Mult => {
                            let result = value_ops::mult(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Div => {
                            let result = value_ops::div(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Mod => {
                            let result = value_ops::modulo(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Pow => {
                            let result = value_ops::pow(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Eq => {
                            let result = value_ops::eq(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::NotEq => {
                            let result = value_ops::neq(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Greater => {
                            let result = value_ops::greater(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::GreaterEq => {
                            let result = value_ops::greater_eq(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Lesser => {
                            let result = value_ops::lesser(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::LesserEq => {
                            let result = value_ops::lesser_eq(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::As => {
                            let result = value_ops::convert(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Is => {
                            let result = value_ops::is_op(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                Value::Boolean(result),
                                area!(source.clone(), node.span),
                            ))
                        },
                        Token::Pipe => {
                            let result = value_ops::either(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
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
                            let result = value_ops::plus(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            right = globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            )
                        },
                        Token::MinusEq => {
                            let result = value_ops::minus(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            right = globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            )
                        },
                        Token::MultEq => {
                            let result = value_ops::mult(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            right = globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            )
                        },
                        Token::DivEq => {
                            let result = value_ops::div(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            right = globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            )
                        },
                        Token::ModEq => {
                            let result = value_ops::modulo(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            right = globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            )
                        },
                        Token::PowEq => {
                            let result = value_ops::pow(&globals.get(left).clone(), &globals.get(right).clone(), area!(source.clone(), node.span), globals)?;
                            right = globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            )
                        },
                        _ => (),
                    }
                    globals.set_value(
                        left,
                        globals.get(right).value.clone(),
                        Some(area!(source.clone(), start_node.span))
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
                    let result = value_ops::identity(&globals.get(value).clone(), area!(source.clone(), node.span), globals)?;
                    Ok(globals.insert_value(
                        result,
                        area!(source.clone(), node.span),
                    ))
                },
                Token::Minus => {
                    let result = value_ops::negate(&globals.get(value).clone(), area!(source.clone(), node.span), globals)?;
                    Ok(globals.insert_value(
                        result,
                        area!(source.clone(), node.span),
                    ))
                },
                _ => unreachable!()
            }
        },
        NodeType::Declaration { var_name, value } => {
            let initial_id = execute!(value => scope_id);
            // let value_id = clone!(initial_id => area!(source.clone(), start_node.span));
            globals.redef_value(initial_id, area!(source.clone(), start_node.span));
            // globals.propagate_def(value_id);

            globals.set_var(scope_id, var_name.to_string(), initial_id);
            Ok(initial_id)
        }
        NodeType::Var { var_name } => {
            // println!("c: {:?} {}", globals.get_scope(scope_id).vars, var_name);
            match globals.get_var(scope_id, var_name) {
                Some(i) => Ok( i ),
                None => Err(
                    RuntimeError::UndefinedVar {
                        var_name: var_name.to_string(),
                        area: area!(source.clone(), start_node.span),
                    }
                ),
            }
        }
        NodeType::StatementList { statements } => {
            let mut ret_id = globals.insert_value(
                Value::Null,
                area!(source.clone(), start_node.span)
            );
            for i in statements {
                // println!("d1: {:?}", globals.get_scope(scope_id).vars);
                ret_id = execute!(i => scope_id);
                // println!("d2: {:?}", globals.get_scope(scope_id).vars);
                if globals.exits.len() > 0 {
                    ret!( Ok( ret_id ) );
                }
            }
            Ok(ret_id)
        }
        NodeType::Block { code, .. } => {
            let derived = globals.derive_scope(scope_id);
            let value = execute!(code => derived);
            globals.redef_value(value, area!(source.clone(), start_node.span));
            Ok( value )
        }
        NodeType::If { cond, code, else_branch } => {
            let cond_value = execute!(cond => scope_id);
            let mut ret_id = globals.insert_value(
                Value::Null,
                area!(source.clone(), start_node.span)
            );
            if value_ops::to_bool(globals.get(cond_value), area!(source.clone(), cond.span), globals)? {
                ret_id = execute!(code => scope_id)
            } else if let Some(b) = else_branch {
                ret_id = execute!(b => scope_id)
            }
            globals.redef_value(ret_id, area!(source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::While { cond, code } => {
            let mut ret_id = globals.insert_value(
                Value::Null,
                area!(source.clone(), start_node.span)
            );
            loop {
                let cond_value = execute!(cond => scope_id);
                if !(value_ops::to_bool(globals.get(cond_value), area!(source.clone(), cond.span), globals)?) {
                    break;
                }
                ret_id = execute!(code => scope_id);
                match globals.exits.last() {
                    Some(
                        Exit::Break(v, _)
                    ) => {
                        ret_id = *v;
                        globals.exits.pop();
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
            globals.redef_value(ret_id, area!(source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::For { var: (name, var_area), code, iter } => {
            let mut ret_id = globals.insert_value(
                Value::Null,
                area!(source.clone(), start_node.span)
            );
            let iter_id = execute!(iter => scope_id);
            for i in value_ops::iter(&globals.get(iter_id), area!(source.clone(), iter.span), globals)? {
                let derived = globals.derive_scope(scope_id);
                {
                    let temp = globals.insert_value(
                        i,
                        var_area.clone(),
                    );
                    globals.set_var(derived, name.clone(), temp);
                }
                ret_id = execute!(code => derived);
                match globals.exits.last() {
                    Some(
                        Exit::Break(v, _)
                    ) => {
                        ret_id = *v;
                        globals.exits.pop();
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
            globals.redef_value(ret_id, area!(source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::Loop { code } => {
            loop {
                let mut ret_id = execute!(code => scope_id);
                match globals.exits.last() {
                    Some(
                        Exit::Break(v, _)
                    ) => {
                        ret_id = *v;
                        globals.exits.pop();
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
        }
        NodeType::FuncDef { func_name, args, code, header_area } => {
            let mut args_exec = vec![];
            for (s, c, t) in args {
                match t {
                    Some(n) => args_exec.push( (s.clone(), c.clone(), {
                        let _n = execute!(n => scope_id);
                        Some( proteclone!( _n => area!(source.clone(), n.span) ) )
                    }) ),
                    None => args_exec.push( (s.clone(), c.clone(), None) ),
                }
            }
            let value_id = globals.insert_value(
                Value::Function( Function {
                    args: args_exec,
                    code: code.clone(),
                    parent_scope: scope_id,
                    header_area: header_area.clone(),
                    self_arg: None,
                } ),
                area!(source.clone(), start_node.span)
            );
            globals.set_var(scope_id, func_name.to_string(), value_id);
            Ok( clone!(value_id => @) )
        }
        NodeType::Lambda { args, code, header_area } => {
            let mut args_exec = vec![];
            for (s, c, t) in args {
                match t {
                    Some(n) => args_exec.push( (s.clone(), c.clone(), {
                        let _n = execute!(n => scope_id);
                        Some( proteclone!( _n => area!(source.clone(), n.span) ) )
                    }) ),
                    None => args_exec.push( (s.clone(), c.clone(), None) ),
                }
            }
            Ok( globals.insert_value(
                Value::Function( Function {
                    args: args_exec,
                    code: code.clone(),
                    parent_scope: scope_id,
                    header_area: header_area.clone(),
                    self_arg: None,
                } ),
                area!(source.clone(), start_node.span)
            ) )
        }
        NodeType::Array { elements } => {
            let mut ids = vec![];
            for i in elements {
                ids.push( protecute!(i => scope_id) );
            }
            Ok( globals.insert_value(
                Value::Array(ids),
                area!(source.clone(), start_node.span)
            ) )
        }
        NodeType::Dictionary { map } => {
            let mut id_map = HashMap::new();
            for (k, v) in map {
                let id = match v {
                    Some(n) => protecute!(n => scope_id),
                    None => match globals.get_var(scope_id, k) {
                        Some(i) => proteclone!(i => @),
                        None => ret!(Err(
                            RuntimeError::UndefinedVar {
                                var_name: k.to_string(),
                                area: area!(source.clone(), start_node.span),
                            }
                        ))
                    },
                };
                id_map.insert( k.clone(), id );
            }
            Ok( globals.insert_value(
                Value::Dictionary(id_map),
                area!(source.clone(), start_node.span)
            ) )
        }
        NodeType::Index { base, index } => {
            let base_id = protecute!(base => scope_id);
            match &globals.get(base_id).value.clone() {
                Value::Array(arr) => {
                    let index_id = execute!(index => scope_id);
                    match &globals.get(index_id).value.clone() {
                        Value::Number(n) => {
                            let mut id = *n as isize;
                            id = if id < 0 { arr.len() as isize + id } else {id};
                            match arr.get(id as usize) {
                                Some(i) => Ok(*i),
                                None => ret!(Err( RuntimeError::IndexOutOfBounds {
                                    index: id,
                                    length: arr.len(),
                                    area: area!(source.clone(), start_node.span),
                                } ))
                            }
                        }
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "number".to_string(),
                            found: format!("{}", other.type_str(globals)),
                            area: area!(source.clone(), index.span),
                            defs: vec![(other.type_str(globals), globals.get(index_id).def_area.clone())],
                        } ))
                    }
                },
                Value::Dictionary(map) => {
                    let index_id = execute!(index => scope_id);
                    match &globals.get(index_id).value.clone() {
                        Value::String(s) => {
                            match map.get(s) {
                                Some(i) => Ok(*i),
                                None => ret!(Err( RuntimeError::NonexistentKey {
                                    key: s.clone(),
                                    area: area!(source.clone(), start_node.span),
                                } ))
                            }
                        }
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "string".to_string(),
                            found: format!("{}", other.type_str(globals)),
                            area: area!(source.clone(), index.span),
                            defs: vec![(other.type_str(globals), globals.get(index_id).def_area.clone())],
                        } ))
                    }
                }
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "array or dict".to_string(),
                    found: format!("{}", other.type_str(globals)),
                    area: area!(source.clone(), base.span),
                    defs: vec![(other.type_str(globals), globals.get(base_id).def_area.clone())],
                } ))
            }
        }
        NodeType::Member { base, member } => {
            let base_id = execute!(base => scope_id);
            let typ = globals.get(base_id).value.typ();
            if let Some(data) = match typ {
                ValueType::Builtin(b) => globals.builtin_impls.get(&b),
                ValueType::CustomStruct(id) => globals.impls.get(&id),
            } {
                if data.methods.contains(member) {
                    let func = *data.members.get(member).unwrap();
                    match &globals.get(func).value {
                        Value::Function(f) => {
                            let f_v = Value::Function( Function {
                                args: f.args.clone(),
                                code: f.code.clone(),
                                parent_scope: f.parent_scope,
                                header_area: f.header_area.clone(),
                                self_arg: Some(base.clone()),
                            } );
                            let f_a = globals.get(func).def_area.clone();
                            ret!(Ok( globals.insert_value(
                                f_v,
                                f_a
                            ) ))
                        },
                        _ => unreachable!(),
                    }
                }
            }
            match &globals.get(base_id).value.clone() {
                Value::Dictionary(map) => {
                    match map.get(member) {
                        Some(i) => Ok(*i),
                        None => ret!(Err( RuntimeError::NonexistentKey {
                            key: member.clone(),
                            area: area!(source.clone(), start_node.span),
                        } ))
                    }
                }
                Value::StructInstance { fields, .. } => {
                    match fields.get(member) {
                        Some(i) => Ok(*i),
                        None => ret!(Err( RuntimeError::NonexistentField {
                            field: member.clone(),
                            area: area!(source.clone(), start_node.span),
                        } ))
                    }
                }
                _ => ret!(Err( RuntimeError::NonexistentField {
                    field: member.clone(),
                    area: area!(source.clone(), start_node.span),
                } ))
            }
        }
        NodeType::Return { node: ret } => {
            // println!("{:?}", info);
            let ret_value = match ret {
                Some(n) => execute!(n => scope_id),
                None => globals.insert_value(
                    Value::Null,
                    area!(source.clone(), start_node.span)
                ),
            };
            globals.exits.push( Exit::Return(ret_value, node.span) );
            Ok( clone!(ret_value => @) )
        }
        NodeType::Break { node: ret } => {
            // println!("{:?}", info);
            let ret_value = match ret {
                Some(n) => execute!(n => scope_id),
                None => globals.insert_value(
                    Value::Null,
                    area!(source.clone(), start_node.span)
                ),
            };
            globals.exits.push( Exit::Break(ret_value, node.span) );
            Ok( clone!(ret_value => @) )
        }
        NodeType::StructDef { struct_name, fields, field_areas, def_area } => {
            let mut fields_exec = HashMap::new();

            for (k, (t, d)) in fields {
                let _t = protecute!(t => scope_id);
                let t = proteclone!( _t => area!(source.clone(), t.span) );
                let d = if let Some(n) = d {
                    let temp = protecute!(n => scope_id);
                    Some(temp)
                } else { None };
                fields_exec.insert(
                    k.clone(),
                    (t, d)
                );
            }

            let type_id = globals.new_struct( CustomStruct {
                name: struct_name.clone(),
                fields: fields_exec,
                def_area: def_area.clone(),
                field_areas: field_areas.clone(),
            });
            let value_id = globals.insert_value(
                Value::Type(ValueType::CustomStruct(type_id)),
                area!(source.clone(), start_node.span)
            );
            globals.set_var(scope_id, struct_name.to_string(), value_id);
            Ok( value_id )
        }
        NodeType::StructInstance { fields, base, field_areas } => {
            let base_id = protecute!(base => scope_id);
            match &globals.get(base_id).value.clone() {
                Value::Type(ValueType::CustomStruct(id)) => {
                    let struct_type = globals.custom_structs.map[id].clone();

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
                            None => match globals.get_var(scope_id, k) {
                                Some(i) => proteclone!(i => @),
                                None => ret!(Err(
                                    RuntimeError::UndefinedVar {
                                        var_name: k.to_string(),
                                        area: area!(source.clone(), start_node.span),
                                    }
                                ))
                            },
                        };
                        id_map.insert( k.clone(), id );
                    }
                    for (k, v) in &id_map {
                        let typ = struct_type.fields[k].0;
                        let result = value_ops::is_op(&globals.get(*v).clone(), &globals.get(typ).clone(), area!(source.clone(), node.span), globals)?;
                        if !result {
                            ret!(Err( RuntimeError::PatternMismatch {
                                typ: globals.get(*v).value.type_str(globals),
                                pattern: match &globals.get(typ).value {
                                    Value::Type(t) => t.to_str(globals),
                                    Value::Pattern(p) => p.to_str(globals),
                                    _ => unreachable!(),
                                },
                                pattern_area: globals.get(typ).def_area.clone(),
                                type_area: globals.get(*v).def_area.clone(),
                            } ))
                        }
                    }
                    if id_map.len() != globals.custom_structs.map[id].fields.len() {
                        let mut missing = vec![];
                        for (k, _) in &globals.custom_structs.map[id].fields {
                            if !id_map.contains_key(k) { missing.push(k.clone()) }
                        }
                        ret!(Err( RuntimeError::MissingStructFields {
                            fields: missing,
                            area: area!(source.clone(), start_node.span),
                            struct_def: globals.custom_structs.map[id].def_area.clone(),
                        } ))
                    }
                    Ok( globals.insert_value(
                        Value::StructInstance { struct_id: *id, fields: id_map },
                        area!(source.clone(), start_node.span)
                    ) )
                },
                _ => ret!(Err( RuntimeError::InstanceNonStruct {
                    area: area!(source.clone(), base.span),
                } ))
            }
        }
        NodeType::Impl { type_var: (name, name_area), fields } => {

            let mut current_scope = scope_id;
            loop {
                match globals.get_scope(current_scope).vars.get(name) {
                    Some(id) => {
                        match &globals.get(*id).value.clone() {
                            Value::Type(t) => {
                                match t {
                                    ValueType::Builtin(b) => {
                                        if !globals.builtin_impls.contains_key(b) {
                                            globals.builtin_impls.insert(b.clone(), ImplData::new());
                                        }
                                        for (k, v) in fields {
                                            let temp = protecute!(v => scope_id);
                                            globals.builtin_impls.get_mut(b).unwrap().members.insert( k.clone(), temp );
                                            if let ASTNode {
                                                node: NodeType::Lambda { args, .. },
                                                ..
                                            } = v.clone() {
                                                if let Some((s, _, _)) = args.get(0) {
                                                    if s == "self" { globals.builtin_impls.get_mut(b).unwrap().methods.push(k.clone()) }
                                                }
                                            }
                                        }
                                        // globals.builtin_impls.get_mut(b).unwrap().members.insert(k, v)
                                    },
                                    ValueType::CustomStruct(id) => {
                                        if !globals.impls.contains_key(id) {
                                            globals.impls.insert(*id, ImplData::new());
                                        }
                                        for (k, v) in fields {
                                            let temp = protecute!(v => scope_id);
                                            globals.impls.get_mut(id).unwrap().members.insert( k.clone(), temp );
                                            if let ASTNode {
                                                node: NodeType::Lambda { args, .. },
                                                ..
                                            } = v.clone() {
                                                if let Some((s, _, _)) = args.get(0) {
                                                    if s == "self" { globals.impls.get_mut(id).unwrap().methods.push(k.clone()) }
                                                }
                                            }
                                        }
                                        // globals.builtin_impls.get_mut(b).unwrap().members.insert(k, v)
                                    },
                                };
                                ret!(Ok(globals.insert_value(
                                    Value::Null,
                                    area!(source.clone(), start_node.span)
                                )))
                            },
                            other => {
                                let temp = globals.get(*id).def_area.clone();
                                ret!(Err( RuntimeError::TypeMismatch {
                                    expected: "type".to_string(),
                                    found: format!("{}", other.type_str(globals)),
                                    area: name_area.clone(),
                                    defs: vec![(other.type_str(globals), temp)],
                                } ))
                            }
                        }
                    },
                    None => match globals.get_scope(current_scope).parent_id {
                        Some(p_id) => current_scope = p_id,
                        None => break,
                    },
                }
            }
            Err(
                RuntimeError::UndefinedVar {
                    var_name: name.to_string(),
                    area: area!(source.clone(), start_node.span),
                }
            )
        },
        NodeType::Associated { base, assoc } => {
            let base_id = protecute!(base => scope_id);
            match &globals.get(base_id).value.clone() {
                Value::Type(t) => {
                    let impld = match t {
                        ValueType::Builtin(b) => globals.builtin_impls.get(b).clone(),
                        ValueType::CustomStruct(id) => globals.impls.get(id).clone(),
                    };
                    match impld {
                        None => Err( RuntimeError::NoAssociatedMember {
                            assoc: assoc.clone(),
                            area: area!(source.clone(), start_node.span),
                        } ),
                        Some(fields) => {
                            match fields.members.get(assoc) {
                                Some(i) => Ok(*i),
                                None => ret!(Err( RuntimeError::NoAssociatedMember {
                                    assoc: assoc.clone(),
                                    area: area!(source.clone(), start_node.span),
                                } ))
                            }
                        }
                    }
                }
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "type".to_string(),
                    found: format!("{}", other.type_str(globals)),
                    area: area!(source.clone(), base.span),
                    defs: vec![(other.type_str(globals), globals.get(base_id).def_area.clone())],
                } ))
            }
        },
        NodeType::Import { path } => {

            let mut buf = PathBuf::new();
            buf.push(path);
            let code = match fs::read_to_string(buf.clone()) {
                Ok(s) => s,
                Err(_) => ret!(Err( RuntimeError::NonexistentFile {
                    path: path.clone(),
                    area: area!(source.clone(), start_node.span),
                } )),
            };

            let mut tokens_iter = lexer::Token
                ::lexer(&code);
            let mut tokens = vec![];
            loop {
                match tokens_iter.next() {
                    Some(t) => tokens.push((
                        t,
                        (
                            tokens_iter.span().start,
                            tokens_iter.span().end,
                        )
                    )),
                    None => break,
                }
            }
            tokens.push((
                lexer::Token::Eof,
                (code.len(), code.len())
            ));

            // println!("{:?}", tokens);

            let mod_source = EmeraldSource::File(buf);
            let ast = crate::parser::parse(&tokens, &mod_source);
            match ast {
                Ok((node, _)) => {
                    

                    let mod_scope_root = globals.insert_scope(Scope { vars: HashMap::new(), parent_id: None, extra_prot: vec![scope_id] });
                    globals.protect_scope(mod_scope_root);

                    globals.import_trace.push( area!(source.clone(), start_node.span) );
                    globals.exports.push( HashMap::new() );
                    globals.init_global(mod_scope_root, mod_source.clone());

                    execute(
                        &node,
                        mod_scope_root,
                        globals,
                        mod_source.clone(),
                    )?;

                    globals.import_trace.pop();

                    let ret_value = Value::Dictionary( globals.exports.last().unwrap().clone() );
                    // for i in globals.exports.last().unwrap() {
                    //     println!("_ {} {}", i.0, i.1);
                    //     println!("__ {:?}", globals.get(*i.1).value);
                    // }


                    globals.exports.pop();

                    
                    Ok(globals.insert_value(
                        ret_value,
                        area!(source.clone(), start_node.span),
                    ))
        
                },
                Err(e) => {
                    ret!(Err( RuntimeError::ErrorParsingImport {
                        import_area: area!(source.clone(), start_node.span),
                        error: e,
                    } ))
                },
            }

        },
        NodeType::Export { name, value } => {
            // println!("a: {:?}", globals.get_scope(scope_id).vars);
            let val = execute!(value => scope_id);
            // println!("b: {:?}", globals.get_scope(scope_id).vars);
            globals.exports.last_mut().unwrap().insert(name.clone(), val);
            Ok( val )
        },
        NodeType::Call { base, args } => {
            let base_id = protecute!(base => scope_id);
            
            match &globals.get(base_id).value.clone() {
                Value::Builtin(name) => run_builtin(
                    name_to_builtin(name),
                    start_node,
                    args,
                    scope_id,
                    globals,
                    source,
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
                                call_area: area!(source.clone(), start_node.span)
                            }
                        ))
                    }

                    let derived = globals.derive_scope(*parent_scope);
                    for (arg, (name, a, d)) in args.iter().zip(func_args) {
                        let arg_id = if name == "self" { protecute_raw!(arg => scope_id) } else { protecute!(arg => scope_id) };
                        
                        match d {
                            Some(typ) => {
                                let result = value_ops::is_op(&globals.get(arg_id).clone(), &globals.get(*typ).clone(), area!(source.clone(), node.span), globals)?;
                                if !result {
                                    ret!(Err( RuntimeError::PatternMismatch {
                                        typ: globals.get(arg_id).value.type_str(globals),
                                        pattern: match &globals.get(*typ).value {
                                            Value::Type(t) => t.to_str(globals),
                                            _ => unreachable!(),
                                        },
                                        pattern_area: globals.get(*typ).def_area.clone(),
                                        type_area: globals.get(arg_id).def_area.clone(),
                                    } ))
                                }
                            },
                            None => (),
                        }
                        let arg_id = proteclone!(arg_id => a.clone());
                        globals.set_var(derived, name.clone(), arg_id);
                    }
                    
                    globals.trace.push( area!(source.clone(), start_node.span) );
                    let ret_id = execute!(code => derived);
                    match globals.exits.last() {
                        Some(
                            Exit::Return(v, _)
                        ) => {
                            let ret_id = *v;
                            globals.exits.pop();
                            ret!( Ok( ret_id ) )
                        },
                        Some(
                            Exit::Break(_, span)
                        ) => {
                            let bruh = span.clone();
                            ret!(Err(
                                RuntimeError::BreakUsedOutside {
                                    break_area: area!(source.clone(), bruh),
                                    outer_area: area!(source.clone(), code.span),
                                }
                            ))
                        },
                        None => (),
                    }
                    globals.trace.pop();
                    globals.redef_value(ret_id, area!(source.clone(), start_node.span));
                    Ok( ret_id )
                },

                Value::Type(_) => {
                    if args.len() != 1 {
                        ret!(Err(
                            RuntimeError::TypeArgCount {
                                provided: args.len(),
                                call_area: area!(source.clone(), start_node.span)
                            }
                        ))
                    }
                    let arg_id = execute!(&args[0] => scope_id);
                    let result = value_ops::convert(&globals.get(arg_id).clone(), &globals.get(base_id).clone(), area!(source.clone(), node.span), globals)?;
                    Ok(globals.insert_value(
                        result,
                        area!(source.clone(), node.span),
                    ))
                }
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "function, builtin or type".to_string(),
                    found: format!("{}", other.type_str(globals)),
                    area: area!(source.clone(), base.span),
                    defs: vec![(other.type_str(globals), globals.get(base_id).def_area.clone())],
                } ))
            }


        }
    };

    globals.pop_protected();

    exec_result

}


