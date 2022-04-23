


use std::{collections::{HashMap, HashSet}, fs, path::PathBuf};
use slotmap::{SlotMap, new_key_type};

use convert_case::{Case, Casing};
use fnv::FnvHashMap;
use logos::Logos;

use crate::{value::{Value, value_ops, Function, ValueType, Pattern, McVector, InstanceVariant, Range, Selector, ValueIter, FuncArg}, parser::{ASTNode, NodeType, BlockType, MatchArm, VariantType, check_selector_type, Located}, EmeraldSource, error::RuntimeError, lexer::{Token, self}, CodeArea, builtins::{BuiltinType, builtin_type_from_str, BUILTIN_NAMES, BUILTIN_TYPE_NAMES}};
use crate::builtins::{run_builtin, name_to_builtin};

#[derive(Debug, Clone)]
pub enum Exit {
    Return(ValuePos, (usize, usize)),
    Break(ValuePos, (usize, usize)),
    Continue((usize, usize)),
}


new_key_type! { pub struct ValuePos; }
new_key_type! { pub struct ScopePos; }
new_key_type! { pub struct TypePos; }
pub type McFuncID = usize;



#[derive(Debug, Clone)]
pub struct StoredValue {
    pub value: Value,
    pub def_area: CodeArea,
}

#[derive(Debug, Clone)]
pub struct CustomStruct {
    pub name: String,
    pub fields: FnvHashMap<String, (ASTNode, Option<ASTNode>)>,
    pub field_areas: FnvHashMap<String, CodeArea>,
    pub def_area: CodeArea,
    pub def_scope: ScopePos,
}


#[derive(Debug, Clone)]
pub struct CustomEnum {
    pub name: String,
    pub variants: FnvHashMap<String, VariantType>,
    pub variant_areas: FnvHashMap<String, CodeArea>,
    pub def_area: CodeArea,
    pub def_scope: ScopePos,
}


#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub def_area: CodeArea,
}



#[derive(Debug, Clone)]
pub struct ImplData {
    pub members: FnvHashMap<String, ValuePos>,
    pub methods: Vec<String>,
}

impl ImplData {
    pub fn new() -> Self {
        ImplData {
            members: FnvHashMap::default(),
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
        for (i, _) in &globals.values {
            marked_values.insert(i);
        }
        for (i, _) in &globals.scopes {
            marked_scopes.insert(i);
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



#[derive(Debug)]
pub struct Register<K, V> {
    pub counter: K,
    pub map: HashMap<K, V>,
}



pub struct Globals {
    pub values: SlotMap<ValuePos, StoredValue>,
    pub scopes: SlotMap<ScopePos, Scope>,

    pub custom_structs: SlotMap<TypePos, CustomStruct>,
    pub custom_enums: SlotMap<TypePos, CustomEnum>,
    pub modules: SlotMap<TypePos, Module>,



    pub last_amount: usize,

    pub protected: Vec<Protector>,
    pub backup_protector: Option<Protector>,

    pub builtin_impls: HashMap<BuiltinType, ImplData>,
    pub struct_impls: HashMap<TypePos, ImplData>,
    pub enum_impls: HashMap<TypePos, ImplData>,
    pub module_impls: HashMap<TypePos, ImplData>,

    
    pub exits: Vec<Exit>,
    pub trace: Vec<CodeArea>,
    pub import_trace: Vec<CodeArea>,

    pub exports: Vec<FnvHashMap<String, ValuePos>>,
    pub import_caches: HashMap<PathBuf, FnvHashMap<String, ValuePos>>,

    pub mcfuncs: Vec<Vec<String>>,

}




pub struct Scope {
    pub vars: HashMap<String, ValuePos>,
    pub parent_id: Option<ScopePos>,
    pub extra_prot: Vec<ScopePos>,
    pub func_id: McFuncID,
}



impl Globals {

    pub fn new() -> (Self, ScopePos) {
        let mut scopes = SlotMap::with_key();
        let base_scope = scopes.insert(Scope { vars: HashMap::new(), parent_id: None, extra_prot: vec![], func_id: McFuncID::default() });
        (
            Self {
                values: SlotMap::with_key(),
                scopes,
                custom_structs: SlotMap::with_key(),
                custom_enums: SlotMap::with_key(),
                modules: SlotMap::with_key(),
                last_amount: 0,
                protected: vec![],
                backup_protector: None,
                // custom_types: vec![],
                struct_impls: HashMap::new(),
                enum_impls: HashMap::new(),
                builtin_impls: HashMap::new(),
                module_impls: HashMap::new(),
                exits: vec![],
                trace: vec![],
                import_trace: vec![],
                exports: vec![],
                import_caches: HashMap::new(),
                mcfuncs: vec![],
            },
            base_scope,
        )
    }

    pub fn init_global(&mut self, scope_id: ScopePos, source: EmeraldSource) {
        for i in BUILTIN_NAMES {
            let id = self.insert_value(
                Value::Builtin(name_to_builtin(i)),
                CodeArea {
                    source: source.clone(),
                    range: (0, 0)
                },
            );
            self.set_var(scope_id, i.to_string(), id);
        }

        for i in BUILTIN_TYPE_NAMES {
            let id = self.insert_value(
                Value::Type(ValueType::Builtin(builtin_type_from_str(i))),
                CodeArea {
                    source: source.clone(),
                    range: (0, 0)
                },
            );
            self.set_var(scope_id, i.to_case(Case::Snake).to_string(), id);
        }
    }


    fn push_protected(&mut self) {
        self.protected.push( Protector::new() );
    }
    fn pop_protected(&mut self) {
        self.backup_protector = self.protected.pop();
    }


    pub fn new_struct(
        &mut self,
        typ: CustomStruct,
    ) -> TypePos {
        self.custom_structs.insert( typ )
    }

    pub fn new_enum(
        &mut self,
        typ: CustomEnum,
    ) -> TypePos {
        self.custom_enums.insert( typ )
    }

    pub fn new_module(
        &mut self,
        typ: Module,
    ) -> TypePos {
        self.modules.insert( typ )
    }



    pub fn insert_value(
        &mut self,
        value: Value,
        def_area: CodeArea,
    ) -> ValuePos {
        self.values.insert( StoredValue {
            value,
            def_area,
        } )
    }
    pub fn set_value(
        &mut self,
        id: ValuePos,
        value: Value,
        def_area: Option<CodeArea>,
    ) {
        let stored_val = self.get_mut(id);
        stored_val.value = value;
        if let Some(a) = def_area {
            stored_val.def_area = a;
        }
    }
    pub fn redef_value(
        &mut self,
        id: ValuePos,
        def_area: CodeArea,
    ) {
        self.get_mut(id).def_area = def_area;
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
        return match self.values.get(id) {
            Some(v) => v,
            None => panic!("bad value fuck you"),
        }
    }
    pub fn get_mut(&mut self, id: ValuePos) -> &mut StoredValue {
        return match self.values.get_mut(id) {
            Some(v) => v,
            None => panic!("bad mutable value fuck you"),
        }
    }

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


    pub fn get_scope(&self, scope_id: ScopePos) -> &Scope {
        self.scopes.get(scope_id).unwrap()
    }
    pub fn get_scope_mut(&mut self, scope_id: ScopePos) -> &mut Scope {
        self.scopes.get_mut(scope_id).unwrap()
    }


    pub fn insert_scope(
        &mut self,
        scope: Scope,
    ) -> ScopePos {
        self.scopes.insert( scope )
    }
    fn derive_scope(&mut self, scope_id: ScopePos, mc_func_id: McFuncID) -> ScopePos {
        let pos = self.scopes.insert(Scope {
            vars: HashMap::new(),
            parent_id: Some(scope_id),
            extra_prot: vec![],
            func_id: mc_func_id,
        });
        self.protect_scope(pos);
        pos
    }
    fn derive_scope_mcfunc(&mut self, scope_id: ScopePos) -> ScopePos {
        self.mcfuncs.push(vec![]);

        let pos = self.scopes.insert( Scope {
            vars: HashMap::new(),
            parent_id: Some(scope_id),
            extra_prot: vec![],
            func_id: self.mcfuncs.len() - 1,
        });
        self.protect_scope(pos);
        pos
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

        for (_, s) in &self.custom_structs {
            if collector.marked_scopes.contains(&s.def_scope) {
                self.mark_scope(s.def_scope, &mut collector);
            }
        }
        for (_, s) in &self.custom_enums {
            if collector.marked_scopes.contains(&s.def_scope) {
                self.mark_scope(s.def_scope, &mut collector);
            }
        }

        for ImplData { members, .. } in self.struct_impls.values() {
            for v in members.values() {
                self.mark_value(*v, &mut collector);
            }
        }
        for ImplData { members, .. } in self.enum_impls.values() {
            for v in members.values() {
                self.mark_value(*v, &mut collector);
            }
        }
        for ImplData { members, .. } in self.builtin_impls.values() {
            for v in members.values() {
                self.mark_value(*v, &mut collector);
            }
        }
        for ImplData { members, .. } in self.module_impls.values() {
            for v in members.values() {
                self.mark_value(*v, &mut collector);
            }
        }
        for e in &self.exports {
            for v in e.values() {
                self.mark_value(*v, &mut collector);
            }
        }

        for map in self.import_caches.values() {
            for v in map.values() {
                self.mark_value(*v, &mut collector);
            }
        }

        for i in collector.marked_scopes {
            self.scopes.remove(i);
        }
        for i in collector.marked_values {
            self.values.remove(i);
        }

        self.last_amount = self.values.len();

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
        for id in self.get_scope(scope_id).vars.values() {
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
            if collector.marked_scopes.contains(i) { self.mark_scope(*i, collector) }
        }
    }


    pub fn insert_command(&mut self, func_id: McFuncID, command: String) {
        match self.mcfuncs.get_mut(func_id) {
            Some(v) => {v.push(command);},
            // None => {self.mcfuncs.push(func_id, vec![command]);},
            None => unreachable!(),
        };
    }

    pub fn get_impl_data(&self, id: ValuePos) -> Option<&ImplData> {
        match self.get(id).value.typ() {
            ValueType::Builtin(b) => self.builtin_impls.get(&b),
            ValueType::CustomStruct(id) => self.struct_impls.get(&id),
            ValueType::CustomEnum(id) => self.enum_impls.get(&id),
            ValueType::Module(id) => self.module_impls.get(&id),
        }
    }

    pub fn get_method(&self, id: ValuePos, name: &str) -> Option<Function> {
        match self.get_impl_data(id) {
            Some(d) => if d.methods.contains(&name.to_string()) {
                let m_id = d.members.get(&name.to_string()).unwrap();
                match self.get(*m_id).value.clone() {
                    Value::Function(f) => Some( f ),
                    _ => unreachable!(),
                }
            } else { None },
            None => None,
        }
    }





}


macro_rules! interpreter_util {
    ($globals:expr, $source:expr) => {
        #[allow(unused_macros)]
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
        #[allow(unused_macros)]
        macro_rules! execute_raw {
            ($node:expr => $scope_id:expr) => {
                {
                    execute($node, $scope_id, $globals, $source)?
                }
            }
        }
        #[allow(unused_macros)]
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
        #[allow(unused_macros)]
        macro_rules! protecute_raw {
            ($node:expr => $scope_id:expr) => {
                {
                    let id = execute($node, $scope_id, $globals, $source)?;
                    $globals.protect_value(id);
                    id
                }
            }
        }
        #[allow(unused_macros)]
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
        #[allow(unused_macros)]
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
        #[allow(unused_macros)]
        macro_rules! ret {
            ($thing:expr, Pop: $n:expr) => {
                {
                    for _ in 0..$n {
                        $globals.pop_protected();
                    }
                    return $thing;
                }
            };
        }
    };
}

macro_rules! area {
    ($source:expr, $area:expr) => {
        CodeArea {source: $source.clone(), range: $area}
    };
    ($source:expr, $start:expr, $end:expr) => {
        CodeArea {source: $source.clone(), range: ($start, $end)}
    };
}




pub fn do_assign(
    left: &ASTNode,
    right: ValuePos,
    scope_id: ScopePos,
    globals: &mut Globals,
    source: &EmeraldSource,
    list: &mut HashMap<(Option<ValuePos>, Option<String>), ValuePos>,
    is_declaration: bool,
) -> Result<(), RuntimeError> {
    interpreter_util!(globals, source);

    match &left.node {
        NodeType::Var { var_name } => {
            if !is_declaration {
                let left_id = protecute_raw!(left => scope_id);
                list.insert((Some(left_id), None), proteclone!(right => @));
            } else {
                list.insert((None, Some(var_name.clone())), proteclone!(right => @));
            }
        },
        NodeType::Index {..} | NodeType::Member { .. } if !is_declaration => {
            let left_id = protecute_raw!(left => scope_id);
            list.insert((Some(left_id), None), proteclone!(right => @));
        }
        NodeType::Array { elements } => {
            match &globals.get(right).value.clone() {
                Value::Array(arr) => {
                    if arr.len() != elements.len() {
                        return Err( RuntimeError::DestructureLengthMismatch {
                            for_type: "array".to_string(),
                            expected: elements.len(),
                            found: arr.len(),
                            area1: area!(source.clone(), left.span),
                            area2: globals.get(right).def_area.clone(),
                        } )
                    } else {
                        for (l, r) in elements.iter().zip(arr) {
                            do_assign(l, *r, scope_id, globals, source, list, is_declaration)?;
                        }
                    }
                },
                other => {
                    return Err( RuntimeError::DestructureTypeMismatch {
                        tried: "array".to_string(),
                        found: other.type_str(globals),
                        area1: area!(source.clone(), left.span),
                        area2: globals.get(right).def_area.clone(),
                    } )
                }
            }
        },
        NodeType::Tuple { elements } => {
            match &globals.get(right).value.clone() {
                Value::Tuple(arr) => {
                    if arr.len() != elements.len() {
                        return Err( RuntimeError::DestructureLengthMismatch {
                            for_type: "tuple".to_string(),
                            expected: elements.len(),
                            found: arr.len(),
                            area1: area!(source.clone(), left.span),
                            area2: globals.get(right).def_area.clone(),
                        } )
                    } else {
                        for (l, r) in elements.iter().zip(arr) {
                            do_assign(l, *r, scope_id, globals, source, list, is_declaration)?;
                        }
                    }
                },
                other => {
                    return Err( RuntimeError::DestructureTypeMismatch {
                        tried: "tuple".to_string(),
                        found: other.type_str(globals),
                        area1: area!(source.clone(), left.span),
                        area2: globals.get(right).def_area.clone(),
                    } )
                }
            }
        },
        NodeType::Option { inner } => {
            match &globals.get(right).value.clone() {
                Value::Option(val) => {

                    match (inner, val) {
                        (Some(i), Some(v)) => {
                            do_assign(i, *v, scope_id, globals, source, list, is_declaration)?;
                        },
                        (None, None) => (),
                        (i, v) => return Err( RuntimeError::DestructureOptionMismatch {
                            tried: if i.is_some() { "#(...)" } else { "#" }.to_string(),
                            found: if v.is_some() { "#(...)" } else { "#" }.to_string(),
                            area1: area!(source.clone(), left.span),
                            area2: globals.get(right).def_area.clone(),
                        } )
                    }
                },
                other => {
                    return Err( RuntimeError::DestructureTypeMismatch {
                        tried: "option".to_string(),
                        found: other.type_str(globals),
                        area1: area!(source.clone(), left.span),
                        area2: globals.get(right).def_area.clone(),
                    } )
                }
            }
        }
        NodeType::Dictionary { map } => {
            match &globals.get(right).value.clone() {
                Value::Dictionary(value_map) => {

                    for (k, v) in map {
                        if !value_map.contains_key(k) {
                            return Err( RuntimeError::DestructureNonExistentKeyField {
                                for_type: "dict".to_string(),
                                what: "key".to_string(),
                                name: k.clone(),
                                area1: area!(source.clone(), left.span),
                                area2: globals.get(right).def_area.clone(),
                            } )
                        }
                        do_assign(v, value_map[k], scope_id, globals, source, list, is_declaration)?;
                    }

                },
                other => {
                    return Err( RuntimeError::DestructureTypeMismatch {
                        tried: "dict".to_string(),
                        found: other.type_str(globals),
                        area1: area!(source.clone(), left.span),
                        area2: globals.get(right).def_area.clone(),
                    } )
                }
            }
        }
        NodeType::StructInstance {
            base,
            fields,
            ..
        } => {
            let base_id = protecute!(base => scope_id);
            match &globals.get(base_id).value.clone() {
                Value::Type(ValueType::CustomStruct(id)) => {
                    match &globals.get(right).value.clone() {
                        Value::StructInstance { struct_id, fields: value_fields } => {
                            if struct_id != id {
                                return Err( RuntimeError::DestructureTypeMismatch {
                                    tried: globals.custom_structs[*id].name.clone(),
                                    found: globals.custom_structs[*struct_id].name.clone(),
                                    area1: area!(source.clone(), left.span),
                                    area2: globals.get(right).def_area.clone(),
                                } )
                            }
                            
                            for (k, v) in fields {
                                if !value_fields.contains_key(k) {
                                    return Err( RuntimeError::DestructureNonExistentKeyField {
                                        for_type: globals.custom_structs[*id].name.clone(),
                                        what: "field".to_string(),
                                        name: k.clone(),
                                        area1: area!(source.clone(), left.span),
                                        area2: globals.get(right).def_area.clone(),
                                    } )
                                }
                                do_assign(v, value_fields[k], scope_id, globals, source, list, is_declaration)?;
                            }

                        },
                        other => {
                            return Err( RuntimeError::DestructureTypeMismatch {
                                tried: globals.custom_structs[*id].name.clone(),
                                found: other.type_str(globals),
                                area1: area!(source.clone(), left.span),
                                area2: globals.get(right).def_area.clone(),
                            } )
                        }
                    }
                },
                _ => ret!(Err( RuntimeError::InstanceNonStruct {
                    area: area!(source.clone(), base.span),
                } ), Pop: 1)
            }
        }
        NodeType::EnumInstance {
            base,
            variant_name,
            variant_area,
            variant,
        } => {
            let base_id = protecute!(base => scope_id);
            match &globals.get(base_id).value.clone() {
                Value::Type(ValueType::CustomEnum(id)) => {
                    match &globals.get(right).value.clone() {
                        Value::EnumInstance {
                            enum_id,
                            variant_name: value_variant_name,
                            variant: value_variant
                        } => {
                            if enum_id != id {
                                return Err( RuntimeError::DestructureTypeMismatch {
                                    tried: globals.custom_structs[*id].name.clone(),
                                    found: globals.custom_structs[*enum_id].name.clone(),
                                    area1: area!(source.clone(), left.span),
                                    area2: globals.get(right).def_area.clone(),
                                } )
                            }
                            if variant_name != value_variant_name {
                                return Err( RuntimeError::DestructureVariantMismatch {
                                    tried: variant_name.clone(),
                                    found: value_variant_name.clone(),
                                    area1: area!(source.clone(), left.span),
                                    area2: globals.get(right).def_area.clone(),
                                } )
                            }
                            
                            let enum_type = globals.custom_enums[*id].clone();

                            match variant {
                                VariantType::Unit => {
                                    match &enum_type.variants[variant_name] {
                                        VariantType::Tuple(_) => ret!(Err( RuntimeError::IncorrectVariantType {
                                            expected: "tuple".to_string(),
                                            found: "unit".to_string(),
                                            variant_name: variant_name.clone(),
                                            used: variant_area.clone(),
                                            variant_def: enum_type.variant_areas[variant_name].clone(),
                                        } ), Pop: 1),
                                        VariantType::Struct { .. } => ret!(Err( RuntimeError::IncorrectVariantType {
                                            expected: "struct".to_string(),
                                            found: "unit".to_string(),
                                            variant_name: variant_name.clone(),
                                            used: variant_area.clone(),
                                            variant_def: enum_type.variant_areas[variant_name].clone(),
                                        } ), Pop: 1),
                                        VariantType::Unit => (),
                                    }
                                },
                                VariantType::Tuple(_) => {
                                    match &enum_type.variants[variant_name] {
                                        VariantType::Unit => ret!(Err( RuntimeError::IncorrectVariantType {
                                            expected: "unit".to_string(),
                                            found: "tuple".to_string(),
                                            variant_name: variant_name.clone(),
                                            used: variant_area.clone(),
                                            variant_def: enum_type.variant_areas[variant_name].clone(),
                                        } ), Pop: 1),
                                        VariantType::Struct { .. } => ret!(Err( RuntimeError::IncorrectVariantType {
                                            expected: "struct".to_string(),
                                            found: "tuple".to_string(),
                                            variant_name: variant_name.clone(),
                                            used: variant_area.clone(),
                                            variant_def: enum_type.variant_areas[variant_name].clone(),
                                        } ), Pop: 1),
                                        VariantType::Tuple(_) => (),
                                    }
                                },
                                VariantType::Struct { .. } => {
                                    match &enum_type.variants[variant_name] {
                                        VariantType::Tuple(_) => ret!(Err( RuntimeError::IncorrectVariantType {
                                            expected: "tuple".to_string(),
                                            found: "struct".to_string(),
                                            variant_name: variant_name.clone(),
                                            used: variant_area.clone(),
                                            variant_def: enum_type.variant_areas[variant_name].clone(),
                                        } ), Pop: 1),
                                        VariantType::Unit { .. } => ret!(Err( RuntimeError::IncorrectVariantType {
                                            expected: "unit".to_string(),
                                            found: "struct".to_string(),
                                            variant_name: variant_name.clone(),
                                            used: variant_area.clone(),
                                            variant_def: enum_type.variant_areas[variant_name].clone(),
                                        } ), Pop: 1),
                                        VariantType::Struct { .. } => (),
                                    }
                                },
                            }


                            match variant {
                                VariantType::Unit => {
                                    match value_variant {
                                        InstanceVariant::Tuple(_) => ret!(Err( RuntimeError::DestructureIncorrectVariantType  {
                                            expected: "unit".to_string(),
                                            found: "tuple".to_string(),
                                            expected_area: area!(source.clone(), left.span),
                                            found_area: globals.get(right).def_area.clone(),
                                        } ), Pop: 1),
                                        InstanceVariant::Struct { .. } => ret!(Err( RuntimeError::DestructureIncorrectVariantType  {
                                            expected: "unit".to_string(),
                                            found: "struct".to_string(),
                                            expected_area: area!(source.clone(), left.span),
                                            found_area: globals.get(right).def_area.clone(),
                                        } ), Pop: 1),
                                        InstanceVariant::Unit => (), // not much to destructure lol
                                    }
                                },
                                VariantType::Tuple(arr) => {
                                    match value_variant {
                                        InstanceVariant::Unit => ret!(Err( RuntimeError::DestructureIncorrectVariantType  {
                                            expected: "tuple".to_string(),
                                            found: "unit".to_string(),
                                            expected_area: area!(source.clone(), left.span),
                                            found_area: globals.get(right).def_area.clone(),
                                        } ), Pop: 1),
                                        InstanceVariant::Struct { .. } => ret!(Err( RuntimeError::DestructureIncorrectVariantType  {
                                            expected: "tuple".to_string(),
                                            found: "struct".to_string(),
                                            expected_area: area!(source.clone(), left.span),
                                            found_area: globals.get(right).def_area.clone(),
                                        } ), Pop: 1),
                                        InstanceVariant::Tuple(value_arr) => {
                                            if arr.len() != value_arr.len() {
                                                return Err( RuntimeError::DestructureLengthMismatch {
                                                    for_type: "tuple variant".to_string(),
                                                    expected: arr.len(),
                                                    found: value_arr.len(),
                                                    area1: area!(source.clone(), left.span),
                                                    area2: globals.get(right).def_area.clone(),
                                                } )
                                            } else {
                                                for (l, r) in arr.iter().zip(value_arr) {
                                                    do_assign(l, *r, scope_id, globals, source, list, is_declaration)?;
                                                }
                                            }
                                        },
                                    }
                                },
                                VariantType::Struct { fields, .. } => {
                                    match value_variant {
                                        InstanceVariant::Unit => ret!(Err( RuntimeError::DestructureIncorrectVariantType  {
                                            expected: "struct".to_string(),
                                            found: "unit".to_string(),
                                            expected_area: area!(source.clone(), left.span),
                                            found_area: globals.get(right).def_area.clone(),
                                        } ), Pop: 1),
                                        InstanceVariant::Tuple(_) => ret!(Err( RuntimeError::DestructureIncorrectVariantType  {
                                            expected: "struct".to_string(),
                                            found: "tuple".to_string(),
                                            expected_area: area!(source.clone(), left.span),
                                            found_area: globals.get(right).def_area.clone(),
                                        } ), Pop: 1),
                                        InstanceVariant::Struct { fields: value_fields } => {
                                            for (k, v) in fields {
                                                if !value_fields.contains_key(k) {
                                                    return Err( RuntimeError::DestructureNonExistentKeyField {
                                                        for_type: globals.custom_enums[*id].name.clone(),
                                                        what: "struct variant field".to_string(),
                                                        name: k.clone(),
                                                        area1: area!(source.clone(), left.span),
                                                        area2: globals.get(right).def_area.clone(),
                                                    } )
                                                }
                                                do_assign(v, value_fields[k], scope_id, globals, source, list, is_declaration)?;
                                            }
                                        },
                                    }
                                },
                            }

                        },
                        other => {
                            return Err( RuntimeError::DestructureTypeMismatch {
                                tried: globals.custom_enums[*id].name.clone(),
                                found: other.type_str(globals),
                                area1: area!(source.clone(), left.span),
                                area2: globals.get(right).def_area.clone(),
                            } )
                        }
                    }
                },
                _ => ret!(Err( RuntimeError::InstanceNonEnum {
                    area: area!(source.clone(), base.span),
                } ), Pop: 1)
            }
        }
        _ => {
            let left_id = protecute!(left => scope_id);
            let equal = value_ops::eq(&globals.get(left_id).clone(), &globals.get(right).clone(), globals);
            if !equal {
                let value1 = globals.get(left_id).value.to_str(globals, &mut vec![]);
                let value2 = globals.get(right).value.to_str(globals, &mut vec![]);
                return Err(
                    RuntimeError::EqualAssertionFailed {
                        value1, value2,
                        area1: area!(source.clone(), left.span),
                        area2: globals.get(right).def_area.clone(),
                    }
                )
            }
        },
    }


    Ok(())

}



fn run_func(
    func: &Function,
    arg_ids: Vec<ValuePos>,
    globals: &mut Globals,
    scope_id: ScopePos,
    source: &EmeraldSource,
    start_node: &ASTNode,
) -> Result<ValuePos, RuntimeError> {
    interpreter_util!(globals, source);

    if func.args.len() != arg_ids.len() {
        return Err(
            RuntimeError::IncorrectArgumentCount {
                provided: arg_ids.len(),
                takes: func.args.len(),
                header_area: func.header_area.clone(),
                call_area: area!(source.clone(), start_node.span)
            }
        )
    }

    let derived = globals.derive_scope(func.parent_scope, globals.get_scope(scope_id).func_id);
    for (arg_id, FuncArg {
        name: Located {inner, ..},
        pattern: t,
        ..
    }) in arg_ids.iter().zip(func.args.clone()) {
        
        match t {
            Some(Located {inner: pat, area} ) => {
                let result = globals.get(*arg_id).value.matches_pat(&pat, globals);
                if !result {
                    return Err( RuntimeError::PatternMismatch {
                        typ: globals.get(*arg_id).value.type_str(globals),
                        pattern: pat.to_str(globals),
                        pattern_area: area,
                        type_area: globals.get(*arg_id).def_area.clone(),
                    } )
                }
            },
            None => (),
        }
        globals.set_var(derived, inner.clone(), *arg_id);
    }
    
    globals.trace.push( area!(source.clone(), start_node.span) );
    let ret_id = protecute!(&func.code => derived);

    if let Some(Located {inner: pat, area} ) = &func.ret_pattern {
        let matches = globals.get(ret_id).value.matches_pat(pat, globals);
        if !matches {
            return Err( RuntimeError::PatternMismatch {
                typ: globals.get(ret_id).value.type_str(globals),
                pattern: pat.to_str(globals),
                pattern_area: area.clone(),
                type_area: globals.get(ret_id).def_area.clone(),
            } )
        }
    }

    match globals.exits.last() {
        Some(
            Exit::Return(v, _)
        ) => {
            let ret_id = *v;
            globals.exits.pop();
            return Ok( ret_id )
        },
        Some(
            Exit::Break(_, span)
        ) => {
            let bruh = span;
            return Err(
                RuntimeError::BreakUsedOutside {
                    break_area: area!(source.clone(), *bruh),
                    outer_area: area!(source.clone(), func.code.span),
                }
            )
        },
        Some(
            Exit::Continue(span)
        ) => {
            let bruh = span;
            return Err(
                RuntimeError::ContinueUsedOutside {
                    continue_area: area!(source.clone(), *bruh),
                    outer_area: area!(source.clone(), func.code.span),
                }
            )
        },
        None => (),
    }
    globals.trace.pop();
    globals.redef_value(ret_id, area!(source.clone(), start_node.span));
    Ok( ret_id )
}





pub fn execute(
    node: &ASTNode,
    scope_id: ScopePos,
    globals: &mut Globals,
    source: &EmeraldSource,
) -> Result<ValuePos, RuntimeError> {
    interpreter_util!(globals, source);

    let start_node = node;

    macro_rules! run_func {
        ($arg_ids:expr, $func:expr) => {
            {

                run_func($func, $arg_ids, globals, scope_id, source, start_node)?
                
            }
        }
    }
    
    if globals.values.len() > 50000 + globals.last_amount {
    // if true {
        globals.collect(scope_id);
        // println!("collect: {} {} {}", globals.values.len(), globals.scopes.len(), globals.protected.len());
    }

    globals.push_protected();
    
    let exec_result = match &node.node {
        NodeType::Value { value } => Ok(globals.insert_value(
            value.clone(),
            CodeArea {source: source.clone(), range: node.span},
        )),
        NodeType::Option { inner } => {
            match inner {
                Some(n) => {
                    let val = execute!(n => scope_id);
                    Ok(globals.insert_value(
                        Value::Option(Some(val)),
                        CodeArea {source: source.clone(), range: node.span},
                    ))
                },
                None => Ok(globals.insert_value(
                    Value::Option(None),
                    CodeArea {source: source.clone(), range: node.span},
                )),
            }
        }
        NodeType::OptionPattern { pattern } => {

            let pat_id = protecute!(pattern => scope_id);
            match &globals.get(pat_id).value.clone() {
                Value::Type(t) => {
                    Ok( globals.insert_value(
                        Value::Pattern(Pattern::Option(Box::new(Pattern::Type(t.clone())))),
                        area!(source.clone(), start_node.span)
                    ) )
                },
                Value::Pattern(p) => {
                    Ok( globals.insert_value(
                        Value::Pattern(Pattern::Option(Box::new(p.clone()))),
                        area!(source.clone(), start_node.span)
                    ) )
                },
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "type or pattern".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), pattern.span),
                    defs: vec![(other.type_str(globals), globals.get(pat_id).def_area.clone())],
                } ), Pop: 1)
            }
        }
        NodeType::Op { left, op, right } => {
            match op {
                Token::Plus |
                Token::Minus |
                Token::Mult |
                Token::Div |
                Token::Mod |
                Token::EuclMod |
                Token::Pow |
                Token::Eq |
                Token::NotEq |
                Token::Greater |
                Token::GreaterEq |
                Token::Lesser |
                Token::LesserEq |
                Token::As |
                Token::Is |
                Token::Pipe |
                Token::DoubleDot |
                Token::In => {
                    let left_raw = protecute_raw!(left => scope_id);
                    let right_raw = protecute_raw!(right => scope_id);

                    let left_clone = proteclone!(left_raw => @);
                    let right_clone = proteclone!(right_raw => @);

                    macro_rules! op_helper {
                        (
                            $(
                                $tok:ident: $name:ident Overload: $overload_name:ident
                            )+
                        ) => {
                            match op {
                                $(
                                    Token::$tok => {
                                        match globals.get_method(left_clone, &format!("_{}_", stringify!($overload_name))) {
                                            Some(f) => Ok( run_func!(vec![left_raw, right_clone], &f) ),
                                            None => match globals.get_method(right_clone, &format!("_r_{}_", stringify!($overload_name))) {
                                                Some(f) => Ok( run_func!(vec![right_raw, left_clone], &f) ),
                                                None => {
                                                    let result = value_ops::$name(
                                                        &globals.get(left_clone).clone(),
                                                        &globals.get(right_clone).clone(),
                                                        area!(source.clone(), node.span), globals
                                                    )?;
                                                    Ok(globals.insert_value(
                                                        result,
                                                        area!(source.clone(), node.span),
                                                    ))
                                                },
                                            }
                                        }
                                    }
                                )+
                                _ => unreachable!(),
                            }
                        }
                    }

                    op_helper!(
                        Plus: plus                  Overload: plus
                        Minus: minus                Overload: minus
                        Mult: mult                  Overload: mult
                        Div: div                    Overload: div
                        Mod: modulo                 Overload: mod
                        EuclMod: eucl_modulo        Overload: eucl_mod
                        Pow: pow                    Overload: pow
                        Eq: eq_op                   Overload: eq
                        NotEq: neq_op               Overload: neq
                        Greater: greater            Overload: greater
                        GreaterEq: greater_eq       Overload: greater_eq
                        Lesser: lesser              Overload: lesser
                        LesserEq: lesser_eq         Overload: lesser_eq
                        As: convert                 Overload: as
                        Is: is_op                   Overload: is
                        Pipe: either                Overload: either
                        DoubleDot: range            Overload: range
                        In: in_op                   Overload: in
                    )
            
                }
                Token::PlusEq |
                Token::MinusEq |
                Token::MultEq |
                Token::DivEq |
                Token::ModEq |
                Token::EuclModEq |
                Token::PowEq => {
                    let left_raw = protecute_raw!(left => scope_id);
                    let right_raw = protecute_raw!(right => scope_id);

                    let left_clone = proteclone!(left_raw => @);
                    let right_clone = proteclone!(right_raw => @);


                    macro_rules! op_helper {
                        (
                            $(
                                $tok:ident: $name:ident Overload: $overload_name:ident
                            )+
                        ) => {
                            match op {
                                $(
                                    Token::$tok => {
                                        match globals.get_method(left_clone, &format!("_{}_", stringify!($overload_name))) {
                                            Some(f) => Ok( run_func!(vec![left_raw, right_clone], &f) ),
                                            None => match globals.get_method(right_clone, &format!("_r_{}_", stringify!($overload_name))) {
                                                Some(f) => Ok( run_func!(vec![right_raw, left_clone], &f) ),
                                                None => {
                                                    let result = value_ops::$name(&globals.get(left_clone).clone(), &globals.get(right_clone).clone(), area!(source.clone(), node.span), globals)?;
                                                    let result_id = globals.insert_value(
                                                        result,
                                                        area!(source.clone(), node.span),
                                                    );
                                                    globals.set_value(
                                                        left_raw,
                                                        globals.get(result_id).value.clone(),
                                                        Some(area!(source.clone(), start_node.span))
                                                    );
                                                    Ok(result_id)
                                                },
                                            }
                                        }
                                    }
                                )+
                                _ => unreachable!(),
                            }
                        }
                    }


                    op_helper!(
                        PlusEq: plus                Overload: plus_eq
                        MinusEq: minus              Overload: minus_eq
                        MultEq: mult                Overload: mult_eq
                        DivEq: div                  Overload: div_eq
                        ModEq: modulo               Overload: mod_eq
                        EuclModEq: eucl_modulo      Overload: eucl_mod_eq
                        PowEq: pow                  Overload: pow_eq
                        // Assign: overwrite           Overload: assign
                    )
                }
                Token::Assign => {
                    let right_raw = protecute_raw!(right => scope_id);

                    let mut list = HashMap::new();
                    do_assign(left, right_raw, scope_id, globals, source, &mut list, false)?;

                    for (l, r) in list {
                        let left_clone = proteclone!(l.0.unwrap() => @);
                        let right_clone = proteclone!(r => @);

                        match globals.get_method(left_clone, "_assign_") {
                            Some(f) => {run_func!(vec![l.0.unwrap(), right_clone], &f);},
                            None => match globals.get_method(right_clone, "_r_assign_") {
                                Some(f) => {run_func!(vec![r, left_clone], &f);},
                                None => {
                                    globals.set_value(
                                        l.0.unwrap(),
                                        globals.get(right_clone).value.clone(),
                                        Some(area!(source.clone(), start_node.span))
                                    );
                                },
                            }
                        }
                    }
                    
                    Ok(proteclone!(right_raw => @))
                }
                Token::And => {
                    let left_raw = protecute_raw!(left => scope_id);
                    let left_clone = proteclone!(left_raw => @);

                    match globals.get_method(left_clone, "_and_") {
                        Some(f) => {
                            let right_id = protecute!(right => scope_id);
                            Ok( run_func!(vec![left_raw, right_id], &f) )
                        },
                        None => {
                            if !value_ops::to_bool(globals.get(left_clone), area!(source.clone(), left.span), globals)? {
                                Ok(globals.insert_value(
                                    Value::Boolean(false),
                                    area!(source.clone(), node.span),
                                ))
                            } else {
                                let right_raw = protecute_raw!(right => scope_id);
                                let right_clone = proteclone!(right_raw => @);
                                match globals.get_method(right_clone, "_r_and_") {
                                    Some(f) => Ok( run_func!(vec![right_raw, left_clone], &f) ),
                                    None => {
                                        if !value_ops::to_bool(globals.get(right_clone), area!(source.clone(), right.span), globals)? {
                                            Ok(globals.insert_value(
                                                Value::Boolean(false),
                                                area!(source.clone(), node.span),
                                            ))
                                        } else {
                                            Ok(globals.insert_value(
                                                Value::Boolean(true),
                                                area!(source.clone(), node.span),
                                            ))
                                        }
                                    },
                                }
                            }
                        }
                    }

                }
                Token::Or => {
                    let left_raw = protecute_raw!(left => scope_id);
                    let left_clone = proteclone!(left_raw => @);

                    match globals.get_method(left_clone, "_or_") {
                        Some(f) => {
                            let right_id = protecute!(right => scope_id);
                            Ok( run_func!(vec![left_raw, right_id], &f) )
                        },
                        None => {
                            if value_ops::to_bool(globals.get(left_clone), area!(source.clone(), left.span), globals)? {
                                Ok(globals.insert_value(
                                    Value::Boolean(true),
                                    area!(source.clone(), node.span),
                                ))
                            } else {
                                let right_raw = protecute_raw!(right => scope_id);
                                let right_clone = proteclone!(right_raw => @);
                                match globals.get_method(right_clone, "_r_or_") {
                                    Some(f) => Ok( run_func!(vec![right_raw, left_clone], &f) ),
                                    None => {
                                        if value_ops::to_bool(globals.get(right_clone), area!(source.clone(), right.span), globals)? {
                                            Ok(globals.insert_value(
                                                Value::Boolean(true),
                                                area!(source.clone(), node.span),
                                            ))
                                        } else {
                                            Ok(globals.insert_value(
                                                Value::Boolean(false),
                                                area!(source.clone(), node.span),
                                            ))
                                        }
                                    },
                                }
                            }
                        }
                    }

                }
                _ => unreachable!()
            }
        },
        NodeType::Unary { op, node } => {
            // let value = protecute!(node => scope_id);
            let value_raw = protecute_raw!(node => scope_id);

            let value_clone = proteclone!(value_raw => @);

            macro_rules! op_helper {
                (
                    $(
                        $tok:ident: $name:ident Overload: $overload_name:ident
                    )+
                ) => {
                    match op {
                        $(
                            Token::$tok => {
                                match globals.get_method(value_clone, &format!("_{}_", stringify!($overload_name))) {
                                    Some(f) => Ok( run_func!(vec![value_raw], &f) ),
                                    None => {
                                        let result = value_ops::$name(&globals.get(value_clone).clone(), area!(source.clone(), node.span), globals)?;
                                        Ok(globals.insert_value(
                                            result,
                                            area!(source.clone(), start_node.span),
                                        ))
                                    },
                                }
                            }
                        )+
                        _ => unreachable!(),
                    }
                }
            }


            op_helper!(
                Plus: identity                          Overload: identity
                Minus: negate                           Overload: negate
                ExclMark: not                           Overload: not
                DoubleDot: unary_range                  Overload: unary_range
                TripleDot: unary_unbounded_range        Overload: unbounded_range
            )
        },
        NodeType::UnboundedRange { base } => {
            let val = protecute!(base => scope_id);
            let result = match &globals.get(val).value.clone() {
                Value::Number(n1) => Value::Range(Range{
                    start: Some(*n1),
                    end: None,
                    step: 1.0,
                }),
                Value::Range(Range{start, end: Some(e), step}) if *step == 1.0 => Value::Range(Range{
                    start: *start,
                    end: None,
                    step: *e,
                }),
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "number or range".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), base.span),
                    defs: vec![(other.type_str(globals), area!(source.clone(), base.span))],
                } ), Pop: 1)
            };
            Ok(globals.insert_value(
                result,
                area!(source.clone(), node.span),
            ))
        }
        NodeType::Declaration { left, right } => {

            let right_id = protecute!(right => scope_id);

            let mut list = HashMap::new();
            do_assign(left, right_id, scope_id, globals, source, &mut list, true)?;

            for (l, r) in list {
                let right_clone = proteclone!(r => @);
                
                globals.set_var(scope_id, l.1.unwrap(), right_clone);
            }
            
            Ok(proteclone!(right_id => @))
        }
        NodeType::Var { var_name } => {
            if var_name == "_" {
                Ok( globals.insert_value(
                    Value::Pattern(Pattern::Any),
                    area!(source.clone(), start_node.span)
                ) )
            } else {
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
        }
        NodeType::StatementList { statements } => {
            let mut ret_id = globals.insert_value(
                Value::unit(),
                area!(source.clone(), start_node.span)
            );
            for i in statements {
                ret_id = execute!(i => scope_id);
                if !globals.exits.is_empty() {
                    ret!( Ok( ret_id ), Pop: 1 );
                }
            }
            Ok(ret_id)
        }
        NodeType::Block { code, typ } => {
            let value = match typ {
                BlockType::Regular { .. } => execute!(code => globals.derive_scope(scope_id, globals.get_scope(scope_id).func_id)),
                BlockType::McFunc => {
                    let derived = globals.derive_scope_mcfunc(scope_id);
                    globals.mcfuncs.push(vec![]);
                    execute!(code => derived);
                    globals.insert_value(
                        Value::McFunc(globals.mcfuncs.len()),
                        area!(source.clone(), node.span),
                    )
                },
            };
            globals.redef_value(value, area!(source.clone(), start_node.span));
            Ok( value )
        }
        NodeType::If { cond, code, else_branch } => {
            let cond_value = protecute!(cond => scope_id);
            let mut ret_id = globals.insert_value(
                Value::unit(),
                area!(source.clone(), start_node.span)
            );
            globals.protect_value(ret_id);

            match globals.get_method(cond_value, "_if_") {
                Some(f) => {
                    let code_f = protecute!( &ASTNode {
                        span: node.span,
                        node: NodeType::Lambda { args: vec![], header_area: area!(source.clone(), node.span), code: code.clone(), ret_pattern: None },
                    } => scope_id);
                    let else_f = match else_branch {
                        Some(b) => {
                            let id = protecute!( &ASTNode {
                                span: node.span,
                                node: NodeType::Lambda { args: vec![], header_area: area!(source.clone(), node.span), code: b.clone(), ret_pattern: None },
                            } => scope_id);
                            globals.insert_value(
                                Value::some(id),
                                area!(source.clone(), start_node.span)
                            )
                        },
                        None => globals.insert_value(
                            Value::none(),
                            area!(source.clone(), start_node.span)
                        ),
                    };
                    ret_id = run_func!(vec![cond_value, code_f, else_f], &f);
                },
                None => {
                    if value_ops::to_bool(globals.get(cond_value), area!(source.clone(), cond.span), globals)? {
                        ret_id = execute!(code => scope_id)
                    } else if let Some(b) = else_branch {
                        ret_id = execute!(b => scope_id)
                    }
                }
            }


            globals.redef_value(ret_id, area!(source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::While { cond, code } => {
            let mut ret_id = globals.insert_value(
                Value::unit(),
                area!(source.clone(), start_node.span)
            );
            globals.protect_value(ret_id);

            let cond_value = protecute!(cond => scope_id);
            match globals.get_method(cond_value, "_while_") {
                Some(f) => {
                    let code_f = protecute!( &ASTNode {
                        span: node.span,
                        node: NodeType::Lambda { args: vec![], header_area: area!(source.clone(), node.span), code: code.clone(), ret_pattern: None },
                    } => scope_id);
                    let cond_f = protecute!( &ASTNode {
                        span: node.span,
                        node: NodeType::Lambda { args: vec![], header_area: area!(source.clone(), node.span), code: cond.clone(), ret_pattern: None },
                    } => scope_id);
                    ret_id = run_func!(vec![cond_value, code_f, cond_f], &f);
                },
                None => {
                    let mut calculated = true;
                    loop {
                        globals.push_protected();
                        let cond_value = if calculated { cond_value } else { execute!(cond => scope_id) };
                        calculated = false;
                        if !(value_ops::to_bool(globals.get(cond_value), area!(source.clone(), cond.span), globals)?) {
                            break;
                        }
                        ret_id = protecute!(code => scope_id);
                        match globals.exits.last() {
                            Some(
                                Exit::Break(v, _)
                            ) => {
                                ret_id = *v;
                                globals.exits.pop();
                                ret!(Ok( ret_id ), Pop: 2);
                            },
                            Some(
                                Exit::Return(_, _)
                            ) => {
                                ret!(Ok( ret_id ), Pop: 2);
                            },
                            Some(
                                Exit::Continue(_)
                            ) => {
                                globals.exits.pop();
                            },
                            None => (),
                        }
                        globals.pop_protected();
                    }
                }
            }

            globals.redef_value(ret_id, area!(source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::Match { value, arms } => {
            let mut ret_id = globals.insert_value(
                Value::unit(),
                area!(source.clone(), start_node.span)
            );
            globals.protect_value(ret_id);
            let val_id = protecute!(value => scope_id);
            for (arm, then) in arms {
                match arm {
                    MatchArm::Pattern(pat) => {
                        let pat_id = protecute!(pat => scope_id);
                        let matches = value_ops::is_op_raw(&globals.get(val_id).clone(), &globals.get(pat_id).clone(), area!(source.clone(), node.span), globals)?;
                        if !matches {
                            continue;
                        } else {
                            let derived = globals.derive_scope(scope_id, globals.get_scope(scope_id).func_id);
                            ret_id = protecute!(then => derived);
                            break;
                        }
                    },
                    MatchArm::Structure(structure) => {
                        let mut list = HashMap::new();
                        let matches = do_assign(structure, val_id, scope_id, globals, source, &mut list, true);
                        match matches {
                            Ok(_) => {
                                let derived = globals.derive_scope(scope_id, globals.get_scope(scope_id).func_id);
                                for (l, r) in list.clone() {
                                    let right_clone = proteclone!(r => @);
                                    
                                    globals.set_var(derived, l.1.unwrap(), right_clone);
                                }
                                ret_id = protecute!(then => derived);
                                break;
                            },
                            Err(e) => {
                                if e.is_destructure_error() {
                                    globals.protected.push(globals.backup_protector.clone().unwrap());
                                    continue;
                                } else {
                                    ret!(Err( e ), Pop: 1)
                                }
                            },
                        }
                    },
                }
            }
            globals.redef_value(ret_id, area!(source.clone(), start_node.span));
            Ok( ret_id )
        }
        NodeType::For { left, code, iter } => {

            let mut ret_id = globals.insert_value(
                Value::unit(),
                area!(source.clone(), start_node.span)
            );
            globals.protect_value(ret_id);

            let iter_id = protecute!(iter => scope_id);


            macro_rules! for_loop {
                ($i:expr) => {
                    globals.push_protected();
                    let derived = globals.derive_scope(scope_id, globals.get_scope(scope_id).func_id);
                    {
                        let temp = globals.insert_value(
                            $i,
                            area!(source.clone(), left.span),
                        );

                        let mut list = HashMap::new();
                        do_assign(left, temp, derived, globals, source, &mut list, true)?;

                        for (l, r) in list {
                            let right_clone = clone!(r => @);
                            
                            globals.set_var(derived, l.1.unwrap(), right_clone);
                        }

                    }
                    ret_id = execute!(code => derived);
                    match globals.exits.last() {
                        Some(
                            Exit::Break(v, _)
                        ) => {
                            ret_id = *v;
                            globals.exits.pop();
                            ret!(Ok( ret_id ), Pop: 2);
                        },
                        Some(
                            Exit::Return(_, _)
                        ) => {
                            ret!(Ok( ret_id ), Pop: 2);
                        },
                        Some(
                            Exit::Continue(_)
                        ) => {
                            globals.exits.pop();
                        },
                        None => (),
                    }
                    globals.pop_protected();
                }
            }


            match globals.get_method(iter_id, "_iter_") {
                Some(f) => {
                    let next_f = run_func!(vec![iter_id], &f);
                    let next_val = globals.get(next_f).value.clone();
                    match next_val {
                        Value::Function(f) => {
                            loop {
                                let next = run_func!(Vec::<ValuePos>::new(), &f);
                                match globals.get(next).value.clone() {
                                    Value::Option(o) => match o {
                                        Some(v) => {
                                            let val = globals.get(v).value.clone();
                                            for_loop!(val);
                                        },
                                        None => break,
                                    }
                                    other => ret!(Err( RuntimeError::TypeMismatch {
                                        expected: "option".to_string(),
                                        found: other.type_str(globals),
                                        area: globals.get(next).def_area.clone(),
                                        defs: vec![(other.type_str(globals), globals.get(next).def_area.clone())],
                                    } ), Pop: 1)
                                }
                            }
                        }
                        _ => {
                            for i in value_ops::iter(globals.get(next_f), globals.get(next_f).def_area.clone(), globals)? {
                                for_loop!(i);
                            }
                        }
                    }
                },
                None => {
                    for i in value_ops::iter(globals.get(iter_id), area!(source.clone(), iter.span), globals)? {
                        for_loop!(i);
                    }
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
                        ret!(Ok( ret_id ), Pop: 1);
                    },
                    Some(
                        Exit::Return(_, _)
                    ) => {
                        ret!(Ok( ret_id ), Pop: 1);
                    },
                    Some(
                        Exit::Continue(_)
                    ) => {
                        globals.exits.pop();
                    },
                    None => (),
                }
            }
        }
        NodeType::FuncDef { func_name, args, code, header_area, ret_pattern } => {
            let mut args_exec = vec![];
            for FuncArg {
                name: s,
                pattern: t,
                default: d,
                is_ref,
            } in args {
                let t = match t {
                    Some(n) => {
                        let n_id = execute!(n => scope_id);
                        let n_id = proteclone!( n_id => area!(source.clone(), n.span) );

                        let pat = match globals.get(n_id).value.clone() {
                            Value::Type(t) => Pattern::Type(t),
                            Value::Pattern(p) => p,
                            other => ret!(Err( RuntimeError::TypeMismatch {
                                expected: "type or pattern".to_string(),
                                found: other.type_str(globals),
                                area: area!(source.clone(), n.span),
                                defs: vec![(other.type_str(globals), globals.get(n_id).def_area.clone())],
                            } ), Pop: 1)
                        };

                        Some( Located {
                            inner: pat,
                            area: area!(source.clone(), n.span),
                        } )

                    }
                    None => None,
                };
                let d = match d {
                    Some(n) => {
                        let _n = execute!(n => scope_id);
                        Some( proteclone!( _n => area!(source.clone(), n.span) ) )
                    }
                    None => None,
                };
                args_exec.push( FuncArg {
                    name: s.clone(),
                    pattern: t,
                    default: d,
                    is_ref: *is_ref,
                } )
            }
            let ret_pattern = match ret_pattern {
                Some(r) => {
                    let r_id = execute!(r => scope_id);
                    let r_id = proteclone!( r_id => area!(source.clone(), r.span) );

                    let pat = match globals.get(r_id).value.clone() {
                        Value::Type(t) => Pattern::Type(t),
                        Value::Pattern(p) => p,
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "type or pattern".to_string(),
                            found: other.type_str(globals),
                            area: area!(source.clone(), r.span),
                            defs: vec![(other.type_str(globals), globals.get(r_id).def_area.clone())],
                        } ), Pop: 1)
                    };

                    Some( Located {
                        inner: pat,
                        area: area!(source.clone(), r.span),
                    } )

                }
                None => None,
            };
            let value_id = globals.insert_value(
                Value::Function( Function {
                    args: args_exec,
                    code: code.clone(),
                    parent_scope: scope_id,
                    header_area: header_area.clone(),
                    self_arg: None,
                    ret_pattern
                } ),
                area!(source.clone(), start_node.span)
            );
            globals.set_var(scope_id, func_name.to_string(), value_id);
            Ok( clone!(value_id => @) )
        }
        NodeType::Lambda { args, code, header_area, ret_pattern } => {
            let mut args_exec = vec![];
            for FuncArg {
                name: s,
                pattern: t,
                default: d,
                is_ref,
            } in args {
                let t = match t {
                    Some(n) => {
                        let n_id = execute!(n => scope_id);
                        let n_id = proteclone!( n_id => area!(source.clone(), n.span) );

                        let pat = match globals.get(n_id).value.clone() {
                            Value::Type(t) => Pattern::Type(t),
                            Value::Pattern(p) => p,
                            other => ret!(Err( RuntimeError::TypeMismatch {
                                expected: "type or pattern".to_string(),
                                found: other.type_str(globals),
                                area: area!(source.clone(), n.span),
                                defs: vec![(other.type_str(globals), globals.get(n_id).def_area.clone())],
                            } ), Pop: 1)
                        };

                        Some( Located {
                            inner: pat,
                            area: area!(source.clone(), n.span),
                        } )

                    }
                    None => None,
                };
                let d = match d {
                    Some(n) => {
                        let _n = execute!(n => scope_id);
                        Some( proteclone!( _n => area!(source.clone(), n.span) ) )
                    }
                    None => None,
                };
                args_exec.push( FuncArg {
                    name: s.clone(),
                    pattern: t,
                    default: d,
                    is_ref: *is_ref,
                } )
            }
            let ret_pattern = match ret_pattern {
                Some(r) => {
                    let r_id = execute!(r => scope_id);
                    let r_id = proteclone!( r_id => area!(source.clone(), r.span) );

                    let pat = match globals.get(r_id).value.clone() {
                        Value::Type(t) => Pattern::Type(t),
                        Value::Pattern(p) => p,
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "type or pattern".to_string(),
                            found: other.type_str(globals),
                            area: area!(source.clone(), r.span),
                            defs: vec![(other.type_str(globals), globals.get(r_id).def_area.clone())],
                        } ), Pop: 1)
                    };

                    Some( Located {
                        inner: pat,
                        area: area!(source.clone(), r.span),
                    } )

                }
                None => None,
            };
            Ok( globals.insert_value(
                Value::Function( Function {
                    args: args_exec,
                    code: code.clone(),
                    parent_scope: scope_id,
                    header_area: header_area.clone(),
                    self_arg: None,
                    ret_pattern
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
        NodeType::Tuple { elements } => {
            let mut ids = vec![];
            for i in elements {
                ids.push( protecute!(i => scope_id) );
            }
            Ok( globals.insert_value(
                Value::Tuple(ids),
                area!(source.clone(), start_node.span)
            ) )
        }
        NodeType::Dictionary { map } => {
            let mut id_map = FnvHashMap::default();
            for (k, v) in map {
                let id = protecute!(v => scope_id);
                id_map.insert( k.clone(), id );
            }
            Ok( globals.insert_value(
                Value::Dictionary(id_map),
                area!(source.clone(), start_node.span)
            ) )
        }
        NodeType::Index { base, args, args_area } => {


            
            
            let base_raw = protecute_raw!(base => scope_id);
            let base_clone = proteclone!(base_raw => @);

            let v = &globals.get(base_clone).value.clone();
            
            match globals.get_method(base_clone, "_index_") {
                Some(f) => Ok( {
                    let mut arg_ids = vec![];
                    for i in args {
                        arg_ids.push( protecute!(i => scope_id) );
                    }
                    let arr_value = globals.insert_value(
                        Value::Array(arg_ids),
                        area!(source.clone(), start_node.span)
                    );

                    run_func!(vec![base_raw, arr_value], &f)
                } ),
                None => match &globals.get(base_clone).value.clone() {
                    Value::Array(arr) | Value::Tuple(arr) => {
                        if args.len() != 1 {
                            ret!(Err(
                                RuntimeError::IndexArgCount {
                                    provided: args.len(),
                                    expected: "1".to_string(),
                                    index_area: args_area.clone(),
                                    val_area: area!(source.clone(), base.span),
                                    typ: v.type_str(globals),
                                }
                            ), Pop: 1)
                        }
                        let index = &args[0];
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
                                    } ), Pop: 1)
                                }
                            }
                            other => ret!(Err( RuntimeError::TypeMismatch {
                                expected: "number".to_string(),
                                found: other.type_str(globals),
                                area: area!(source.clone(), index.span),
                                defs: vec![(other.type_str(globals), globals.get(index_id).def_area.clone())],
                            } ), Pop: 1)
                        }
                    },
                    Value::Dictionary(map) => {
                        if args.len() != 1 {
                            ret!(Err(
                                RuntimeError::IndexArgCount {
                                    provided: args.len(),
                                    expected: "1".to_string(),
                                    index_area: area!(source.clone(), start_node.span),
                                    val_area: area!(source.clone(), base.span),
                                    typ: v.type_str(globals),
                                }
                            ), Pop: 1)
                        }
                        let index = &args[0];
                        let index_id = execute!(index => scope_id);
                        match &globals.get(index_id).value.clone() {
                            Value::String(s) => {
                                match map.get(s) {
                                    Some(i) => Ok(*i),
                                    None => ret!(Err( RuntimeError::NonexistentKey {
                                        key: s.clone(),
                                        area: area!(source.clone(), start_node.span),
                                    } ), Pop: 1)
                                }
                            }
                            other => ret!(Err( RuntimeError::TypeMismatch {
                                expected: "string".to_string(),
                                found: other.type_str(globals),
                                area: area!(source.clone(), index.span),
                                defs: vec![(other.type_str(globals), globals.get(index_id).def_area.clone())],
                            } ), Pop: 1)
                        }
                    },
                    Value::Type(ValueType::Builtin(BuiltinType::Array)) => {
                        match args.len() {
                            1 => {
                                let pat = &args[0];
                                let pat_id = protecute!(pat => scope_id);
                                match &globals.get(pat_id).value.clone() {
                                    Value::Type(t) => {
                                        Ok( globals.insert_value(
                                            Value::Pattern(Pattern::Array(Box::new(Pattern::Type(t.clone())), None)),
                                            area!(source.clone(), start_node.span)
                                        ) )
                                    },
                                    Value::Pattern(p) => {
                                        Ok( globals.insert_value(
                                            Value::Pattern(Pattern::Array(Box::new(p.clone()), None)),
                                            area!(source.clone(), start_node.span)
                                        ) )
                                    },
                                    other => ret!(Err( RuntimeError::TypeMismatch {
                                        expected: "type or pattern".to_string(),
                                        found: other.type_str(globals),
                                        area: area!(source.clone(), pat.span),
                                        defs: vec![(other.type_str(globals), globals.get(pat_id).def_area.clone())],
                                    } ), Pop: 1)
                                }
                            },
                            2 => {
                                let pat = &args[0];
                                let pat_id = protecute!(pat => scope_id);
                                let l = &args[1];
                                let l_id = protecute!(l => scope_id);
    
                                let pat = match &globals.get(pat_id).value.clone() {
                                    Value::Type(t) => {
                                        Box::new(Pattern::Type(t.clone()))
                                    },
                                    Value::Pattern(p) => {
                                        Box::new(p.clone())
                                    },
                                    other => ret!(Err( RuntimeError::TypeMismatch {
                                        expected: "type or pattern".to_string(),
                                        found: other.type_str(globals),
                                        area: area!(source.clone(), pat.span),
                                        defs: vec![(other.type_str(globals), globals.get(pat_id).def_area.clone())],
                                    } ), Pop: 1)
                                };
                                
                                let l = match &globals.get(l_id).value.clone() {
                                    Value::Number(n) => {
                                        let id = *n as isize;
                                        if id < 0 {
                                            ret!(Err( RuntimeError::CustomError {
                                                msg: "Cannot create array pattern with negative length".to_string(),
                                                area: area!(source.clone(), l.span),
                                                labels: vec![
                                                    ("This number is smaller than 0".to_string(), area!(source.clone(), l.span))
                                                ],
                                            } ), Pop: 1)
                                        }
                                        id as usize
                                    }
                                    other => ret!(Err( RuntimeError::TypeMismatch {
                                        expected: "number".to_string(),
                                        found: other.type_str(globals),
                                        area: area!(source.clone(), l.span),
                                        defs: vec![(other.type_str(globals), globals.get(l_id).def_area.clone())],
                                    } ), Pop: 1)
                                };
                                Ok( globals.insert_value(
                                    Value::Pattern(Pattern::Array(pat, Some(l))),
                                    area!(source.clone(), start_node.span)
                                ) )
                            },
                            l => ret!(Err(
                                RuntimeError::IndexArgCount {
                                    provided: l,
                                    expected: "1 or 2".to_string(),
                                    index_area: args_area.clone(),
                                    val_area: area!(source.clone(), base.span),
                                    typ: "array type".to_string(),
                                }
                            ), Pop: 1)
                        }
                    },
                    Value::Type(ValueType::Builtin(BuiltinType::Tuple)) => {
    
                        let mut v = vec![];
                        for i in args {
                            let pat = i;
                            let pat_id = protecute!(pat => scope_id);
                            match &globals.get(pat_id).value.clone() {
                                Value::Type(t) => v.push( Pattern::Type(t.clone()) ),
                                Value::Pattern(p) => v.push( p.clone() ),
                                other => ret!(Err( RuntimeError::TypeMismatch {
                                    expected: "type or pattern".to_string(),
                                    found: other.type_str(globals),
                                    area: area!(source.clone(), pat.span),
                                    defs: vec![(other.type_str(globals), globals.get(pat_id).def_area.clone())],
                                } ), Pop: 1)
                            }
                        }
    
                        Ok( globals.insert_value(
                            Value::Pattern(Pattern::Tuple(v)),
                            area!(source.clone(), start_node.span)
                        ) )
                    },
                    Value::Type(ValueType::Builtin(BuiltinType::Dict)) => {
                        match args.len() {
                            1 => {
                                let dict = &args[0];
                                let dict_id = protecute!(dict => scope_id);
    
                                match &globals.get(dict_id).value.clone() {
                                    Value::Dictionary(map) => {
                                        let mut p_map = FnvHashMap::default();
                                        for (k, i) in map {
                                            let entry = globals.get(*i);
                                            match &entry.value {
                                                Value::Type(t) => p_map.insert(k.clone(), Pattern::Type(t.clone())),
                                                Value::Pattern(p) => p_map.insert(k.clone(), p.clone()),
                                                _ => ret!(Err( RuntimeError::CustomError {
                                                    msg: "Dict pattern can only be created using a dict with type or pattern values".to_string(),
                                                    area: area!(source.clone(), dict.span),
                                                    labels: vec![
                                                        ("This dict doesn't have only type or pattern values".to_string(), area!(source.clone(), dict.span))
                                                    ],
                                                } ), Pop: 1)
                                            };
                                        }
                                        Ok( globals.insert_value(
                                            Value::Pattern(Pattern::Dict(p_map)),
                                            area!(source.clone(), start_node.span)
                                        ) )
                                    },
                                    other => ret!(Err( RuntimeError::TypeMismatch {
                                        expected: "dict".to_string(),
                                        found: other.type_str(globals),
                                        area: area!(source.clone(), dict.span),
                                        defs: vec![(other.type_str(globals), globals.get(dict_id).def_area.clone())],
                                    } ), Pop: 1)
                                }
                            },
                            l => ret!(Err(
                                RuntimeError::IndexArgCount {
                                    provided: l,
                                    expected: "1".to_string(),
                                    index_area: args_area.clone(),
                                    val_area: area!(source.clone(), base.span),
                                    typ: "dict type".to_string(),
                                }
                            ), Pop: 1)
                        }
                    },
                    other => ret!(Err( RuntimeError::TypeMismatch {
                        expected: "array, dict, or array type".to_string(),
                        found: other.type_str(globals),
                        area: area!(source.clone(), base.span),
                        defs: vec![(other.type_str(globals), globals.get(base_clone).def_area.clone())],
                    } ), Pop: 1)
                }
            }
            

        }
        NodeType::Member { base, member } => {
            let base_id = execute!(base => scope_id);
            let typ = globals.get(base_id).value.typ();
            if let Some(data) = match typ {
                ValueType::Builtin(b) => globals.builtin_impls.get(&b),
                ValueType::CustomStruct(id) => globals.struct_impls.get(&id),
                ValueType::CustomEnum(id) => globals.enum_impls.get(&id),
                ValueType::Module(id) => globals.module_impls.get(&id),
            } {
                if data.methods.contains(&member.inner) {
                    let func = *data.members.get(&member.inner).unwrap();
                    match &globals.get(func).value {
                        Value::Function(f) => {
                            let f_v = Value::Function( Function {
                                args: f.args.clone(),
                                code: f.code.clone(),
                                parent_scope: f.parent_scope,
                                header_area: f.header_area.clone(),
                                self_arg: Some(base.clone()),
                                ret_pattern: f.ret_pattern.clone(),
                            } );
                            let f_a = globals.get(func).def_area.clone();
                            ret!(Ok( globals.insert_value(
                                f_v,
                                f_a
                            ) ), Pop: 1)
                        },
                        _ => unreachable!(),
                    }
                }
            }
            match (&globals.get(base_id).value.clone(), &member.inner[..]) {
                (Value::Dictionary(map), member) => {
                    match map.get(member) {
                        Some(i) => ret!( Ok(*i), Pop: 1 ),
                        None => ret!(Err( RuntimeError::NonexistentKey {
                            key: member.to_string(),
                            area: area!(source.clone(), start_node.span),
                        } ), Pop: 1)
                    }
                }
                (s @Value::StructInstance { fields, .. }, member) => {
                    match fields.get(member) {
                        Some(i) => ret!( Ok(*i), Pop: 1 ),
                        None => ret!(Err( RuntimeError::NonexistentField {
                            field: member.to_string(),
                            type_str: s.type_str(globals),
                            val_area: area!(source.clone(), base.span),
                            area: area!(source.clone(), start_node.span),
                        } ), Pop: 1)
                    }
                }
                _ => (),
            }

            let val = match (&globals.get(base_id).value.clone(), &member.inner[..]) {
                (Value::String(s), "length") =>
                    Value::Number(s.chars().count() as f64),
                (Value::Array(arr), "length") =>
                    Value::Number(arr.len() as f64),

                (Value::Range(Range { start, .. }), "start") =>
                    if let Some(n) = start {
                        let id = globals.insert_value(
                            Value::Number(*n),
                            area!(source.clone(), base.span)
                        );
                        Value::some(id)
                    } else {Value::none()},
                (Value::Range(Range { step, .. }), "step") =>
                    Value::Number(*step as f64),
                (Value::Range(Range { end, .. }), "end") =>
                    if let Some(n) = end {
                        let id = globals.insert_value(
                            Value::Number(*n),
                            area!(source.clone(), base.span)
                        );
                        Value::some(id)
                    } else {Value::none()},

                (Value::McFunc(id), "str") => /* if globals.mcfuncs.map[id].len() != 1 { */
                    Value::String( format!("function mrld:gen{}", id) ),
                // } else {
                //     Value::String( globals.mcfuncs.map[id][0].clone() )
                // }
                
                (Value::McVector(
                    McVector::Pos(x, ..) |
                    McVector::WithRot(x, ..)
                ), "x") =>
                    Value::String( format!("{}{}", x.prefix(), x.get_inner().to_str(globals, &mut vec![])) ),
                (Value::McVector(
                    McVector::Pos(_, y, _) |
                    McVector::WithRot(_, y, _, _, _)
                ), "y") =>
                    Value::String( format!("{}{}", y.prefix(), y.get_inner().to_str(globals, &mut vec![])) ),
                (Value::McVector(
                    McVector::Pos(_, _, z) |
                    McVector::WithRot(_, _, z, _, _)
                ), "z") =>
                    Value::String( format!("{}{}", z.prefix(), z.get_inner().to_str(globals, &mut vec![])) ),

                    
                (Value::McVector(
                    McVector::WithRot(_, _, _, h, _)
                ), "h_rot") =>
                    Value::String( format!("{}{}", h.prefix(), h.get_inner().to_str(globals, &mut vec![])) ),
                (Value::McVector(
                    McVector::WithRot(_, _, _, _, v)
                ), "v_rot") =>
                    Value::String( format!("{}{}", v.prefix(), v.get_inner().to_str(globals, &mut vec![])) ),


                (Value::Selector(Selector { args, .. }), "args") => {
                    let mut out = vec![];
                    for (s, id) in args {
                        let s = globals.insert_value(Value::String(s.clone()), area!(source.clone(), start_node.span));
                        let v = globals.clone_value(*id, None);
                        let item = vec![s, v];
                        out.push( globals.insert_value(
                            Value::Tuple(item),
                            area!(source.clone(), start_node.span)
                        ) );
                    }
                    Value::Array(out)
                }

                (Value::Selector( Selector { selector_type, .. }), "selector_type") =>
                    Value::String( match selector_type {
                        lexer::SelectorType::Players => "a",
                        lexer::SelectorType::Entities => "e",
                        lexer::SelectorType::Nearest => "p",
                        lexer::SelectorType::Random => "r",
                        lexer::SelectorType::Executor => "s",
                    }.to_string() ),
                
                (other, _) => ret!(Err( RuntimeError::NonexistentField {
                    field: member.inner.clone(),
                    type_str: other.type_str(globals),
                    val_area: area!(source.clone(), base.span),
                    area: member.area.clone(),
                } ), Pop: 1),
            };

            Ok( globals.insert_value(
                val,
                area!(source.clone(), start_node.span)
            ) )
        }
        NodeType::Return { node: ret } => {
            let ret_value = match ret {
                Some(n) => execute!(n => scope_id),
                None => globals.insert_value(
                    Value::unit(),
                    area!(source.clone(), start_node.span)
                ),
            };
            globals.exits.push( Exit::Return(ret_value, node.span) );
            Ok( clone!(ret_value => @) )
        }
        NodeType::Break { node: ret } => {
            let ret_value = match ret {
                Some(n) => execute!(n => scope_id),
                None => globals.insert_value(
                    Value::unit(),
                    area!(source.clone(), start_node.span)
                ),
            };
            globals.exits.push( Exit::Break(ret_value, node.span) );
            Ok( clone!(ret_value => @) )
        }
        NodeType::Continue => {
            let ret_value = globals.insert_value(
                Value::unit(),
                area!(source.clone(), start_node.span)
            );
            globals.exits.push( Exit::Continue(node.span) );
            Ok( clone!(ret_value => @) )
        }
        NodeType::Propagate { base } => {
            let prop_value = execute!(base => scope_id);

            match globals.get_method(prop_value, "_try_") {
                Some(f) => Ok( {
                    run_func!(vec![prop_value], &f)
                } ),
                None => {
                    match &globals.get(prop_value).value.clone() {
                        Value::Option(o) => {
                            match o {
                                Some(v) => {
                                    Ok( clone!(*v => @) )
                                },
                                None => {
                                    globals.exits.push( Exit::Return(prop_value, node.span) );
                                    Ok( clone!(prop_value => @) )
                                }
                            }
                        }
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "option".to_string(),
                            found: other.type_str(globals),
                            area: area!(source, base.span),
                            defs: vec![(other.type_str(globals), area!(source, base.span))],
                        } ), Pop: 1)
                    }
                },
            }
        }
        NodeType::StructDef { struct_name, fields, field_areas, def_area } => {
            let type_id = globals.new_struct( CustomStruct {
                name: struct_name.clone(),
                fields: fields.clone(),
                def_area: def_area.clone(),
                field_areas: field_areas.clone(),
                def_scope: scope_id,
            });
            let value_id = globals.insert_value(
                Value::Type(ValueType::CustomStruct(type_id)),
                area!(source.clone(), start_node.span)
            );
            globals.set_var(scope_id, struct_name.to_string(), value_id);
            Ok( value_id )
        }
        NodeType::EnumDef {
            enum_name,
            variants,
            variant_areas,
            def_area,
        } => {
            let type_id = globals.new_enum( CustomEnum {
                name: enum_name.clone(),
                variants: variants.clone(),
                def_area: def_area.clone(),
                variant_areas: variant_areas.clone(),
                def_scope: scope_id,
            });
            let value_id = globals.insert_value(
                Value::Type(ValueType::CustomEnum(type_id)),
                area!(source.clone(), start_node.span)
            );
            globals.set_var(scope_id, enum_name.to_string(), value_id);
            Ok( value_id )
        }
        NodeType::ModuleDef { module_name, def_area } => {
            let type_id = globals.new_module( Module {
                name: module_name.clone(),
                def_area: def_area.clone(),
            });
            let value_id = globals.insert_value(
                Value::Type(ValueType::Module(type_id)),
                area!(source.clone(), start_node.span)
            );
            globals.set_var(scope_id, module_name.to_string(), value_id);
            Ok( value_id )
        }
        NodeType::StructInstance { fields, base, field_areas } => {
            let base_id = protecute!(base => scope_id);
            match &globals.get(base_id).value.clone() {
                Value::Type(ValueType::CustomStruct(id)) => {
                    let struct_type = globals.custom_structs[*id].clone();

                    let mut id_map = FnvHashMap::default();

                    for (f, (_, d)) in struct_type.fields.clone() {
                        match d {
                            Some(v) => { id_map.insert(f.clone(), protecute!(&v => struct_type.def_scope)); },
                            None => (),
                        }
                    }
                    for (k, v) in fields {
                        if !struct_type.fields.contains_key(k) {
                            ret!(Err( RuntimeError::NoStructField {
                                field_name: k.clone(),
                                used: field_areas[k].clone(),
                                struct_def: struct_type.def_area.clone(),
                            } ), Pop: 1)
                        }
                        let id = protecute!(v => scope_id);
                        id_map.insert( k.clone(), id );
                    }
                    for (k, v) in &id_map {
                        let typ = protecute!(&struct_type.fields[k].0 => struct_type.def_scope);
                        let result = value_ops::is_op_raw(&globals.get(*v).clone(), &globals.get(typ).clone(), area!(source.clone(), struct_type.fields[k].0.span), globals)?;
                        if !result {
                            ret!(Err( RuntimeError::PatternMismatch {
                                typ: globals.get(*v).value.type_str(globals),
                                pattern: match &globals.get(typ).value {
                                    Value::Type(t) => t.to_str(globals),
                                    Value::Pattern(p) => p.to_str(globals),
                                    _ => unreachable!(),
                                },
                                pattern_area: area!(source.clone(), struct_type.fields[k].0.span),
                                type_area: globals.get(*v).def_area.clone(),
                            } ), Pop: 1)
                        }
                    }
                    if id_map.len() != struct_type.fields.len() {
                        let mut missing = vec![];
                        for k in struct_type.fields.keys() {
                            if !id_map.contains_key(k) { missing.push(k.clone()) }
                        }
                        ret!(Err( RuntimeError::MissingStructFields {
                            fields: missing,
                            area: area!(source.clone(), start_node.span),
                            struct_def: struct_type.def_area.clone(),
                        } ), Pop: 1)
                    }
                    Ok( globals.insert_value(
                        Value::StructInstance { struct_id: *id, fields: id_map },
                        area!(source.clone(), start_node.span)
                    ) )
                },
                _ => ret!(Err( RuntimeError::InstanceNonStruct {
                    area: area!(source.clone(), base.span),
                } ), Pop: 1)
            }
        }
        NodeType::EnumInstance {
            base,
            variant_name,
            variant,
            variant_area,
        }  => {
            let base_id = protecute!(base => scope_id);
            match &globals.get(base_id).value.clone() {
                Value::Type(ValueType::CustomEnum(id)) => {
                    let enum_type = globals.custom_enums[*id].clone();

                    if !enum_type.variants.contains_key(variant_name) {
                        ret!(Err( RuntimeError::NonexistentVariant {
                            variant_name: variant_name.clone(),
                            used: variant_area.clone(),
                            enum_def: enum_type.def_area.clone(),
                        } ), Pop: 1)
                    }

                    match variant {
                        VariantType::Unit => {
                            match &enum_type.variants[variant_name] {
                                VariantType::Tuple(_) => ret!(Err( RuntimeError::IncorrectVariantType {
                                    expected: "tuple".to_string(),
                                    found: "unit".to_string(),
                                    variant_name: variant_name.clone(),
                                    used: variant_area.clone(),
                                    variant_def: enum_type.variant_areas[variant_name].clone(),
                                } ), Pop: 1),
                                VariantType::Struct { .. } => ret!(Err( RuntimeError::IncorrectVariantType {
                                    expected: "struct".to_string(),
                                    found: "unit".to_string(),
                                    variant_name: variant_name.clone(),
                                    used: variant_area.clone(),
                                    variant_def: enum_type.variant_areas[variant_name].clone(),
                                } ), Pop: 1),
                                VariantType::Unit => {
                                    Ok( globals.insert_value(
                                        Value::EnumInstance {
                                            enum_id: *id,
                                            variant_name: variant_name.clone(),
                                            variant: InstanceVariant::Unit,
                                        },
                                        area!(source.clone(), start_node.span)
                                    ) )
                                },
                            }
                        },
                        VariantType::Tuple(values) => {
                            match &enum_type.variants[variant_name] {
                                VariantType::Unit => ret!(Err( RuntimeError::IncorrectVariantType {
                                    expected: "unit".to_string(),
                                    found: "tuple".to_string(),
                                    variant_name: variant_name.clone(),
                                    used: variant_area.clone(),
                                    variant_def: enum_type.variant_areas[variant_name].clone(),
                                } ), Pop: 1),
                                VariantType::Struct { .. } => ret!(Err( RuntimeError::IncorrectVariantType {
                                    expected: "struct".to_string(),
                                    found: "tuple".to_string(),
                                    variant_name: variant_name.clone(),
                                    used: variant_area.clone(),
                                    variant_def: enum_type.variant_areas[variant_name].clone(),
                                } ), Pop: 1),
                                VariantType::Tuple(types) => {

                                    if types.len() != values.len() {
                                        ret!(Err( RuntimeError::TupleVariantNotEnoughArguments {
                                            expected: types.len(),
                                            found: values.len(),
                                            variant_name: variant_name.clone(),
                                            used: area!(source.clone(), start_node.span),
                                            variant_def: enum_type.variant_areas[variant_name].clone(),
                                        } ), Pop: 1)
                                    }

                                    let mut elems = vec![];

                                    for (v, t) in values.iter().zip(types) {
                                        let type_id = protecute!(t => enum_type.def_scope);
                                        let value_id = protecute!(v => scope_id);

                                        let result = value_ops::is_op_raw(&globals.get(value_id).clone(), &globals.get(type_id).clone(), area!(source.clone(), t.span), globals)?;
                                        if !result {
                                            ret!(Err( RuntimeError::PatternMismatch {
                                                typ: globals.get(value_id).value.type_str(globals),
                                                pattern: match &globals.get(type_id).value {
                                                    Value::Type(t) => t.to_str(globals),
                                                    Value::Pattern(p) => p.to_str(globals),
                                                    _ => unreachable!(),
                                                },
                                                pattern_area: area!(source.clone(), t.span),
                                                type_area: globals.get(value_id).def_area.clone(),
                                            } ), Pop: 1)
                                        }

                                        elems.push(value_id);
                                    }

                                    Ok( globals.insert_value(
                                        Value::EnumInstance {
                                            enum_id: *id,
                                            variant_name: variant_name.clone(),
                                            variant: InstanceVariant::Tuple(elems),
                                        },
                                        area!(source.clone(), start_node.span)
                                    ) )
                                },
                            }
                        },
                        VariantType::Struct {
                            fields,
                            field_areas,
                        } => {
                            match &enum_type.variants[variant_name] {
                                VariantType::Tuple(_) => ret!(Err( RuntimeError::IncorrectVariantType {
                                    expected: "tuple".to_string(),
                                    found: "struct".to_string(),
                                    variant_name: variant_name.clone(),
                                    used: variant_area.clone(),
                                    variant_def: enum_type.variant_areas[variant_name].clone(),
                                } ), Pop: 1),
                                VariantType::Unit { .. } => ret!(Err( RuntimeError::IncorrectVariantType {
                                    expected: "unit".to_string(),
                                    found: "struct".to_string(),
                                    variant_name: variant_name.clone(),
                                    used: variant_area.clone(),
                                    variant_def: enum_type.variant_areas[variant_name].clone(),
                                } ), Pop: 1),
                                VariantType::Struct {
                                    fields: field_types, ..
                                } => {
                                    let mut id_map = FnvHashMap::default();

                                    for (k, v) in fields {
                                        if !field_types.contains_key(k) {
                                            ret!(Err( RuntimeError::NoStructVariantField {
                                                variant_name: variant_name.clone(),
                                                field_name: k.clone(),
                                                used: field_areas[k].clone(),
                                                variant_def: enum_type.variant_areas[variant_name].clone(),
                                            } ), Pop: 1)
                                        }
                                        let id = protecute!(v => scope_id);
                                        id_map.insert( k.clone(), id );
                                    }

                                    for (k, v) in &id_map {
                                        let typ = protecute!(&field_types[k] => enum_type.def_scope);
                                        let result = value_ops::is_op_raw(&globals.get(*v).clone(), &globals.get(typ).clone(), area!(source.clone(), field_types[k].span), globals)?;
                                        if !result {
                                            ret!(Err( RuntimeError::PatternMismatch {
                                                typ: globals.get(*v).value.type_str(globals),
                                                pattern: match &globals.get(typ).value {
                                                    Value::Type(t) => t.to_str(globals),
                                                    Value::Pattern(p) => p.to_str(globals),
                                                    _ => unreachable!(),
                                                },
                                                pattern_area: area!(source.clone(), field_types[k].span),
                                                type_area: globals.get(*v).def_area.clone(),
                                            } ), Pop: 1)
                                        }
                                    }
                                    if id_map.len() != field_types.len() {
                                        let mut missing = vec![];
                                        for k in field_types.keys() {
                                            if !id_map.contains_key(k) { missing.push(k.clone()) }
                                        }
                                        ret!(Err( RuntimeError::MissingStructVariantFields {
                                            fields: missing,
                                            variant_name: variant_name.clone(),
                                            area: area!(source.clone(), start_node.span),
                                            variant_def: enum_type.variant_areas[variant_name].clone(),
                                        } ), Pop: 1)
                                    }
                                    Ok( globals.insert_value(
                                        Value::EnumInstance {
                                            enum_id: *id,
                                            variant_name: variant_name.clone(),
                                            variant: InstanceVariant::Struct{ fields: id_map },
                                        },
                                        area!(source.clone(), start_node.span)
                                    ) )
                                },
                            }
                        },
                    }
                },
                _ => ret!(Err( RuntimeError::InstanceNonEnum {
                    area: area!(source.clone(), base.span),
                } ), Pop: 1)
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
                                            if let Value::Function(
                                                Function { args, .. }
                                            ) = globals.get(temp).value.clone() {
                                                if let Some( FuncArg {
                                                    name: Located {inner, ..}, ..
                                                } ) = args.get(0) {
                                                    if inner == "self" { globals.builtin_impls.get_mut(b).unwrap().methods.push(k.clone()) }
                                                }
                                            }
                                        }
                                        // globals.builtin_impls.get_mut(b).unwrap().members.insert(k, v)
                                    },
                                    ValueType::CustomStruct(id) => {
                                        if !globals.struct_impls.contains_key(id) {
                                            globals.struct_impls.insert(*id, ImplData::new());
                                        }
                                        for (k, v) in fields {
                                            let temp = protecute!(v => scope_id);
                                            globals.struct_impls.get_mut(id).unwrap().members.insert( k.clone(), temp );
                                            if let Value::Function(
                                                Function { args, .. }
                                            ) = globals.get(temp).value.clone() {
                                                if let Some( FuncArg {
                                                    name: Located {inner, ..}, ..
                                                } ) = args.get(0) {
                                                    if inner == "self" { globals.struct_impls.get_mut(id).unwrap().methods.push(k.clone()) }
                                                }
                                            }
                                        }
                                        // globals.builtin_impls.get_mut(b).unwrap().members.insert(k, v)
                                    },
                                    ValueType::CustomEnum(id) => {
                                        if !globals.enum_impls.contains_key(id) {
                                            globals.enum_impls.insert(*id, ImplData::new());
                                        }
                                        for (k, v) in fields {
                                            let temp = protecute!(v => scope_id);
                                            globals.enum_impls.get_mut(id).unwrap().members.insert( k.clone(), temp );
                                            if let Value::Function(
                                                Function { args, .. }
                                            ) = globals.get(temp).value.clone() {
                                                if let Some( FuncArg {
                                                    name: Located {inner, ..}, ..
                                                } ) = args.get(0) {
                                                    if inner == "self" { globals.enum_impls.get_mut(id).unwrap().methods.push(k.clone()) }
                                                }
                                            }
                                        }
                                        // globals.builtin_impls.get_mut(b).unwrap().members.insert(k, v)
                                    },
                                    ValueType::Module(id) => {
                                        if !globals.module_impls.contains_key(id) {
                                            globals.module_impls.insert(*id, ImplData::new());
                                        }
                                        for (k, v) in fields {
                                            let temp = protecute!(v => scope_id);
                                            globals.module_impls.get_mut(id).unwrap().members.insert( k.clone(), temp );
                                            if let Value::Function(
                                                Function { args, .. }
                                            ) = globals.get(temp).value.clone() {
                                                if let Some( FuncArg {
                                                    name: Located {inner, ..}, ..
                                                } ) = args.get(0) {
                                                    if inner == "self" { globals.module_impls.get_mut(id).unwrap().methods.push(k.clone()) }
                                                }
                                            }
                                        }
                                        // globals.builtin_impls.get_mut(b).unwrap().members.insert(k, v)
                                    },
                                };
                                ret!(Ok(globals.insert_value(
                                    Value::unit(),
                                    area!(source.clone(), start_node.span)
                                )), Pop: 1)
                            },
                            other => {
                                let temp = globals.get(*id).def_area.clone();
                                ret!(Err( RuntimeError::TypeMismatch {
                                    expected: "type".to_string(),
                                    found: other.type_str(globals),
                                    area: name_area.clone(),
                                    defs: vec![(other.type_str(globals), temp)],
                                } ), Pop: 1)
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
                        ValueType::Builtin(b) => globals.builtin_impls.get(b),
                        ValueType::CustomStruct(id) => globals.struct_impls.get(id),
                        ValueType::CustomEnum(id) => globals.enum_impls.get(id),
                        ValueType::Module(id) => globals.module_impls.get(id),
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
                                } ), Pop: 1)
                            }
                        }
                    }
                }
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "type".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), base.span),
                    defs: vec![(other.type_str(globals), globals.get(base_id).def_area.clone())],
                } ), Pop: 1)
            }
        },
        NodeType::Import { path, cached } => {


            let mut buf = match &source {
                EmeraldSource::String(_) => ret!(Err( RuntimeError::CantImportInEval {
                    import_area: area!(source.clone(), start_node.span),
                } ), Pop: 1),
                EmeraldSource::File(f) => {
                    f.clone()
                },
            };
            
            buf.pop();
            buf.push(path);


            if *cached {
                match globals.import_caches.get(&buf) {
                    Some(cache) => {
                        let ret_value = Value::Dictionary( cache.clone() );
                        
                        ret!( Ok(globals.insert_value(
                            ret_value,
                            area!(source.clone(), start_node.span),
                        )), Pop: 1 );
                    },
                    None => (),
                }
            }

            let code = match fs::read_to_string(buf.clone()) {
                Ok(s) => s,
                Err(_) => ret!(Err( RuntimeError::NonexistentFile {
                    path: path.clone(),
                    area: area!(source.clone(), start_node.span),
                } ), Pop: 1),
            };

            let mut tokens_iter = lexer::Token
                ::lexer(&code);
            let mut tokens = vec![];
            while let Some(t) = tokens_iter.next() {
                tokens.push((
                    t,
                    (
                        tokens_iter.span().start,
                        tokens_iter.span().end,
                    )
                ))
            }
            tokens.push((
                lexer::Token::Eof,
                (code.len(), code.len())
            ));

            let mod_source = EmeraldSource::File(buf.clone());
            let ast = crate::parser::parse(&tokens, &mod_source);
            match ast {
                Ok((node, _)) => {
                    

                    let mod_scope_root = globals.insert_scope(Scope {
                        vars: HashMap::new(),
                        parent_id: None,
                        extra_prot: vec![scope_id],
                        func_id: globals.get_scope(scope_id).func_id,
                    });
                    globals.protect_scope(mod_scope_root);

                    globals.import_trace.push( area!(source.clone(), start_node.span) );
                    globals.exports.push( FnvHashMap::default() );
                    globals.init_global(mod_scope_root, mod_source.clone());

                    execute(
                        &node,
                        mod_scope_root,
                        globals,
                        &mod_source,
                    )?;

                    globals.import_trace.pop();

                    let ret_value = Value::Dictionary( globals.exports.last().unwrap().clone() );

                    globals.import_caches.insert(buf, globals.exports.pop().unwrap());
                    
                    Ok(globals.insert_value(
                        ret_value,
                        area!(source.clone(), start_node.span),
                    ))
        
                },
                Err(e) => {
                    ret!(Err( RuntimeError::ErrorParsingImport {
                        import_area: area!(source.clone(), start_node.span),
                        error: e,
                    } ), Pop: 1)
                },
            }

        },
        NodeType::Export { name, value } => {
            let val = execute!(value => scope_id);
            globals.exports.last_mut().unwrap().insert(name.clone(), val);
            Ok( val )
        },
        NodeType::Extract { value } => {
            let val = execute!(value => scope_id);
            match &globals.get(val).value.clone() {
                Value::Dictionary(map) => {
                    for (n, i) in map {
                        let _i = clone!(*i => @);
                        globals.set_var(scope_id, n.clone(), _i);
                    }
                },
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "dict".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), value.span),
                    defs: vec![(other.type_str(globals), globals.get(val).def_area.clone())],
                } ), Pop: 1)
            }
            Ok( clone!(val => @) )
        },
        NodeType::Call { base, args, .. } => {
            let base_raw = protecute_raw!(base => scope_id);
            let base_clone = proteclone!(base_raw => @);


            match globals.get_method(base_clone, "_call_") {
                Some(f) => Ok( {

                    let mut arg_ids = vec![];
                    for i in args {
                        arg_ids.push( protecute!(i => scope_id) );
                    }
                    let arr_value = globals.insert_value(
                        Value::Array(arg_ids),
                        area!(source.clone(), start_node.span)
                    );

                    run_func!(vec![base_raw, arr_value], &f)
                } ),
                None => {
                    match &globals.get(base_clone).value.clone() {
                        Value::Builtin(b) => run_builtin(
                            *b,
                            start_node,
                            args,
                            scope_id,
                            globals,
                            source,
                        ),
                        
                        Value::Function( f @ Function { args: params, self_arg, header_area, ..} ) => {
        
        
                            let mut args = args.clone();
                            if let Some(n) = self_arg {
                                args.insert(0, *n.clone());
                            }
        
                            if args.len() > params.len() {
                                ret!(Err(
                                    RuntimeError::IncorrectArgumentCount {
                                        provided: args.len(),
                                        takes: params.len(),
                                        header_area: header_area.clone(),
                                        call_area: area!(source.clone(), start_node.span)
                                    }
                                ), Pop: 1)
                            }
        
        
                            let mut arg_ids = vec![];
                            for (counter, FuncArg {
                                name: s,
                                default: d,
                                is_ref,
                                ..
                            }) in params.iter().enumerate() {
                                let arg_id = if counter >= args.len() {
                                    if let Some(d) = d {
                                        globals.clone_value(*d, None)
                                    } else {
                                        ret!(Err(
                                            RuntimeError::ArgumentNotProvided {
                                                arg_area: s.area.clone(),
                                                arg_name: s.inner.clone(),
                                                call_area: area!(source.clone(), start_node.span)
                                            }
                                        ), Pop: 1)
                                    }
                                } else {
                                    let arg = &args[counter];
                                    if s.inner == "self" || *is_ref { protecute_raw!(arg => scope_id) } else { protecute!(arg => scope_id) }
                                };
                                arg_ids.push( arg_id );
                            }
        
        
                            Ok( run_func!(arg_ids, f) )
                        },
        
                        Value::Type(_) => {
                            if args.len() != 1 {
                                ret!(Err(
                                    RuntimeError::TypeArgCount {
                                        provided: args.len(),
                                        call_area: area!(source.clone(), start_node.span)
                                    }
                                ), Pop: 1)
                            }
                            let arg_id = execute!(&args[0] => scope_id);
                            let result = value_ops::convert(&globals.get(arg_id).clone(), &globals.get(base_clone).clone(), area!(source.clone(), node.span), globals)?;
                            Ok(globals.insert_value(
                                result,
                                area!(source.clone(), node.span),
                            ))
                        }
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "function, builtin, or type".to_string(),
                            found: other.type_str(globals),
                            area: area!(source.clone(), base.span),
                            defs: vec![(other.type_str(globals), globals.get(base_clone).def_area.clone())],
                        } ), Pop: 1)
                    }
                },
            }
        },
        NodeType::MCCall { base } => {
            let base_id = protecute!(base => scope_id);
            
            match &globals.get(base_id).value.clone() {

                Value::McFunc(id) => {

                    let current_id = globals.get_scope(scope_id).func_id;

                    // if globals.mcfuncs.map[id].len() == 1 {
                    //     globals.insert_command(current_id, globals.mcfuncs.map[id][0].clone());
                    // } else {
                        globals.insert_command(current_id, format!("function mrld:gen{}", id));
                    // }

                    
                    Ok(globals.insert_value(
                        Value::unit(),
                        area!(source.clone(), node.span),
                    ))
                }
                Value::String(s) => {

                    let current_id = globals.get_scope(scope_id).func_id;

                    // if globals.mcfuncs.map[id].len() == 1 {
                    //     globals.insert_command(current_id, globals.mcfuncs.map[id][0].clone());
                    // } else {
                        globals.insert_command(current_id, s.to_string());
                    // }

                    
                    Ok(globals.insert_value(
                        Value::unit(),
                        area!(source.clone(), node.span),
                    ))
                }
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "mc_func or string".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), base.span),
                    defs: vec![(other.type_str(globals), globals.get(base_id).def_area.clone())],
                } ), Pop: 1)
            }


        },
        NodeType::CurrentMcId => {
            let val_id = globals.insert_value(
                Value::McFunc( globals.get_scope(scope_id).func_id ),
                area!(source.clone(), node.span),
            );
            Ok( val_id )
        },
        NodeType::McVector { x, y, z, rot } => {
            let x_val_id = execute!(x.get_inner() => scope_id); let x_val =  globals.get(x_val_id).value.clone();
            match x_val {
                Value::Number(_) => (),
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), x.get_inner().span),
                    defs: vec![(other.type_str(globals), globals.get(x_val_id).def_area.clone())],
                } ), Pop: 1),
            }
            let y_val_id = execute!(y.get_inner() => scope_id); let y_val =  globals.get(y_val_id).value.clone();
            match y_val {
                Value::Number(_) => (),
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), y.get_inner().span),
                    defs: vec![(other.type_str(globals), globals.get(y_val_id).def_area.clone())],
                } ), Pop: 1),
            }
            let z_val_id = execute!(z.get_inner() => scope_id); let z_val =  globals.get(z_val_id).value.clone();
            match z_val {
                Value::Number(_) => (),
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "number".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), z.get_inner().span),
                    defs: vec![(other.type_str(globals), globals.get(z_val_id).def_area.clone())],
                } ), Pop: 1),
            }

            let val_id = match rot {
                None => {
                    globals.insert_value(
                        Value::McVector(McVector::Pos(
                            x.reapply(Box::new(x_val)),
                            y.reapply(Box::new(y_val)),
                            z.reapply(Box::new(z_val))
                        )),
                        area!(source.clone(), start_node.span),
                    )
                },
                Some((h, v)) => {
                    
                    let h_val_id = execute!(h.get_inner() => scope_id); let h_val =  globals.get(h_val_id).value.clone();
                    match h_val {
                        Value::Number(_) => (),
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "number".to_string(),
                            found: other.type_str(globals),
                            area: area!(source.clone(), h.get_inner().span),
                            defs: vec![(other.type_str(globals), globals.get(h_val_id).def_area.clone())],
                        } ), Pop: 1),
                    }
                    let v_val_id = execute!(v.get_inner() => scope_id); let v_val =  globals.get(v_val_id).value.clone();
                    match v_val {
                        Value::Number(_) => (),
                        other => ret!(Err( RuntimeError::TypeMismatch {
                            expected: "number".to_string(),
                            found: other.type_str(globals),
                            area: area!(source.clone(), v.get_inner().span),
                            defs: vec![(other.type_str(globals), globals.get(v_val_id).def_area.clone())],
                        } ), Pop: 1),
                    }
                    globals.insert_value(
                        Value::McVector(McVector::WithRot(
                            x.reapply(Box::new(x_val)),
                            y.reapply(Box::new(y_val)),
                            z.reapply(Box::new(z_val)),
                            h.reapply(Box::new(h_val)),
                            v.reapply(Box::new(v_val))
                        )),
                        area!(source.clone(), start_node.span),
                    )
                },
            };
            Ok( val_id )
        },
        NodeType::Selector { selector_type, args } => {
            let mut arg_vec = vec![];
            for (s, a, v) in args {
                let id = protecute!(v => scope_id);
                let val = globals.get(id).value.clone();
                check_selector_type(s.clone(), globals, a.clone(), &val)?;
                arg_vec.push((s.clone(), id));
            }
            Ok( globals.insert_value(
                Value::Selector(Selector {
                    selector_type: selector_type.clone(),
                    args: arg_vec,
                }),
                area!(source.clone(), start_node.span)
            ) )
        },
        NodeType::Throw { msg } => {
            let msg_val_id = execute!(msg => scope_id);
            let msg_val =  globals.get(msg_val_id).value.clone();
            println!("a{:#?}", source);
            match msg_val {
                Value::String(s) => ret!(Err( RuntimeError::Throw {
                    area: area!(source.clone(), start_node.span),
                    msg: s,
                } ), Pop: 1),
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "string".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), msg.span),
                    defs: vec![(other.type_str(globals), globals.get(msg_val_id).def_area.clone())],
                } ), Pop: 1),
            }
        }
        NodeType::ListComp { expr, sources, cond } => {

            #[derive(Clone, Debug)]
            enum IterType {
                Regular(ValueIter),
                Custom(Function, ValuePos),
            }
            


            let mut iters = vec![];
            for (e, i) in sources {
                let iter_id = protecute!(i => scope_id);


                let iter = match globals.get_method(iter_id, "_iter_") {
                    Some(f) => IterType::Custom(f, iter_id),
                    None => IterType::Regular(
                        value_ops::iter(globals.get(iter_id), area!(source.clone(), i.span), globals)?
                    )
                };


                iters.push((e, iter));
            }

            let mut v_vec: Vec<Value> = vec![];
            let mut result_vec: Vec<ValuePos> = vec![];

            #[allow(clippy::too_many_arguments)]
            fn comp(
                expr: &ASTNode,
                cond: &Option<Box<ASTNode>>,
                iters: &mut Vec<(&ASTNode, IterType)>,
                base_scope: ScopePos,
                globals: &mut Globals,
                source: &EmeraldSource,
                idx: usize,
                v_vec: &mut Vec<Value>,
                result_vec: &mut Vec<ValuePos>,
                start_node: &ASTNode,
            ) -> Result<(), RuntimeError> {
                let is_last = idx == iters.len() - 1;

                macro_rules! for_loop {
                    ($i:expr) => {
                        globals.push_protected();
                        v_vec.push($i);
                        if is_last {
                            let derived = globals.derive_scope(base_scope, globals.get_scope(base_scope).func_id);
                            for ((left, _), i) in iters.iter().zip(v_vec.clone()) {
                                let temp = globals.insert_value(
                                    i.clone(),
                                    area!(source.clone(), left.span),
                                );

                                let mut list = HashMap::new();
                                do_assign(left, temp, derived, globals, source, &mut list, true)?;

                                for (l, r) in list {
                                    let right_clone = {
                                        let new_id = globals.clone_value(
                                            r,
                                            None,
                                        );
                                        globals.protect_value(new_id);
                                        new_id
                                    };
                                    
                                    globals.set_var(derived, l.1.unwrap(), right_clone);
                                }
                            }
                            let expr_id = {
                                let id = execute(expr, derived, globals, source)?;
                                let out = globals.clone_value(
                                    id,
                                    None,
                                );
                                globals.protect_value(id);
                                globals.protect_value(out);
                                out
                            };

                            match cond {
                                Some(c) => {
                                    let cond_id = {
                                        let id = execute(c, derived, globals, source)?;
                                        let out = globals.clone_value(
                                            id,
                                            None,
                                        );
                                        globals.protect_value(id);
                                        globals.protect_value(out);
                                        out
                                    };
                                    if value_ops::to_bool(globals.get(cond_id), area!(source.clone(), c.span), globals)? {
                                        result_vec.push(expr_id)
                                    }
                                },
                                None => result_vec.push(expr_id),
                            }
                        } else {
                            comp(
                                expr,
                                cond,
                                iters,
                                base_scope,
                                globals,
                                source,
                                idx + 1,
                                v_vec,
                                result_vec,
                                start_node
                            )?
                        }
                        v_vec.pop();
                        globals.pop_protected();
                    }
                }

                match &iters[idx].1 {
                    IterType::Regular(iter) => {
                        for i in iter.clone() {
                            for_loop!(i);
                        }
                    },
                    IterType::Custom(f, iter_id) => {

                        let next_f = run_func(f, vec![*iter_id], globals, base_scope, source, start_node)?;
                        let next_val = globals.get(next_f).value.clone();
                        match next_val {
                            Value::Function(f) => loop {
                                let next = run_func(&f, vec![], globals, base_scope, source, start_node)?;
                                match globals.get(next).value.clone() {
                                    Value::Option(o) => match o {
                                        Some(v) => {
                                            let val = globals.get(v).value.clone();
                                            for_loop!(val);
                                        },
                                        None => break,
                                    }
                                    other => return Err( RuntimeError::TypeMismatch {
                                        expected: "option".to_string(),
                                        found: other.type_str(globals),
                                        area: globals.get(next).def_area.clone(),
                                        defs: vec![(other.type_str(globals), globals.get(next).def_area.clone())],
                                    } )
                                }
                            },
                            _ => {
                                for i in value_ops::iter(globals.get(next_f), globals.get(next_f).def_area.clone(), globals)? {
                                    for_loop!(i);
                                }
                            }
                        }
                    },
                }
                

                Ok(())
            }

            comp(expr, cond, &mut iters, scope_id, globals, source, 0, &mut v_vec, &mut result_vec, start_node)?;

            Ok( globals.insert_value(
                Value::Array(result_vec),
                area!(source.clone(), start_node.span)
            ) )
        },
        NodeType::FuncPattern { args, ret_pattern } => {
            
            let mut v = vec![];
            for i in args {
                let pat = i;
                let pat_id = protecute!(pat => scope_id);
                match &globals.get(pat_id).value.clone() {
                    Value::Type(t) => v.push( Pattern::Type(t.clone()) ),
                    Value::Pattern(p) => v.push( p.clone() ),
                    other => ret!(Err( RuntimeError::TypeMismatch {
                        expected: "type or pattern".to_string(),
                        found: other.type_str(globals),
                        area: area!(source.clone(), pat.span),
                        defs: vec![(other.type_str(globals), globals.get(pat_id).def_area.clone())],
                    } ), Pop: 1)
                }
            }
            let ret_id = protecute!(ret_pattern => scope_id);
            let ret = match &globals.get(ret_id).value.clone() {
                Value::Type(t) => Pattern::Type(t.clone()),
                Value::Pattern(p) => p.clone(),
                other => ret!(Err( RuntimeError::TypeMismatch {
                    expected: "type or pattern".to_string(),
                    found: other.type_str(globals),
                    area: area!(source.clone(), ret_pattern.span),
                    defs: vec![(other.type_str(globals), globals.get(ret_id).def_area.clone())],
                } ), Pop: 1)
            };

            Ok( globals.insert_value(
                Value::Pattern(Pattern::Func(v, Box::new(ret))),
                area!(source.clone(), start_node.span)
            ) )

        }
    };

    globals.pop_protected();

    exec_result

}


