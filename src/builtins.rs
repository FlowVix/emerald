
use crate::interpreter::execute;
use crate::error::RuntimeError;
use crate::interpreter::MemoryPos;
use crate::CodeArea;
use crate::RunInfo;
use crate::ScopeList;
use crate::Memory;
use crate::interpreter::ScopePos;
use crate::parser::ASTNode;
use crate::value::Value;

use core::time;
use std::io;
use std::io::Write;
use std::{collections::HashMap, thread};

macro_rules! area {
    ($source:expr, $area:expr) => {
        CodeArea {source: $source, range: $area}
    };
    ($source:expr, $start:expr, $end:expr) => {
        CodeArea {source: $source, range: ($start, $end)}
    };
}


macro_rules! builtin_types {
    (
        $(
            $type_name:ident,
        )*
    ) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum BuiltinType {
            $(
                $type_name,
            )*
        }
        pub fn builtin_type_str(b: BuiltinType) -> String {
            match b {
                $(
                    BuiltinType::$type_name => stringify!($type_name).to_lowercase().to_string(),
                )*
            }
        }
        pub fn builtin_type_from_str(s: &str) -> BuiltinType {
            match s {
                $(
                    stringify!($type_name) => BuiltinType::$type_name,
                )*
                _ => unreachable!(),
            }
        }
        pub fn builtin_type_names() -> Vec<String> {
            let mut list = vec![];
            $(
                list.push(stringify!($type_name).to_string());
            )*
            list
        }

    };
}




enum SpecialArgs {
    Any
}



macro_rules! builtins {
    {
        ($call_node:ident, $scope_id:ident, $memory:ident, $scopes:ident, $info:ident, $areas:ident)
        $(
            [$builtin_name:ident]: $func_name:ident(
                $(@$arg_info:ident => $var:ident)?
                $(
                    $(
                        $arg_name:ident $(: $type:ident)?
                    ),+
                )?
            ) $code:block
        )*
    } => {
        #[derive(Clone, Copy)]
        pub enum Builtin {
            $(
                $builtin_name,
            )*
        }
        // pub fn builtin_name(b: Builtin) -> String {
        //     match b {
        //         $(
        //             Builtin::$builtin_name => stringify!($func_name).to_string(),  
        //         )*
        //     }
        // }
        pub fn builtin_names() -> Vec<String> {
            let mut list = vec![];
            $(
                list.push(stringify!($func_name).to_string());
            )*
            list
        }
        pub fn name_to_builtin(s: &str) -> Builtin {
            match s {
                $(
                    stringify!($func_name) => Builtin::$builtin_name,  
                )*
                _ => unreachable!(),
            }
        }
        pub fn arg_amount(b: Builtin) -> Option<usize> {
            match b {
                $(
                    Builtin::$builtin_name => {
                        $(
                            let mut amount = 0;
                            $(
                                stringify!($arg_name);
                                amount += 1;
                            )+
                            Some(amount)
                        )?
                        $(
                            stringify!($arg_info);
                            return None;
                        )?
                    },  
                )*
            }
        }
        pub fn run_builtin(
            func: Builtin,
            $call_node: &ASTNode,
            args: &Vec<ASTNode>,
            $scope_id: ScopePos,
            $memory: &mut Memory,
            $scopes: &mut ScopeList,
            $info: &mut RunInfo,
        ) -> Result<MemoryPos, RuntimeError> {
            match arg_amount(func) {
                Some(n) => if args.len() != n {
                    return Err(
                        RuntimeError::IncorrectArgumentCount {
                            provided: args.len(),
                            takes: n,
                            header_area: CodeArea {
                                source: $info.source.clone(),
                                range: (0, 0)
                            },
                            call_area: area!($info.source.clone(), $call_node.span)
                        }
                    )
                }
                None => (),
            }
            match func {
                $(
                    Builtin::$builtin_name => {
                        $(
                            let mut $areas = vec![];
                            let mut __arg_ids = vec![];
                            let mut __index = 0;
                            $(
                                let arg_id = execute(&args[__index], $scope_id, $memory, $scopes, $info)?;

                                $areas.push($memory.get(arg_id).def_area.clone());
                                let $arg_name = $memory.get(arg_id).value.clone();
                                $arg_name.type_str($memory); // just so it doesnt say unused


                                
                                $(
                                    let $arg_name;
                                    match $memory.get(arg_id).value.clone() {
                                        Value::$type(inner) => {$arg_name = inner},
                                        other => return Err( RuntimeError::TypeMismatch {
                                            expected: stringify!($type).to_string(),
                                            found: format!("{}", other.type_str($memory)),
                                            area: area!($info.source.clone(), args[__index].span),
                                            defs: vec![(other.type_str($memory), $memory.get(arg_id).def_area.clone())],
                                        } )
                                    }
                                )?
                                
                                __arg_ids.push( arg_id );
                                __index += 1;
                            )+
                            let __areas = 4;
                            let __to_insert = $code;
                            Ok ($memory.insert(
                                __to_insert,
                                area!($info.source.clone(), $call_node.span)
                            ) )
                        )?
                        $(
                            match SpecialArgs::$arg_info {
                                SpecialArgs::Any => {
                                    let mut $areas = vec![];
                                    let mut $var = vec![];
                                    for i in args {
                                        let arg_id = execute(&i, $scope_id, $memory, $scopes, $info)?;
                                        $areas.push($memory.get(arg_id).def_area.clone());
                                        $var.push( $memory.get(arg_id).value.clone() )
                                    }
                                    Ok ($memory.insert(
                                        $code,
                                        area!($info.source.clone(), $call_node.span)
                                    ) )
                                }
                            }
                        )?
                    },  
                )*
            }
        }
    };
}


builtin_types!(
    Number,
    Bool,
    String,
    Null,
    Builtin,
    Function,
    Array,
    Dict,
    Type,
);


builtins!{

    (call_node, scope_id, memory, scopes, info, __areas)

    [Print]: print(@Any => poopie) {
        let mut out_str = String::new();
        for i in poopie {
            out_str += &i.to_str(memory, &mut vec![]);
        }
        println!("{}", out_str);
        Value::Null
    }

    [Input]: input(prompt: String) {
        print!("{}", format!("{}", prompt));
        io::stdout().flush().unwrap();
        let mut input_str = String::new();
        io::stdin()
            .read_line(&mut input_str)
            .expect("Failed to read line");
        Value::String(
            input_str
                .replace("\r", "")
                .replace("\n", "")
        )
    }

    [Length]: len(arr: Array) {
        Value::Number(arr.len() as f64)
    }


    [Sleep]: sleep(time: Number) {
        thread::sleep(time::Duration::from_millis((time * 1000.0) as u64));
        Value::Null
    }

    [Sin]: sin(n: Number) { Value::Number(n.sin()) }
    [Cos]: cos(n: Number) { Value::Number(n.cos()) }
    [Tan]: tan(n: Number) { Value::Number(n.tan()) }

    [Sqrt]: sqrt(n: Number) { Value::Number(n.sqrt()) }





}


