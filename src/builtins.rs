
use crate::EmeraldSource;
use crate::interpreter::execute;
use crate::error::RuntimeError;
use crate::interpreter::ValuePos;
use crate::CodeArea;
use crate::Globals;
use crate::interpreter::ScopePos;
use crate::parser::ASTNode;
use crate::value::Value;
use convert_case::{Case, Casing};
use crate::generator::Command;

use core::time;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::io;
use std::io::Write;
use std::{thread};

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
        #[derive(Debug, Clone, Copy)]
        #[derive(Eq, Hash, PartialEq)]
        pub enum BuiltinType {
            $(
                $type_name,
            )*
        }
        pub fn builtin_type_str(b: BuiltinType) -> String {
            match b {
                $(
                    BuiltinType::$type_name => stringify!($type_name).to_case(Case::Snake).to_string(),
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

        pub const BUILTIN_TYPE_NAMES: &[&str] = &[
            $(
                stringify!($type_name),
            )*
        ];

    };
}


builtin_types!(
    Number,
    Bool,
    String,
    Option,
    Builtin,
    Function,
    Array,
    Tuple,
    Dict,
    Type,
    Pattern,
    Range,

    McFunc,
    McVector,
    Selector,
);


enum SpecialArgs {
    Any
}



macro_rules! builtins {
    {
        ($call_node:ident, $scope_id:ident, $globals:ident, $source:ident)
        $(
            [$builtin_name:ident]: $func_name:ident(
                $(#[$arg_info:ident] => $var:ident)?
                $(
                    $(
                        $(&$mut:ident)? $arg_name:ident $(: $type:ident)? $(@ $arg_area:ident)?
                    ),+
                )?
            ) $code:block
        )*
    } => {
        #[derive(Clone, Copy, Debug, PartialEq, Hash)]
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
        pub const BUILTIN_NAMES: &[&str] = &[
            $(
                stringify!($func_name),
            )*
        ];
        pub fn name_to_builtin(s: &str) -> Builtin {
            match s {
                $(
                    stringify!($func_name) => Builtin::$builtin_name,  
                )*
                _ => unreachable!(),
            }
        }
        pub fn builtin_to_name(b: Builtin) -> String {
            match b {
                $(
                    Builtin::$builtin_name => stringify!($func_name).to_string(),  
                )*
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
            $globals: &mut Globals,
            $source: &EmeraldSource,
        ) -> Result<ValuePos, RuntimeError> {
            match arg_amount(func) {
                Some(n) => if args.len() != n {
                    return Err(
                        RuntimeError::IncorrectArgumentCount {
                            provided: args.len(),
                            takes: n,
                            header_area: CodeArea {
                                source: $source.clone(),
                                range: (0, 0)
                            },
                            call_area: area!($source.clone(), $call_node.span)
                        }
                    )
                }
                None => (),
            }
            match func {
                $(
                    Builtin::$builtin_name => {
                        $(
                            let mut __index = 0;
                            let mut arg_ids = vec![];
                            $(
                                let arg_id = execute(&args[__index], $scope_id, $globals, $source)?;
                                arg_ids.push(arg_id);
                                $globals.protect_value(arg_id);

                                $(
                                    let $arg_area = area!($source.clone(), args[__index].span);
                                )?

                                let $arg_name = $globals.get(arg_id).value.clone();
                                format!("{:?}", $arg_name); // just so it doesnt say unused
                                
                                $(
                                    let $arg_name;

                                    match $globals.get(arg_id).value.clone() {
                                        Value::$type(v) => {$arg_name = v; format!("{:?}", $arg_name);},
                                        other => return Err( RuntimeError::TypeMismatch {
                                            expected: stringify!($type).to_case(Case::Snake).to_string(),
                                            found: format!("{}", other.type_str($globals)),
                                            area: area!($source.clone(), args[__index].span),
                                            defs: vec![(other.type_str($globals), $globals.get(arg_id).def_area.clone())],
                                        } )
                                    }
                                )?

                                $(
                                    stringify!($mut);
                                    let mut $arg_name = $arg_name;
                                )?
                                
                                __index += 1;
                            )+

                            let __to_insert = $code;

                            $(
                                let mut __index = 0;
                                let arg_id = arg_ids[__index];
                                format!("{:?}", arg_id); // just so it doesnt say unused

                                macro_rules! do_mut {
                                    () => {
                                        $(
                                            stringify!($mut);
                                            let val = $arg_name;
                                            $globals.set_value(
                                                arg_id,
                                                val,
                                                Some( area!($source.clone(), $call_node.span) ),
                                            );
                                        )?
                                    };
                                    ($lol_type:ident) => {
                                        $(
                                            stringify!($mut);
                                            let val = Value::$lol_type($arg_name);
                                            $globals.set_value(
                                                arg_id,
                                                val,
                                                Some( area!($source.clone(), $call_node.span) ),
                                            );
                                        )?
                                    };
                                }
                                "piss?";
                                do_mut!( $($type)? );
                                "fart?";

                                __index += 1;
                            )+

                            Ok ($globals.insert_value(
                                __to_insert,
                                area!($source.clone(), $call_node.span)
                            ) )
                        )?
                        $(
                            match SpecialArgs::$arg_info {
                                SpecialArgs::Any => {
                                    let mut $var = vec![];
                                    for i in args {
                                        let arg_id = execute(&i, $scope_id, $globals, $source)?;
                                        $globals.protect_value(arg_id);
                                        $var.push( $globals.get(arg_id).value.clone() )
                                    }
                                    Ok ($globals.insert_value(
                                        $code,
                                        area!($source.clone(), $call_node.span)
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




builtins!{

    (call_node, scope_id, globals, source)

    [Print]: print(#[Any] => poopie) {
        let mut out_str = String::new();
        for i in poopie {
            out_str += &i.to_str(globals, &mut vec![]);
        }
        println!("{}", out_str);
        Value::unit()
    }

    [Input]: input(s: String) {
        print!("{}", s);
        io::stdout().flush().unwrap();
        let mut input_str = String::new();
        io::stdin()
            .read_line(&mut input_str)
            .expect("Failed to read line");
        Value::String(
            input_str
                .replace('\r', "")
                .replace('\n', "")
        )
    }

    [Length]: len(arr: Array) {
        Value::Number(arr.len() as f64)
    }


    [Sleep]: sleep(time: Number) {
        thread::sleep(time::Duration::from_millis((time * 1000.0) as u64));
        Value::unit()
    }


    [Sin]: sin(n: Number) { Value::Number(n.sin()) }
    [Cos]: cos(n: Number) { Value::Number(n.cos()) }
    [Tan]: tan(n: Number) { Value::Number(n.tan()) }

    [Sinh]: sinh(n: Number) { Value::Number(n.sinh()) }
    [Cosh]: cosh(n: Number) { Value::Number(n.cosh()) }
    [Tanh]: tanh(n: Number) { Value::Number(n.tanh()) }


    [Asin]: asin(n: Number) { Value::Number(n.asin()) }
    [Acos]: acos(n: Number) { Value::Number(n.acos()) }
    [Atan]: atan(n: Number) { Value::Number(n.atan()) }
    [Atan2]: atan2(a: Number, b: Number) { Value::Number(a.atan2(b)) }

    [Asinh]: asinh(n: Number) { Value::Number(n.asinh()) }
    [Acosh]: acosh(n: Number) { Value::Number(n.acosh()) }
    [Atanh]: atanh(n: Number) { Value::Number(n.atanh()) }

    [Floor]: floor(n: Number) { Value::Number(n.floor()) }

    [Sqrt]: sqrt(n: Number) { Value::Number(n.sqrt()) }
    [Ln]: ln(n: Number) { Value::Number(n.ln()) }
    [Log]: log(n: Number, b: Number) { Value::Number(n.log(b)) }

    [Abs]: abs(n: Number) { Value::Number(n.abs()) }
    [Signum]: signum(n: Number) { Value::Number(n.signum()) }
    [Exp]: exp(n: Number) { Value::Number(n.exp()) }





    [Substr]: substr(s: String, start: Number, end: Number) {
        Value::String(s.as_str()[(start as usize)..(end as usize)].to_string())
    }
    [SplitStr]: split_str(s: String, substr: String) {
        let mut out = vec![];
        for split in s.split(&substr) {
            let item = globals.insert_value(
                Value::String(split.to_string()),
                CodeArea {
                    source: source.clone(),
                    range: call_node.span,
                },
            );
            out.push(item);
        }
        Value::Array(out)
    }
    [ReverseStr]: reverse_str(s: String) {
        Value::String(s.chars().rev().collect::<String>())
    }

    [RandInt]: rand_int(a: Number, b: Number) {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let a = a.floor() as i64;
        let b = b.floor() as i64;
        let rand = ( rng.gen::<i64>() ).rem_euclid(b - a) + a;
        Value::Number(rand as f64)
    }
    [RandFloat]: rand_float(a: Number, b: Number) {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let rand = ( rng.gen::<f64>() ).rem_euclid(b - a) + a;
        Value::Number(rand)
    }

    [SetItem]: set_item(&mut d: Dictionary, k: String, v @ v_area) {
        let id = globals.insert_value(
            v,
            v_area,
        );
        d.insert(k, id);
        Value::unit()
    }
    [DeleteItem]: delete_item(&mut d: Dictionary, k: String) {
        d.remove(&k);
        Value::unit()
    }

    [PopIndex]: pop_index(&mut v: Array, i: Number) {
        v.remove(i as usize);
        Value::unit()
    }

    [Hash]: hash(v) {
        let mut s = DefaultHasher::new();
        v.hash(&mut s, Some(globals));
        Value::Number((s.finish() / 10000) as f64)
    }

    [Debug]: debug(v) {
        println!("{:#?}", v);
        Value::unit()
    }

    [ID]: id(#[Any] => poopie) {
        println!("id: {:?}", globals.get_scope(scope_id).func_id);
        Value::unit()

    }
    [Impls]: impls(#[Any] => poopie) {
        println!("builtin impls: {:#?}", globals.builtin_impls);
        println!("impls: {:#?}", globals.struct_impls);
        Value::unit()

    }

    [Command]: command(cmd: String) {
        let id = globals.get_scope(scope_id).func_id;
        globals.insert_command(id, Command::Basic(cmd));
        Value::unit()
    }
    [CommandIf]: command_if(cond: String, cmd @ cmd_area) {
        let f_id = globals.get_scope(scope_id).func_id;
        match cmd {
            Value::String(s) => globals.insert_command(f_id, Command::IfCall(cond, Box::new(Command::Basic(s)))),
            Value::McFunc(id) => globals.insert_command(f_id, Command::IfCall(cond, Box::new(Command::Call(id)))),
            other => return Err( RuntimeError::TypeMismatch {
                expected: "string or mc_func".to_string(),
                found: other.type_str(globals),
                area: cmd_area.clone(),
                defs: vec![(other.type_str(globals), cmd_area)],
            } ),
        }
        Value::unit()
    }
    [OnLoad]: on_load(cmd @ cmd_area) {
        match cmd {
            Value::String(s) => globals.on_load_command(Command::Basic(s)),
            Value::McFunc(id) => globals.on_load_command(Command::Call(id)),
            other => return Err( RuntimeError::TypeMismatch {
                expected: "string or mc_func".to_string(),
                found: other.type_str(globals),
                area: cmd_area.clone(),
                defs: vec![(other.type_str(globals), cmd_area)],
            } ),
        }
        Value::unit()
    }
    [OnTick]: on_tick(cmd @ cmd_area) {
        match cmd {
            Value::String(s) => globals.on_tick_command(Command::Basic(s)),
            Value::McFunc(id) => globals.on_tick_command(Command::Call(id)),
            other => return Err( RuntimeError::TypeMismatch {
                expected: "string or mc_func".to_string(),
                found: other.type_str(globals),
                area: cmd_area.clone(),
                defs: vec![(other.type_str(globals), cmd_area)],
            } ),
        }
        Value::unit()
    }



}


