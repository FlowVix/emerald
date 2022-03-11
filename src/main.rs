

mod lexer;
mod parser;
mod value;
mod interpreter;

mod error;
mod builtins;

use std::{io::{self, Write}, collections::HashMap, path::PathBuf, fs};

use ansi_term;
use ariadne::{Source, Cache};
use builtins::{builtin_names, builtin_type_from_str, builtin_type_names};
use error::{ToReport, RuntimeError};
use interpreter::{execute, Memory, ScopeList, RunInfo, Exit};
use logos::Logos;
use value::{Value, ValueType, Pattern};

use crate::parser::parse;



#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum EmeraldSource {
    String(String),
    File(PathBuf),
}



#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub struct CodeArea {
    source: EmeraldSource,
    range: (usize, usize),
}

impl ariadne::Span for CodeArea {
    type SourceId = EmeraldSource;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.range.0
    }

    fn end(&self) -> usize {
        self.range.1
    }
}

pub struct EmeraldCache {
    files: HashMap<EmeraldSource, Source>
}

impl ariadne::Cache<EmeraldSource> for EmeraldCache {
    fn fetch(&mut self, id: &EmeraldSource) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(
            match self.files.entry(id.clone()) {
                std::collections::hash_map::Entry::Occupied(e) => e.into_mut(),
                std::collections::hash_map::Entry::Vacant(e) => e.insert(
                    Source::from(match id {
                        EmeraldSource::String(s) => s.clone(),
                        EmeraldSource::File(path) => fs::read_to_string(path).map_err(|e| Box::new(e) as _)?,
                    })
                ),
            }
        )
    }

    fn display<'a>(&self, id: &'a EmeraldSource) -> Option<Box<dyn std::fmt::Display + 'a>> {
        match id {
            EmeraldSource::String(_) => Some(Box::new("eval")),
            EmeraldSource::File(f) => Some(Box::new(f.display())),
        }
    }
}

impl Default for EmeraldCache {
    fn default() -> Self {
        Self { files: HashMap::new() }
    }
}

macro_rules! area {
    ($source:expr, $area:expr) => {
        CodeArea {source: $source, range: $area}
    };
    ($source:expr, $start:expr, $end:expr) => {
        CodeArea {source: $source, range: ($start, $end)}
    };
}



fn run(code: String, source: EmeraldSource, print_return: bool) -> bool {
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

    let mut cache = EmeraldCache::default();
    cache.fetch(&source).unwrap();

    let ast = parse(&tokens, &source);
    let mut info = RunInfo {source: source.clone(), exits: vec![], trace: vec![]};
    match ast {
        Ok((node, _)) => {
            let mut memory = Memory::new();
            let mut scopes = ScopeList::new();
            
            for i in builtin_names() {
                scopes.set_var(0, i.clone(), memory.insert(
                    Value::Builtin(i),
                    CodeArea {
                        source: source.clone(),
                        range: (0, 0)
                    },
                ));
            }

            for i in builtin_type_names() {
                scopes.set_var(0, i.to_lowercase().to_string(), memory.insert(
                    Value::Type(ValueType::Builtin(builtin_type_from_str(&i))),
                    CodeArea {
                        source: source.clone(),
                        range: (0, 0)
                    },
                ));
            }

            scopes.set_var(0, "any".to_string(), memory.insert(
                Value::Pattern(Pattern::Any),
                CodeArea {
                    source: source.clone(),
                    range: (0, 0)
                },
            ));

            let mut result = execute(
                &node,
                0,
                &mut memory,
                &mut scopes,
                &mut info,
            );

            result = match info.exits.last() {
                Some(
                    Exit::Return(_, span)
                ) => {
                    Err(
                        RuntimeError::ReturnUsedOutsideProgram {
                            return_area: area!(info.source.clone(), *span),
                        }
                    )
                },
                Some(
                    Exit::Break(_, span)
                ) => {
                    Err(
                        RuntimeError::BreakUsedOutsideProgram {
                            break_area: area!(info.source.clone(), *span),
                        }
                    )
                },
                None => result,
            };

            match result {
                Ok(pos) => {
                    if print_return {
                        match &memory.get(pos).value {
                            Value::Null => (),
                            other => println!("{}", ansi_term::Color::RGB(255, 175, 0).bold().paint(format!("{}", other.to_str(&memory, &mut vec![]))))
                        }
                    }
                    println!("{:?}", memory.protected);
                    // println!("{}", memory.register.len());
                    // for i in &memory.register {
                    //     println!("{}: {:?}", i.0, i.1)
                    // }
                    return true
                },
                Err(e) => {
                    e.to_report().print_error(cache, &info);
                    return false
                },
            }

        },
        Err(e) => {
            let gaga = e.to_report();
            gaga.print_error(cache, &info);
            // println!("{:#?}", gaga);
            return false
        },
    }

    // let mut colors = ColorGenerator::new();
        
    //     Report::build(ReportKind::Error, (), 34)
    //         .with_message("Incompatible types")
    //         .with_label(Label::new(32..33).with_message("This is of type Nat").with_color(colors.next()))
    //         .with_label(Label::new(42..45).with_message("This is of type Str").with_color(colors.next()))
    //         .finish()
    //         .print(Source::from(include_str!("sample.tao")))
    //         .unwrap();


    
}

fn main() {
    print!("\x1B[2J\x1B[1;1H");
    io::stdout().flush().unwrap();

    if true {
        let mut buf = PathBuf::new();
        buf.push("test.mrld");
        let code = fs::read_to_string(buf.clone()).unwrap();
        run(code, EmeraldSource::File(buf), false);
    } else {
        println!("{}", ansi_term::Color::RGB(30, 247, 88).bold().paint("
        
        ╭────────────────────────────────────╮
        │       Emerald v0.0.1 Console       │
        ╰────────────────────────────────────╯
    
    "));
    
        let mut repl_code = String::new();
        
    
        loop {
            print!("{}", ansi_term::Color::RGB(100, 100, 100).bold().paint("\n>>> "));
            io::stdout().flush().unwrap();
    
            let mut input_str = String::new();
            io::stdin()
                .read_line(&mut input_str)
                .expect("Failed to read line");
            
            let input_str = input_str.replace("\r", "");
            let mut input_str = input_str.chars();
            input_str.next_back();
            if run(repl_code.clone() + input_str.as_str(), EmeraldSource::String(repl_code.clone() + input_str.as_str()), true) {
                repl_code += input_str.as_str();
                repl_code += "\n";
            }
    
            
        }
    }

}
