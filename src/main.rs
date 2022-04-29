

mod lexer;
mod parser;
mod value;
mod interpreter;

mod error;
mod builtins;
mod generator;
mod optimize;

use std::{io::{self, Write}, collections::HashMap, path::PathBuf, fs};

use ariadne::Source;
use error::{ToReport, RuntimeError};
use fnv::FnvHashMap;
use interpreter::{execute, Globals, Exit};
use lasso::Rodeo;
use logos::Logos;
use parser::ParseData;

use crate::{parser::parse, generator::generate_datapack};



#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum EmeraldSource {
    String(String),
    File(PathBuf),
}



#[derive(Debug, Clone)]
#[derive(PartialEq, Hash)]
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

#[derive(Default)]
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

    let cache = EmeraldCache::default();
    // cache.fetch(&source).unwrap();

    let data = ParseData {
        source: source.clone(),
        tokens,
    };
    let mut interner = Rodeo::default();

    let ast = parse(&data, &mut interner);
    
    let (mut globals, base_scope) = Globals::new(interner);
    globals.exports.push( FnvHashMap::default() );
    match ast {
        Ok((node, _)) => {
            
            globals.init_global(base_scope, &data.source);

            


            use std::time::Instant;
            let start = Instant::now();

            let mut result = execute(
                &node,
                base_scope,
                &mut globals,
                &source,
            );
            let duration = start.elapsed();
            // for (i, (v, _)) in &globals.values {
            //     println!("{:?}: {:?}", i, v.value)
            // }

            println!("penis: {:?}", duration);

            // println!("collect: {} {} {}", globals.values.len(), globals.scopes.len(), globals.protected.len());
            
            if result.is_ok() {
                result = match globals.exits.last() {
                    Some(
                        Exit::Return(_, span)
                    ) => {
                        Err(
                            RuntimeError::ReturnUsedOutsideProgram {
                                return_area: area!(source.clone(), *span),
                            }
                        )
                    },
                    Some(
                        Exit::Break(_, span)
                    ) => {
                        Err(
                            RuntimeError::BreakUsedOutsideProgram {
                                break_area: area!(source.clone(), *span),
                            }
                        )
                    },
                    Some(
                        Exit::Continue(span)
                    ) => {
                        Err(
                            RuntimeError::ContinueUsedOutsideProgram {
                                continue_area: area!(source.clone(), *span),
                            }
                        )
                    },
                    None => result,
                };
            }

            match result {
                Ok(pos) => {
                    if print_return {
                        println!("{}", ansi_term::Color::RGB(255, 175, 0).bold().paint( globals.get(pos).value.to_str(&globals, &mut vec![]) ))
                    }

                    generate_datapack(&mut globals);

                    true
                },
                Err(e) => {
                    e.to_report().print_error(cache, &globals);
                    false
                },
            }

        },
        Err(e) => {
            let gaga = e.to_report();
            gaga.print_error(cache, &globals);
            false
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
            
            let input_str = input_str.replace('\r', "");
            let mut input_str = input_str.chars();
            input_str.next_back();
            if run(repl_code.clone() + input_str.as_str(), EmeraldSource::String(repl_code.clone() + input_str.as_str()), true) {
                repl_code += input_str.as_str();
                repl_code += "\n";
            }
    
            
        }
    }

}
