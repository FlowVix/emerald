

mod lexer;
mod parser;
mod value;
mod interpreter;

mod error;

use std::{io::{self, Write}, collections::HashMap};

use ansi_term;
use ariadne::{Source, Cache};
use error::ToReport;
use interpreter::{execute, Memory, ScopeList, RunInfo};
use logos::Logos;
use value::Value;

use crate::parser::parse;



#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum EmeraldSource {
    String(String),
}

impl EmeraldSource {
    pub fn name(&self) -> String {
        match self {
            EmeraldSource::String(_) => "eval".to_string(),
        }
    }
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
                    })
                ),
            }
        )
    }

    fn display<'a>(&self, id: &'a EmeraldSource) -> Option<Box<dyn std::fmt::Display + 'a>> {
        match id {
            EmeraldSource::String(_) => Some(Box::new("eval")),
        }
    }
}

impl Default for EmeraldCache {
    fn default() -> Self {
        Self { files: HashMap::new() }
    }
}




fn run(code: String) -> bool {
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
        (code.len(), code.len() + 1)
    ));

    // println!("{:?}", tokens);

    let source = EmeraldSource::String(code.clone());
    let mut cache = EmeraldCache::default();
    cache.fetch(&source);

    let ast = parse(&tokens, &source);
    match ast {
        Ok((node, _)) => {
            let mut memory = Memory::new();
            let mut scopes = ScopeList::new();
            
            scopes.set_var(0, "print".to_string(), memory.insert(
                Value::Builtin("print".to_string()),
                false,
                CodeArea {
                    source: source.clone(),
                    range: (0, 0)
                },
            ));

            let result = execute(
                &node,
                0,
                &mut memory,
                &mut scopes,
                &mut RunInfo {source}
            );

            match result {
                Ok(pos) => {
                    match &memory.get(pos).value {
                        Value::Null => (),
                        other => println!("{}", ansi_term::Color::RGB(255, 175, 0).bold().paint(format!("{}", other.to_str())))
                    }
                    // for i in &memory.register {
                    //     println!("{}: {:?}", i.0, i.1)
                    // }
                    return true
                },
                Err(e) => {
                    e.to_report().print_error(cache);
                    return false
                },
            }

        },
        Err(e) => {
            let gaga = e.to_report();
            gaga.print_error(cache);
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
        if run(repl_code.clone() + input_str.as_str()) {
            repl_code += input_str.as_str();
            repl_code += "\n";
        }

        
    }
}
