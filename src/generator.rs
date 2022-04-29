use std::fs;

use crate::{interpreter::{Globals, McFuncID}, optimize::remove_single};

#[derive(Debug, Clone)]
pub enum Command {
    Basic(String),
    Call(McFuncID),
    IfCall(String, Box<Command>)
}

impl Command {
    pub fn cmd_str(&self) -> String {
        match self {
            Command::Basic(s) => s.clone(),
            Command::Call(id) => format!("function mrld:gen{}", id),
            Command::IfCall(cond, cmd) => format!("execute if {} run {}", cond, cmd.cmd_str()),
        }
    }
    pub fn get_call(&mut self) -> Option<(&mut Command, McFuncID)> {
        let id = match self {
            Command::Call(id) => *id,
            _ => 0,
        };
        match self {
            Command::Basic(_) => None,
            Command::Call(_) => Some((self, id)),
            Command::IfCall(_, c) => c.get_call(),
        }
    }
}







const MCMETA: &str = r#"
{
    "pack": {
        "pack_format": 3,
        "description": "Datapack generated by the Emerald Programming Language"
    }
}
"#;

const LOAD_JSON: &str = r#"
{
    "values": [
        "mrld:genload"
    ]
}
"#;

const TICK_JSON: &str = r#"
{
    "values": [
        "mrld:gentick"
    ]
}
"#;


const BASE_DIR: &str = r#"/Users/flow/Library/Application Support/minecraft/saves/emerald/datapacks/"#;

pub fn generate_datapack(globals: &mut Globals) {

    fs::create_dir_all(BASE_DIR.to_string() + "emeraldgen/data").unwrap();

    fs::remove_dir_all(BASE_DIR.to_string() + "emeraldgen/data").unwrap();
    fs::create_dir_all(BASE_DIR.to_string() + "emeraldgen/data/mrld/functions").unwrap();
    fs::create_dir_all(BASE_DIR.to_string() + "emeraldgen/data/minecraft/tags/functions").unwrap();
    fs::write(BASE_DIR.to_string() +  "emeraldgen/pack.mcmeta", MCMETA).unwrap();

    remove_single(&mut globals.mcfuncs);

    for (i, v) in globals.mcfuncs.iter().enumerate() {
        if !(i > 0 && v.len() == 1) {
            fs::write(
                BASE_DIR.to_string() + &format!("emeraldgen/data/mrld/functions/gen{}.mcfunction", i),
                v.iter().map(|f| f.cmd_str()).collect::<Vec<String>>().join("\n")
            ).unwrap();
        }
    }

    fs::write(
        BASE_DIR.to_string() + "emeraldgen/data/mrld/functions/genload.mcfunction",
        globals.load_func.iter().map(|f| f.cmd_str()).collect::<Vec<String>>().join("\n")
    ).unwrap();
    fs::write(
        BASE_DIR.to_string() + "emeraldgen/data/mrld/functions/gentick.mcfunction",
        globals.tick_func.iter().map(|f| f.cmd_str()).collect::<Vec<String>>().join("\n")
    ).unwrap();

    fs::write(
        BASE_DIR.to_string() + "emeraldgen/data/minecraft/tags/functions/load.json",
        LOAD_JSON,
    ).unwrap();
    fs::write(
        BASE_DIR.to_string() + "emeraldgen/data/minecraft/tags/functions/tick.json",
        TICK_JSON,
    ).unwrap();

}

