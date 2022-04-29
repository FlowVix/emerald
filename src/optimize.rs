use crate::{generator::Command};










pub fn remove_single(mcfuncs: &mut [Vec<Command>]) {
    let single_cmds: Vec<Option<Command>> = mcfuncs.iter().map(
        |v| if v.len() == 1 {
            Some( v[0].clone() )
        } else {
            None
        }
    ).collect();

    for file in mcfuncs.iter_mut() {
        for command in file {
            while let Some((cmd, f_id)) = command.get_call() {
                match &single_cmds[f_id] {
                    Some(c) => (*cmd) = c.clone(),
                    None => break,
                }
            }
        }
    }
}
