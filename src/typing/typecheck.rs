use crate::{interp::ParsedFile, ir::ast::Queued};

pub fn typecheck(files: &mut [ParsedFile]) -> Result<(), &'static str> {
    let mut typecheck_complete = false;
    while !typecheck_complete {
        typecheck_complete = true;

        for queued in files.iter_mut().flat_map(|file| file.ast.iter_mut()) {
            if !queued.all_dependencies_typechecked() {
                typecheck_complete = false;
                continue;
            } else if queued.is_typechecked() {
                continue;
            }

            typecheck_queued(queued)?;
        }
    }

    Ok(())
}

fn typecheck_queued(queued: &mut Queued) -> Result<(), &'static str> {
    todo!()
}
