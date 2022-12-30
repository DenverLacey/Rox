use crate::{
    interp::ParsedFile,
    ir::ast::{Ast, AstInfo},
    util::errors::Result,
};

use super::exe::{Executable, ExecutableBuilder};

pub fn compile_executable(files: &[ParsedFile]) -> Result<Executable> {
    let mut compiler = Compiler::new();

    for node in files
        .iter()
        .flat_map(|file| file.ast.iter().map(|queued| &queued.node))
        .filter(|node| is_node_compilable(*node))
    {
        compiler.compile_node(node)?;
    }

    let exe = compiler.exe.build();
    Ok(exe)
}

fn is_node_compilable(node: &Ast) -> bool {
    match node.info {
        AstInfo::Import(_) | AstInfo::TypeSignature(_) => false,
        _ => true,
    }
}

struct Compiler {
    exe: ExecutableBuilder,
}

impl Compiler {
    fn new() -> Self {
        Self {
            exe: ExecutableBuilder::new(),
        }
    }
}

impl Compiler {
    fn compile_node(&mut self, node: &Ast) -> Result<()> {
        todo!()
    }
}
