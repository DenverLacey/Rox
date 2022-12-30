use crate::{
    interp::{ParsedFile, FunctionInfo},
    ir::{
        annotations::Annotations,
        ast::{Ast, AstInfo, AstInfoFn},
    },
    util::errors::{Result, SourceError}, parsing::tokenization::Token,
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

    compiler.exe.build()
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
        match &node.info {
            AstInfo::Literal => todo!(),
            AstInfo::Unary(kind, expr) => todo!(),
            AstInfo::Binary(kind, lhs, rhs) => todo!(),
            AstInfo::Block(kind, nodes) => todo!(),
            AstInfo::Fn(info) => todo!(),
            AstInfo::Var(info) => todo!(),
            AstInfo::TypeValue(typ) => todo!(),

            AstInfo::Import(_) | AstInfo::TypeSignature(_) => unreachable!(),
            
        }
    }

    fn parse_fn_decl(&mut self, token: &Token, info: &AstInfoFn) -> Result<()> {
        todo!();

        let func = FunctionInfo {
            id: todo!(),
            name: todo!(),
            typ: todo!(),
            code: todo!(),
        };

        let is_entry_point = info.annons.contains(Annotations::FN_ENTRY);
        if is_entry_point {
            if !self.exe.set_entry_point(func.id.0) {
                return Err(SourceError::new("Multiple entry points declared.", token.loc, "This was declared as the entry point when another function already has.").into());
            }
        }

        self.exe.add_func(func);
        Ok(())
    }
}
