use crate::{
    canon::scoping::Scope,
    interp::{Interpreter, ParsedFile},
    ir::ast::{
        Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoVar, AstUnaryKind, Queued,
        QueuedProgress,
    },
    parsing::tokenization::Token,
};

use super::value_type::Type;

pub fn typecheck_files(files: &mut [ParsedFile]) -> Result<(), &'static str> {
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
    let interp = Interpreter::get_mut();

    match &mut queued.node.info {
        AstInfo::Var(info) => {
            let scope = &mut interp.scopes[queued.node.scope.0];
            typecheck_var_decl(scope, &queued.node.token, info)?;
            queued.progress = QueuedProgress::Typechecked;
        }
        AstInfo::Fn(info) => {
            let scope = &mut interp.scopes[queued.node.scope.0];

            match queued.progress {
                QueuedProgress::DependenciesFound => {
                    typecheck_fn_header(scope, &queued.node.token, info)?;
                    queued.progress = QueuedProgress::PartiallyTypechecked;
                }
                QueuedProgress::PartiallyTypechecked => {
                    typecheck_fn_body(scope, &queued.node.token, info)?;
                    queued.progress = QueuedProgress::Typechecked;
                }
                _ => panic!(
                    "[INTERNAL ERR] Function node reached `typecheck_queued` at phase {:?}",
                    queued.progress
                ),
            }
        }
        AstInfo::Import(info) => todo!(),
        _ => {
            typecheck_node(interp, &mut queued.node)?;
            queued.progress = QueuedProgress::Typechecked;
        }
    }

    Ok(())
}

fn typecheck_var_decl(
    scope: &mut Scope,
    token: &Token,
    info: &mut AstInfoVar,
) -> Result<(), &'static str> {
    todo!()
}

fn typecheck_fn_header(
    scope: &mut Scope,
    token: &Token,
    info: &mut AstInfoFn,
) -> Result<(), &'static str> {
    todo!()
}

fn typecheck_fn_body(
    scope: &mut Scope,
    token: &Token,
    info: &mut AstInfoFn,
) -> Result<(), &'static str> {
    todo!()
}

fn typecheck_node(interp: &mut Interpreter, node: &mut Ast) -> Result<(), &'static str> {
    match &mut node.info {
        AstInfo::Literal => {
            let scope = &mut interp.scopes[node.scope.0];
            let typ = typecheck_literal(scope, &node.token)?;
            node.typ = Some(typ);
        }
        AstInfo::Unary(kind, sub_expr) => {
            let scope = &mut interp.scopes[node.scope.0];
            let typ = typecheck_unary(scope, &node.token, *kind, sub_expr)?;
            node.typ = Some(typ);
        }
        AstInfo::Binary(kind, lhs, rhs) => {
            let scope = &mut interp.scopes[node.scope.0];
            let typ = typecheck_binary(scope, &node.token, *kind, lhs, rhs)?;
            node.typ = Some(typ);
        }
        AstInfo::Block(kind, nodes) => {
            let scope = &mut interp.scopes[node.scope.0];
            typecheck_block(scope, &node.token, *kind, nodes)?;
        }
        AstInfo::Var(info) => todo!(),
        AstInfo::Fn(info) => todo!(),
        AstInfo::Import(info) => todo!(),
    }

    Ok(())
}

type TypecheckResult = Result<Type, &'static str>;

fn typecheck_literal(scope: &mut Scope, token: &Token) -> TypecheckResult {
    todo!()
}

fn typecheck_unary(
    scope: &mut Scope,
    token: &Token,
    kind: AstUnaryKind,
    expr: &mut Ast,
) -> TypecheckResult {
    todo!()
}

fn typecheck_binary(
    scope: &mut Scope,
    token: &Token,
    kind: AstBinaryKind,
    lhs: &mut Ast,
    rhs: &mut Ast,
) -> TypecheckResult {
    todo!()
}

fn typecheck_block(
    scope: &mut Scope,
    token: &Token,
    kind: AstBlockKind,
    nodes: &mut [Ast],
) -> Result<(), &'static str> {
    todo!()
}
