use crate::{
    canon::scoping::{FunctionBinding, Scope, ScopeBinding, VariableBinding},
    interp::{Interpreter, ParsedFile},
    ir::ast::{
        Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoVar, AstUnaryKind, Queued,
        QueuedProgress, VariableInitializer,
    },
    parsing::tokenization::{Token, TokenInfo},
    util::lformat,
};

use super::value_type::{Type, TypeInfoFunction};

pub fn typecheck_files(files: &mut [ParsedFile]) -> Result<(), &'static str> {
    let mut typecheck_complete = false;
    while !typecheck_complete {
        typecheck_complete = true;

        // @TODO:
        // As it is currently, functions won't be fully typechecked unless there is one last thing
        // that causes a final loop over the queued nodes.
        //
        for queued in files.iter_mut().flat_map(|file| file.ast.iter_mut()) {
            if !queued.all_dependencies_typechecked() && !matches!(queued.node.info, AstInfo::Fn(_)) {
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
    let interp = Interpreter::get_mut();

    assert!(
        matches!(info.targets.info, AstInfo::Literal),
        "We don't support multiple var decl targets yet."
    );

    let TokenInfo::Ident(ident) = &info.targets.token.info else {
        panic!("[INTERNAL ERR] `targets` node in var decl is a `Literal` node but not an `Ident` node.");
    };

    let var_type: Type;
    match &mut info.initializer {
        VariableInitializer::TypeAndExpr(typ, expr) => {
            typecheck_node(interp, typ)?;
            if !matches!(typ.typ, Some(Type::Type)) {
                return Err("Specified type in variable declaration was not a type.");
            }

            typecheck_node(interp, expr)?;

            let AstInfo::TypeValue(typ_value) = typ.info else {
                unreachable!();
            };

            let Some(expr_type) = expr.typ else {
                panic!("[INTERNAL ERR] `expr` node in var decl doesn't have a type.");
            };

            if expr_type != typ_value {
                return Err(lformat!("Initializer expression of variable declaration is of type `{}` but the specified type is `{}`.", expr_type, typ_value));
            }

            var_type = typ_value;
        }
        VariableInitializer::Type(typ) => {
            typecheck_node(interp, typ)?;
            if !matches!(typ.typ, Some(Type::Type)) {
                return Err("Specified type in variable declaration was not a type.");
            }

            let AstInfo::TypeValue(typ_value) = typ.info else {
                panic!("Specified type node in var decl did not have a `TypeValue` info.");
            };

            var_type = typ_value;
        }
        VariableInitializer::Expr(expr) => {
            typecheck_node(interp, expr)?;
            let Some(expr_type) = expr.typ else {
                panic!("[INTERNAL ERR] `expr` node in var decl doesn't have a type.");
            };

            var_type = expr_type;
        }
    }

    let binding = ScopeBinding::Var(VariableBinding {
        is_mut: info.mutable,
        typ: var_type,
    });
    scope.add_binding(
        ident.clone(),
        binding,
        lformat!("Redeclaration of `{}`", ident),
    )?;

    Ok(())
}

fn typecheck_fn_header(
    scope: &mut Scope,
    token: &Token,
    info: &mut AstInfoFn,
) -> Result<(), &'static str> {
    let interp = Interpreter::get_mut();

    let TokenInfo::Ident(ident) = &info.ident.token.info else {
        panic!("[INTERNAL ERR] `ident` node in fn decl is not an `Ident` node.");
    };

    let AstInfo::Block(AstBlockKind::Params, params) = &mut info.params.info else {
        panic!("[INTERNAL ERR] `params` node of fn decl node not a `Params` node.");
    };

    let mut return_type = None;
    let mut param_types = vec![];

    for param in params {
        let AstInfo::Binary(AstBinaryKind::Param, name, typ) = &mut param.info else {
            panic!("[INTERNAL ERR] Node in `params` of fn decl was not a `Param` node.");
        };

        let TokenInfo::Ident(param_name) = &name.token.info else {
            panic!("[INTERNAL ERR] Name of paramter was not an `Ident` node.");
        };

        typecheck_node(interp, typ)?;
        if !matches!(typ.typ, Some(Type::Type)) {
            return Err("Expression expected to be a type for a function parameter.");
        }

        let AstInfo::TypeValue(param_type) = typ.info else {
            unreachable!();
        };

        // @TODO: Mutable parameters?
        let param_binding = ScopeBinding::Var(VariableBinding {
            is_mut: false,
            typ: param_type,
        });

        let param_scope = &mut interp.scopes[info.params.scope.0];
        param_scope.add_binding(
            param_name.clone(),
            param_binding,
            lformat!(
                "There is already a parameter with the name `{}`.",
                param_name
            ),
        )?;

        param_types.push(param_type);
    }

    let fn_info = TypeInfoFunction {
        params: param_types.into_boxed_slice(),
        returns: return_type,
    };

    let fn_type = interp.create_or_get_function_type(fn_info);
    let fn_id = interp.create_function(ident, fn_type);

    let binding = ScopeBinding::Fn(FunctionBinding {
        id: fn_id,
        typ: fn_type,
    });
    scope.add_binding(
        ident.clone(),
        binding,
        lformat!("Redeclaration of `{}`.", ident),
    )?;

    Ok(())
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
        AstInfo::TypeValue(_) => {}
    }

    Ok(())
}

type TypecheckResult = Result<Type, &'static str>;

fn typecheck_literal(scope: &mut Scope, token: &Token) -> TypecheckResult {
    let typ = match &token.info {
        TokenInfo::Ident(ident) => {
            let Some(binding) = scope.find_binding(ident) else {
                return Err(lformat!("Undeclared identifier `{}`.", ident));
            };

            match binding {
                ScopeBinding::Var(var_binding) => var_binding.typ,
                ScopeBinding::Fn(fn_binding) => fn_binding.typ,
            }
        }
        TokenInfo::Int(_) => Type::Int,
        TokenInfo::Float(_) => Type::Float,
        TokenInfo::String(_) => Type::String,
        _ => panic!(
            "[INTERNAL ERR] `Literal` node has wrong info `{:?}`",
            token.info
        ),
    };

    Ok(typ)
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
