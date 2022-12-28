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

use super::value_type::{Type, TypeInfo, TypeInfoFunction, TypeInfoPointer};

pub fn typecheck_files(files: &mut [ParsedFile]) -> Result<(), &'static str> {
    let mut queued_remaining: usize = files.iter().map(|file| file.ast.len()).sum();
    while queued_remaining != 0 {
        for queued in files.iter_mut().flat_map(|file| file.ast.iter_mut()) {
            if queued_not_ready_for_typechecking(queued) || queued.is_typechecked() {
                continue;
            }

            typecheck_queued(queued)?;
            if queued.is_typechecked() {
                queued_remaining -= 1;
            }
        }
    }

    Ok(())
}

fn queued_not_ready_for_typechecking(queued: &Queued) -> bool {
    !queued.all_dependencies_typechecked() && !matches!(queued.node.info, AstInfo::Fn(_))
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
                    typecheck_fn_signature(scope, &queued.node.token, info)?;
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

fn typecheck_fn_signature(
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

    if let Some(returns) = &mut info.returns {
        typecheck_node(interp, returns)?;

        if !matches!(returns.typ, Some(Type::Type)) {
            return Err("Expression expected to be a type for a function return type.");
        }

        let AstInfo::TypeValue(returns) = returns.info else {
            unreachable!();
        };

        return_type = Some(returns);
    }

    let fn_info = TypeInfoFunction {
        params: param_types.into_boxed_slice(),
        returns: return_type,
    };

    let fn_type = interp.get_or_create_function_type(fn_info);
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
    let interp = Interpreter::get_mut();
    typecheck_node(interp, &mut info.body)
}

fn typecheck_node(interp: &mut Interpreter, node: &mut Ast) -> Result<(), &'static str> {
    match &mut node.info {
        AstInfo::Literal => {
            let scope = &mut interp.scopes[node.scope.0];
            node.typ = typecheck_literal(scope, &node.token)?;

            if matches!(node.typ, Some(Type::Type)) {
                change_to_type_value_node(node);
            }
        }
        AstInfo::Unary(_, _) => node.typ = typecheck_unary(node)?,
        AstInfo::Binary(kind, lhs, rhs) => {
            let scope = &mut interp.scopes[node.scope.0];
            node.typ = typecheck_binary(scope, &node.token, *kind, lhs, rhs)?;
        }
        AstInfo::Block(kind, nodes) => {
            let scope = &mut interp.scopes[node.scope.0];
            typecheck_block(scope, &node.token, *kind, nodes)?;
        }
        AstInfo::Var(info) => {
            let scope = &mut interp.scopes[node.scope.0];
            typecheck_var_decl(scope, &node.token, info)?;
        }
        AstInfo::Fn(info) => todo!(),
        AstInfo::Import(info) => todo!(),
        AstInfo::TypeValue(_) => {}
    }

    Ok(())
}

type TypecheckResult = Result<Option<Type>, &'static str>;

fn typecheck_literal(scope: &mut Scope, token: &Token) -> TypecheckResult {
    let typ = match &token.info {
        TokenInfo::Ident(ident) => {
            let Some(binding) = scope.find_binding(ident) else {
                return Err(lformat!("Undeclared identifier `{}`.", ident));
            };

            match binding {
                ScopeBinding::Var(var_binding) => var_binding.typ,
                ScopeBinding::Fn(fn_binding) => fn_binding.typ,
                ScopeBinding::Type(_) => Type::Type,
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

    Ok(Some(typ))
}

fn change_to_type_value_node(node: &mut Ast) {
    let interp = Interpreter::get();
    let scope = &interp.scopes[node.scope.0];

    match &node.info {
        AstInfo::Literal => match &node.token.info {
            TokenInfo::Ident(ident) => {
                let Some(ScopeBinding::Type(typ)) = scope.find_binding(ident) else {
                    todo!("Handle non-constant type variables.");
                };

                node.info = AstInfo::TypeValue(*typ);
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn typecheck_unary(node: &mut Ast) -> TypecheckResult {
    let AstInfo::Unary(kind, expr) = &mut node.info else {
        panic!("[INTERNAL ERR] Call to `typecheck_unary` on node that isn't a `Unary` node.");
    };

    let interp = Interpreter::get_mut();

    let typ = match kind {
        AstUnaryKind::Neg => {
            typecheck_node(interp, expr)?;

            if expr.typ.is_none() {
                return Err(
                    "Expected either an `Int` or `Float` value for `-` but found no value.",
                );
            }

            let expr_type = expr.typ.unwrap();
            if expr_type != Type::Int && expr_type != Type::Float {
                return Err(lformat!(
                    "Expected either an `Int` or `Float` value for `-` but found a `{}` value.",
                    expr_type
                ));
            }

            Some(expr_type)
        }
        AstUnaryKind::Not => {
            typecheck_node(interp, expr)?;

            if expr.typ.is_none() {
                return Err("Expected a `Bool` value for `!` but found no value.");
            }

            let expr_type = expr.typ.unwrap();
            if expr_type != Type::Bool {
                return Err(lformat!(
                    "Expected a `Bool` value for `!` but found a `{}` value.",
                    expr_type
                ));
            }

            Some(expr_type)
        }
        AstUnaryKind::Ref => typecheck_ref_operator(interp, false, node)?,
        AstUnaryKind::RefMut => typecheck_ref_operator(interp, true, node)?,
        AstUnaryKind::Deref => {
            typecheck_node(interp, expr)?;
            if expr.typ.is_none() {
                return Err("Expected a pointer value for `*` but found no value.");
            }

            let expr_type = expr.typ.unwrap();
            let deref_type: Type;
            if let Type::Composite(idx) = expr_type {
                let typ = &interp.types[idx];
                if let TypeInfo::Pointer(type_info) = typ {
                    deref_type = type_info.pointee_type;
                } else {
                    return Err(lformat!(
                        "Expected a pointer value for `*` but found a `{}` value.",
                        expr_type
                    ));
                }
            } else {
                return Err(lformat!(
                    "Expected a pointer value for `*` but found a `{}` value.",
                    expr_type
                ));
            }

            Some(deref_type)
        }
        AstUnaryKind::XXXPrint => {
            typecheck_node(interp, expr)?;
            None
        }
    };

    Ok(typ)
}

fn typecheck_ref_operator(
    interp: &mut Interpreter,
    mutable_pointee: bool,
    node: &mut Ast,
) -> TypecheckResult {
    let AstInfo::Unary(_, expr) = &mut node.info else {
        panic!("[INTERNAL ERR] Call to `typecheck_ref_operator` on node that isn't a `Unary` node.");
    };

    typecheck_node(interp, expr)?;

    if expr.typ.is_none() {
        todo!("Handle void pointers.");
    }

    let expr_type = expr.typ.unwrap();
    let expr_type = if expr_type == Type::Type {
        if let AstInfo::TypeValue(type_value) = expr.info {
            let ptr_type_value = interp.get_or_create_pointer_type(TypeInfoPointer {
                mutable_pointee,
                pointee_type: type_value,
            });

            node.info = AstInfo::TypeValue(ptr_type_value);
            Type::Type
        } else {
            interp.get_or_create_pointer_type(TypeInfoPointer {
                mutable_pointee,
                pointee_type: Type::Type,
            })
        }
    } else {
        interp.get_or_create_pointer_type(TypeInfoPointer {
            mutable_pointee,
            pointee_type: expr_type,
        })
    };

    Ok(Some(expr_type))
}

fn typecheck_binary(
    scope: &mut Scope,
    token: &Token,
    kind: AstBinaryKind,
    lhs: &mut Ast,
    rhs: &mut Ast,
) -> TypecheckResult {
    let interp = Interpreter::get_mut();

    let typ = match kind {
        AstBinaryKind::Add => {
            typecheck_node(interp, lhs)?;
            typecheck_node(interp, rhs)?;

            let Some(lhs_type) = lhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `+` but found no value.");
            };

            let Some(rhs_type) = rhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `+` but found no value.");
            };

            if lhs_type != Type::Int && lhs_type != Type::Float {
                return Err(lformat!("Left hand side expression of `+` operator expected to be either an `Int` or `Float` value but found a `{}` value.", lhs_type));
            }

            if rhs_type != Type::Int && rhs_type != Type::Float {
                return Err(lformat!("Right hand side expression of `+` operator expected to be either an `Int` or `Float` value but found a `{}` value.", rhs_type));
            }

            if lhs_type != rhs_type {
                return Err(lformat!("Both expressions of `+` operator must be the same type but found `{}` and `{}` values.", lhs_type, rhs_type));
            }

            Some(lhs_type)
        }
        AstBinaryKind::Sub => {
            typecheck_node(interp, lhs)?;
            typecheck_node(interp, rhs)?;

            let Some(lhs_type) = lhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `-` but found no value.");
            };

            let Some(rhs_type) = rhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `-` but found no value.");
            };

            if lhs_type != Type::Int && lhs_type != Type::Float {
                return Err(lformat!("Left hand side expression of `-` operator expected to be either an `Int` or `Float` value but found a `{}` value.", lhs_type));
            }

            if rhs_type != Type::Int && rhs_type != Type::Float {
                return Err(lformat!("Right hand side expression of `-` operator expected to be either an `Int` or `Float` value but found a `{}` value.", rhs_type));
            }

            if lhs_type != rhs_type {
                return Err(lformat!("Both expressions of `-` operator must be the same type but found `{}` and `{}` values.", lhs_type, rhs_type));
            }

            Some(lhs_type)
        }
        AstBinaryKind::Mul => {
            typecheck_node(interp, lhs)?;
            typecheck_node(interp, rhs)?;

            let Some(lhs_type) = lhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `*` but found no value.");
            };

            let Some(rhs_type) = rhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `*` but found no value.");
            };

            if lhs_type != Type::Int && lhs_type != Type::Float {
                return Err(lformat!("Left hand side expression of `*` operator expected to be either an `Int` or `Float` value but found a `{}` value.", lhs_type));
            }

            if rhs_type != Type::Int && rhs_type != Type::Float {
                return Err(lformat!("Right hand side expression of `*` operator expected to be either an `Int` or `Float` value but found a `{}` value.", rhs_type));
            }

            if lhs_type != rhs_type {
                return Err(lformat!("Both expressions of `*` operator must be the same type but found `{}` and `{}` values.", lhs_type, rhs_type));
            }

            Some(lhs_type)
        }
        AstBinaryKind::Div => {
            typecheck_node(interp, lhs)?;
            typecheck_node(interp, rhs)?;

            let Some(lhs_type) = lhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `/` but found no value.");
            };

            let Some(rhs_type) = rhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `/` but found no value.");
            };

            if lhs_type != Type::Int && lhs_type != Type::Float {
                return Err(lformat!("Left hand side expression of `/` operator expected to be either an `Int` or `Float` value but found a `{}` value.", lhs_type));
            }

            if rhs_type != Type::Int && rhs_type != Type::Float {
                return Err(lformat!("Right hand side expression of `/` operator expected to be either an `Int` or `Float` value but found a `{}` value.", rhs_type));
            }

            if lhs_type != rhs_type {
                return Err(lformat!("Both expressions of `/` operator must be the same type but found `{}` and `{}` values.", lhs_type, rhs_type));
            }

            Some(lhs_type)
        }
        AstBinaryKind::Mod => {
            typecheck_node(interp, lhs)?;
            typecheck_node(interp, rhs)?;

            let Some(lhs_type) = lhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `%` but found no value.");
            };

            let Some(rhs_type) = rhs.typ else {
                return Err("Expected either an `Int` or `Float` value for `%` but found no value.");
            };

            if lhs_type != Type::Int {
                return Err(lformat!("Left hand side expression of `%` operator expected to be an `Int` but found a `{}` value.", lhs_type));
            }

            if rhs_type != Type::Int {
                return Err(lformat!("Right hand side expression of `%` operator expected to be an `Int` but found a `{}` value.", rhs_type));
            }

            Some(lhs_type)
        }
        AstBinaryKind::Assign => {
            todo!()
        }
        AstBinaryKind::Call => {
            typecheck_node(interp, lhs)?;
            typecheck_node(interp, rhs)?;

            let fn_info = if let Some(Type::Composite(lhs_type)) = lhs.typ {
                let type_info = &interp.types[lhs_type];
                if let TypeInfo::Function(fn_info) = type_info {
                    fn_info
                } else {
                    return Err(lformat!("Expected left hand side of function call to be a function value but found a `{}` value.", lhs.typ.unwrap()));
                }
            } else {
                return Err("Expected left hand side of function call to be a function value but found no found.");
            };

            let AstInfo::Block(AstBlockKind::Args, args) = &rhs.info else {
                panic!("[INTERNAL ERR] `rhs` node of `Call` node is not a `Params` node.");
            };

            if fn_info.params.len() != args.len() {
                return Err(lformat!("Incorrect number of arguments for function call. Expected {} arguments but was given {}.", fn_info.params.len(), args.len()));
            }

            for (i, (&expected, given)) in fn_info
                .params
                .iter()
                .zip(args.iter().map(|arg| {
                    arg.typ
                        .expect("[INTERNAL ERR] function argument doesn't have a type.")
                }))
                .enumerate()
            {
                if given != expected {
                    return Err(lformat!("Argument {} of function call expected to be a `{}` value but was a `{}` value.", i, expected, given));
                }
            }

            fn_info.returns
        }
        AstBinaryKind::Subscript => {
            todo!()
        }
        AstBinaryKind::Param => {
            unreachable!("`Param` nodes get special handling when typechecking functions.");
        }
    };

    Ok(typ)
}

fn typecheck_block(
    scope: &mut Scope,
    token: &Token,
    kind: AstBlockKind,
    nodes: &mut [Ast],
) -> Result<(), &'static str> {
    let interp = Interpreter::get_mut();

    for node in nodes {
        typecheck_node(interp, node)?;
    }

    Ok(())
}
