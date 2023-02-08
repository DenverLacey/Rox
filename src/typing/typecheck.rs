use crate::{
    canon::scoping::{FunctionBinding, Scope, ScopeBinding, VariableBinding},
    interp::{Interpreter, ParsedFile},
    ir::ast::{
        Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoForControl, AstInfoStruct,
        AstInfoTypeSignature, AstInfoVar, AstOptionalKind, AstUnaryKind, Queued, QueuedProgress,
    },
    parsing::tokenization::{CodeLocation, Token, TokenInfo},
    typing::value_type::TypeKind,
    util::errors::{Result, SourceError, SourceError2},
};

use super::value_type::{
    StructField, Type, TypeInfo, TypeInfoFunction, TypeInfoPointer, TypeInfoStruct,
};

pub fn typecheck_program(files: &mut [ParsedFile]) -> Result<()> {
    let mut t = Typechecker::default();

    loop {
        let mut all_typechecked = true;

        for queued in files.iter_mut().flat_map(|file| file.ast.iter_mut()) {
            if !queued_ready_for_typecheck(queued) {
                all_typechecked = false;
                continue;
            } else if queued.is_typechecked() {
                continue;
            }

            t.typecheck_queued(queued)?;
            if !queued.is_typechecked() {
                all_typechecked = false;
            }
        }

        if all_typechecked {
            break;
        }
    }

    Ok(())
}

pub fn queued_ready_for_typecheck(queued: &Queued) -> bool {
    let interp = Interpreter::get();

    let deps = if matches!(queued.node.info, AstInfo::Fn(_))
        && queued.progress == QueuedProgress::PartiallyTypechecked
    {
        &queued.inner_deps
    } else {
        &queued.deps
    };

    for dep in deps {
        let dep = &interp.parsed_files[dep.parsed_file_idx].ast[dep.queued_idx];
        if dep.progress < QueuedProgress::PartiallyTypechecked {
            return false;
        }
    }

    true
}

#[derive(Default)]
struct Typechecker {
    returns: Vec<Option<Type>>,
}

type TypecheckResult = Result<Option<Type>>;

impl Typechecker {
    fn typecheck_queued(&mut self, queued: &mut Queued) -> Result<()> {
        let interp = Interpreter::get_mut();

        match &mut queued.node.info {
            AstInfo::Var(info) => {
                let scope = &mut interp.scopes[queued.node.scope.0];
                self.typecheck_var_decl(scope, info)?;
                queued.progress = QueuedProgress::Typechecked;
            }
            AstInfo::Fn(info) => {
                let scope = &mut interp.scopes[queued.node.scope.0];

                match queued.progress {
                    QueuedProgress::DependenciesFound => {
                        self.typecheck_fn_signature(scope, &queued.node.token, info)?;
                        queued.progress = QueuedProgress::PartiallyTypechecked;
                    }
                    QueuedProgress::PartiallyTypechecked => {
                        self.typecheck_fn_body(scope, &queued.node.token, info)?;
                        queued.progress = QueuedProgress::Typechecked;
                    }
                    _ => panic!(
                        "[INTERNAL ERR] Function node reached `typecheck_queued` at phase {:?}",
                        queued.progress
                    ),
                }
            }
            AstInfo::Struct(info) => {
                let scope = &mut interp.scopes[queued.node.scope.0];
                self.typecheck_struct_decl(scope, &queued.node.token, info)?;
                queued.progress = QueuedProgress::Typechecked;
            }
            AstInfo::Import(info) => todo!(),
            _ => {
                self.typecheck_node(interp, &mut queued.node)?;
                queued.progress = QueuedProgress::Typechecked;
            }
        }

        Ok(())
    }

    fn typecheck_var_decl(&mut self, scope: &mut Scope, info: &mut AstInfoVar) -> Result<()> {
        let interp = Interpreter::get_mut();

        match info.initializers.as_mut_slice() {
            [] => self.typecheck_var_decl_no_initializer(
                interp,
                scope,
                info.mutable,
                &mut info.targets,
            ),
            [init] => self.typecheck_var_decl_one_initializer(
                interp,
                scope,
                info.mutable,
                &mut info.targets,
                init,
            ),
            inits => self.typecheck_var_decl_many_initializers(
                interp,
                scope,
                info.mutable,
                &mut info.targets,
                inits,
            ),
        }
    }

    fn typecheck_var_decl_no_initializer(
        &mut self,
        interp: &mut Interpreter,
        scope: &mut Scope,
        mutable: bool,
        targets: &mut [Ast],
    ) -> Result<()> {
        for target in targets {
            let AstInfo::Binary(AstBinaryKind::ConstrainedVarDeclTarget, ident, type_constraint) = &mut target.info else {
                panic!("[INTERNAL ERR] target in `Var` node with no initializers is not a `ConstrainedVarDeclTarget` node.");
            };

            let Token { loc: target_loc, info: TokenInfo::Ident(ident) } = &ident.token else {
                panic!("[INTERNAL ERR] lhs node of `ConstrainedVarDeclTarget` is not an `Ident` node.");
            };

            self.typecheck_node(interp, type_constraint)?;
            let  AstInfo::TypeValue(mut type_constraint) = type_constraint.info else {
                return Err(SourceError::new("Type constraint expression is not a type.", type_constraint.token.loc, "This is not a type.").into());
            };

            type_constraint.mutable = mutable;

            let binding = ScopeBinding::Var(VariableBinding {
                typ: type_constraint,
                is_global: scope.is_global(),
                addr: 0,
            });
            scope.add_binding(
                ident.clone(),
                binding,
                *target_loc,
                format!("Redeclaration of `{}`", ident), // @TODO: Improve error
            )?;
        }

        Ok(())
    }

    fn typecheck_variable_for_binding(
        &mut self,
        scope: &mut Scope,
        mutable: bool,
        target: &mut Ast,
        mut init_type: Type,
    ) -> Result<()> {
        match &mut target.info {
            AstInfo::Literal => {
                let Token { loc: target_loc, info: TokenInfo::Ident(ident) } = &target.token else {
                    panic!("[INTERNAL ERR] target node is a `Literal` node but not an `Ident` node.");
                };

                init_type.mutable = mutable;

                let binding = ScopeBinding::Var(VariableBinding {
                    typ: init_type,
                    is_global: scope.is_global(),
                    addr: 0,
                });
                scope.add_binding(
                    ident.clone(),
                    binding,
                    *target_loc,
                    format!("Redeclaration of `{}`", ident), // @TODO: Improve error
                )?;
            }
            AstInfo::Binary(AstBinaryKind::ConstrainedVarDeclTarget, ident, type_constraint) => {
                todo!()
            }
            _ => panic!(
                "[INTERNAL ERR] target node is not an `Ident` or `ConstrainedVarDeclTarget` node."
            ),
        }

        Ok(())
    }

    fn typecheck_var_decl_one_initializer(
        &mut self,
        interp: &mut Interpreter,
        scope: &mut Scope,
        mutable: bool,
        targets: &mut [Ast],
        init: &mut Ast,
    ) -> Result<()> {
        self.typecheck_node(interp, init)?;
        let Some(init_type) = init.typ else {
            return Err(SourceError::new("Variable initializer has no type.", init.token.loc, "Cannot initialize a variable with typeless expression.").into());
        };

        for target in targets {
            self.typecheck_variable_for_binding(scope, mutable, target, init_type)?;
        }

        Ok(())
    }

    fn typecheck_var_decl_many_initializers(
        &mut self,
        interp: &mut Interpreter,
        scope: &mut Scope,
        mutable: bool,
        targets: &mut [Ast],
        inits: &mut [Ast],
    ) -> Result<()> {
        assert_eq!(
            targets.len(),
            inits.len(),
            "[INTERNAL ERR] number of targets and initializers aren't equal."
        );
        for (target, init) in targets.iter_mut().zip(inits) {
            self.typecheck_node(interp, init)?;
            let Some(init_type) = init.typ else {
                return Err(SourceError::new("Variable initializer has no type.", init.token.loc, "Cannot initialize a variable with typeless expression.").into());
            };

            self.typecheck_variable_for_binding(scope, mutable, target, init_type)?;
        }

        Ok(())
    }

    fn typecheck_fn_signature(
        &mut self,
        scope: &mut Scope,
        token: &Token,
        info: &mut AstInfoFn,
    ) -> Result<()> {
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

            self.typecheck_node(interp, typ)?;
            if typ.typ != Some(Type::of(TypeKind::Type)) {
                return Err(SourceError::new(
                    "Non-type expression in function parameter type annotation.",
                    typ.token.loc,
                    "This should be a type.",
                )
                .into());
            }

            let AstInfo::TypeValue(param_type) = typ.info else {
                unreachable!();
            };

            // @TODO: Mutable parameters?
            let param_binding = ScopeBinding::Var(VariableBinding {
                typ: param_type,
                is_global: false,
                addr: 0,
            });

            let param_scope = &mut interp.scopes[info.params.scope.0];
            param_scope.add_binding(
                param_name.clone(),
                param_binding,
                param.token.loc,
                format!(
                    "There is already a parameter with the name `{}`.",
                    param_name
                ),
            )?;

            param_types.push(param_type);
        }

        if let Some(returns) = &mut info.returns {
            self.typecheck_node(interp, returns)?;

            if returns.typ != Some(Type::of(TypeKind::Type)) {
                return Err(SourceError::new(
                    "Expression expected to be a type for a function return type.",
                    returns.token.loc,
                    "This should be a type.",
                )
                .into());
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
        info.id = Some(fn_id);

        let binding = ScopeBinding::Fn(FunctionBinding {
            id: fn_id,
            typ: fn_type,
        });
        scope.add_binding(
            ident.clone(),
            binding,
            token.loc,
            format!("Redeclaration of `{}`.", ident),
        )?;

        Ok(())
    }

    fn typecheck_fn_body(
        &mut self,
        scope: &mut Scope,
        token: &Token,
        info: &mut AstInfoFn,
    ) -> Result<()> {
        let interp = Interpreter::get_mut();

        let id = info
            .id
            .expect("[INTERNAL ERR] funciton doesn't have an assigned FuncID.");
        let fn_info = &interp.funcs[id.0];
        let TypeKind::Composite(fn_type_idx) = fn_info.typ.kind else {
            panic!("[INTERNAL ERR] type of function {:?} is not a composite type.", id);
        };
        let fn_type = &interp.types[fn_type_idx];
        let TypeInfo::Function(fn_type) = fn_type else {
            panic!("[INTERNAL ERR] type of function {:?} is not a function type.", id);
        };

        self.returns.push(fn_type.returns);
        self.typecheck_node(interp, &mut info.body)?;
        self.returns.pop().expect("[INTERNAL ERR] Expected to pop off return type of typechecked function from `returns` but it is empty.");

        Ok(())
    }

    fn typecheck_struct_decl(
        &mut self,
        scope: &mut Scope,
        token: &Token,
        info: &mut AstInfoStruct,
    ) -> Result<()> {
        let interp = Interpreter::get_mut();

        let TokenInfo::Ident(ident) = &info.ident.token.info else {
            panic!("[INTERNAL ERR] `ident` node of `Struct` node not an `Ident` node.");
        };

        let AstInfo::Block(AstBlockKind::Fields, field_nodes) = &mut info.body.info else {
            panic!("[INTERNAL ERR] `fields` node of `Struct` node not a `Fields` node.");
        };

        let mut fields = vec![];
        for field in field_nodes {
            let AstInfo::Binary(AstBinaryKind::Field, ident, typ) = &mut field.info else {
                panic!("[INTERNAL ERR] `field` node of `Fields` node not a `Field` node.");
            };

            let TokenInfo::Ident(field_name) = &ident.token.info else {
                panic!("[INTERNAL ERR] `ident` node of `Field` node not an `Ident` node.");
            };

            self.typecheck_node(interp, typ)?;
            let AstInfo::TypeValue(field_type) = &typ.info else {
                return Err(SourceError::new("Expected a type signature.", typ.token.loc, "This should be a type.").into());
            };

            let field = StructField {
                name: field_name.clone(),
                typ: *field_type,
            };

            fields.push(field);
        }

        let struct_info = TypeInfoStruct {
            name: ident.clone(),
            fields: fields.into_boxed_slice(),
        };

        let struct_type = interp.create_struct_type(struct_info);
        let binding = ScopeBinding::Type(struct_type);
        scope.add_binding(
            ident.clone(),
            binding,
            token.loc,
            format!("Redeclaration of `{}`.", ident),
        )?;

        Ok(())
    }

    fn typecheck_node(&mut self, interp: &mut Interpreter, node: &mut Ast) -> Result<()> {
        match &mut node.info {
            AstInfo::Literal => {
                let scope = &mut interp.scopes[node.scope.0];
                node.typ = self.typecheck_literal(scope, &node.token)?;

                if node.typ == Some(Type::of(TypeKind::Type)) {
                    self.change_to_type_value_node(node);
                }
            }
            AstInfo::Unary(_, _) => node.typ = self.typecheck_unary(node)?,
            AstInfo::Binary(kind, lhs, rhs) => {
                let scope = &mut interp.scopes[node.scope.0];
                node.typ = self.typecheck_binary(scope, &node.token, *kind, lhs, rhs)?;
            }
            AstInfo::Optional(kind, sub) => {
                self.typecheck_optional(*kind, &node.token, sub.as_mut().map(|e| e.as_mut()))?
            }
            AstInfo::Block(kind, nodes) => {
                let scope = &mut interp.scopes[node.scope.0];
                self.typecheck_block(scope, &node.token, *kind, nodes)?;
            }
            AstInfo::Var(info) => {
                let scope = &mut interp.scopes[node.scope.0];
                self.typecheck_var_decl(scope, info)?;
            }
            AstInfo::Fn(info) => todo!(),
            AstInfo::Import(info) => todo!(),
            AstInfo::Struct(info) => todo!(),
            AstInfo::TypeValue(_) => {}
            AstInfo::TypeSignature(sig) => match sig.as_mut() {
                AstInfoTypeSignature::Function(params, returns) => {
                    self.typecheck_node(interp, params)?;

                    let AstInfo::Block(AstBlockKind::Params, params) = &params.info else {
                        panic!("[INTERNAL ERR] `params` node isn't a `Params` node in type signature.");
                    };

                    let param_types = params.iter().map(|param| {
                        let AstInfo::TypeValue(param_type) = &param.info else {
                            return Err(SourceError::new("Parameter in function type signature not a type.", param.token.loc, "This should be a type.").into());
                        };
                        Ok(*param_type)
                    }).collect::<Result<Vec<_>>>()?.into_boxed_slice();

                    let return_type = if let Some(returns) = returns {
                        self.typecheck_node(interp, returns)?;
                        let AstInfo::TypeValue(return_type) = &returns.info else {
                            return Err(SourceError::new("Return type expression of function type signature not a type.", returns.token.loc, "This should be a type.").into());
                        };

                        Some(*return_type)
                    } else {
                        None
                    };

                    let fn_type = TypeInfoFunction {
                        params: param_types,
                        returns: return_type,
                    };

                    let fn_type = interp.get_or_create_function_type(fn_type);
                    node.typ = Some(Type::of(TypeKind::Type));
                    node.info = AstInfo::TypeValue(fn_type);
                }
            },
            AstInfo::If(info) => {
                self.typecheck_node(interp, &mut info.condition)?;

                if info
                    .condition
                    .typ
                    .filter(|t| t.kind == TypeKind::Bool)
                    .is_none()
                {
                    return Err(SourceError::new(
                        "Type mismatch!",
                        info.condition.token.loc,
                        "Expected a `Bool` value here.",
                    )
                    .into());
                }

                self.typecheck_node(interp, &mut info.then_block)?;

                if let Some(else_block) = &mut info.else_block {
                    self.typecheck_node(interp, else_block)?;
                }
            }
            AstInfo::For(info) => {
                match &mut info.control.info {
                    AstInfo::ForControl(control_info) => self.typecheck_c_like_for_loop(interp, control_info, &mut info.body)?,
                    AstInfo::Binary(AstBinaryKind::In, it, seq) => todo!(),
                    _ => {
                        self.typecheck_for_loop_condition(&mut info.control, &mut info.body)?;
                    }
                }
            }
            AstInfo::ForControl(_) => panic!("[INTERNAL ERR] `ForControl` not being handled in `For` branch of `typecheck_node`."),
        }

        Ok(())
    }

    fn typecheck_literal(&mut self, scope: &mut Scope, token: &Token) -> TypecheckResult {
        let typ = match &token.info {
            TokenInfo::Ident(ident) => {
                let Some(binding) = scope.find_binding(ident) else {
                    return Err(SourceError::new("Undeclared identifier.", token.loc, format!("No variable `{}` declared.", ident)).into());
                };

                match binding {
                    ScopeBinding::Var(var_binding) => var_binding.typ,
                    ScopeBinding::Fn(fn_binding) => fn_binding.typ,
                    ScopeBinding::Type(_) => Type::of(TypeKind::Type),
                }
            }
            TokenInfo::Bool(_) => Type::of(TypeKind::Bool),
            TokenInfo::Char(_) => Type::of(TypeKind::Char),
            TokenInfo::Int(_) => Type::of(TypeKind::Int),
            TokenInfo::Float(_) => Type::of(TypeKind::Float),
            TokenInfo::String(_) => Type::of(TypeKind::String),
            _ => panic!(
                "[INTERNAL ERR] `Literal` node has wrong info `{:?}`",
                token.info
            ),
        };

        Ok(Some(typ))
    }

    fn change_to_type_value_node(&mut self, node: &mut Ast) {
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

    fn typecheck_unary(&mut self, node: &mut Ast) -> TypecheckResult {
        let AstInfo::Unary(kind, expr) = &mut node.info else {
            panic!("[INTERNAL ERR] Call to `typecheck_unary` on node that isn't a `Unary` node.");
        };

        let interp = Interpreter::get_mut();

        let typ = match kind {
            AstUnaryKind::Neg => {
                self.typecheck_node(interp, expr)?;

                if expr.typ.is_none() {
                    return Err(SourceError::new(
                        "Type Mismatch.",
                        expr.token.loc,
                        format!(
                            "Type should be `{}` or `{}` but found no type.",
                            TypeKind::Int,
                            TypeKind::Float
                        ),
                    )
                    .into());
                }

                let expr_type = expr.typ.unwrap();
                if expr_type.kind != TypeKind::Int && expr_type.kind != TypeKind::Float {
                    return Err(SourceError::new(
                        "Type Mismatch.",
                        expr.token.loc,
                        format!(
                            "Type should be `{}` or `{}` but found `{}`.",
                            TypeKind::Int,
                            TypeKind::Float,
                            expr_type
                        ),
                    )
                    .into());
                }

                Some(expr_type)
            }
            AstUnaryKind::Not => {
                self.typecheck_node(interp, expr)?;

                if expr.typ.is_none() {
                    return Err(SourceError::new(
                        "Type Mismatch.",
                        expr.token.loc,
                        format!("Type should be `{}` but found no type.", TypeKind::Bool),
                    )
                    .into());
                }

                let expr_type = expr.typ.unwrap();
                if expr_type.kind != TypeKind::Bool {
                    return Err(SourceError::new(
                        "Type Mismatch.",
                        expr.token.loc,
                        format!(
                            "Type should be `{}` but found `{}`.",
                            TypeKind::Bool,
                            expr_type
                        ),
                    )
                    .into());
                }

                Some(expr_type)
            }
            AstUnaryKind::Ref => self.typecheck_ref_operator(interp, false, node)?,
            AstUnaryKind::RefMut => self.typecheck_ref_operator(interp, true, node)?,
            AstUnaryKind::Deref => {
                self.typecheck_node(interp, expr)?;
                if expr.typ.is_none() {
                    return Err(SourceError::new(
                        "Dereference of non-pointer value.",
                        expr.token.loc,
                        "Type should be a pointer type but found no type.",
                    )
                    .into());
                }

                let expr_type = expr.typ.unwrap();
                let deref_type: Type;
                if let TypeKind::Composite(idx) = expr_type.kind {
                    let typ = &interp.types[idx];
                    if let TypeInfo::Pointer(type_info) = typ {
                        deref_type = type_info.pointee_type;
                    } else {
                        return Err(SourceError::new(
                            "Dereference of non-pointer value.",
                            expr.token.loc,
                            format!("Type should be a pointer type but found `{}`.", expr_type),
                        )
                        .into());
                    }
                } else {
                    return Err(SourceError::new(
                        "Dereference of non-pointer value.",
                        expr.token.loc,
                        format!("Type should be a pointer type but found `{}`.", expr_type),
                    )
                    .into());
                }

                Some(deref_type)
            }
            AstUnaryKind::XXXPrint => {
                self.typecheck_node(interp, expr)?;
                None
            }
        };

        Ok(typ)
    }

    fn typecheck_ref_operator(
        &mut self,
        interp: &mut Interpreter,
        mutable_pointee: bool,
        node: &mut Ast,
    ) -> TypecheckResult {
        let AstInfo::Unary(_, expr) = &mut node.info else {
            panic!("[INTERNAL ERR] Call to `typecheck_ref_operator` on node that isn't a `Unary` node.");
        };

        self.typecheck_node(interp, expr)?;

        if expr.typ.is_none() {
            todo!("Handle void pointers.");
        }

        let mut expr_type = expr.typ.unwrap();
        let expr_type = if expr_type.kind == TypeKind::Type {
            if let AstInfo::TypeValue(mut type_value) = expr.info {
                type_value.mutable = mutable_pointee;

                let ptr_type_value = interp.get_or_create_pointer_type(TypeInfoPointer {
                    pointee_type: type_value,
                });

                node.info = AstInfo::TypeValue(ptr_type_value);
                Type::of(TypeKind::Type)
            } else {
                interp.get_or_create_pointer_type(TypeInfoPointer {
                    pointee_type: Type::of(TypeKind::Type),
                })
            }
        } else {
            if (!expr_type.mutable) && mutable_pointee {
                return Err(SourceError::new(
                    "Mutability mismatch.",
                    expr.token.loc,
                    format!(
                        "Cannot take a mutable reference to a `{}` value.",
                        expr_type
                    ),
                )
                .into());
            }

            expr_type.mutable = mutable_pointee;
            interp.get_or_create_pointer_type(TypeInfoPointer {
                pointee_type: expr_type,
            })
        };

        Ok(Some(expr_type))
    }

    fn typecheck_binary(
        &mut self,
        scope: &mut Scope,
        token: &Token,
        kind: AstBinaryKind,
        lhs: &mut Ast,
        rhs: &mut Ast,
    ) -> TypecheckResult {
        let interp = Interpreter::get_mut();

        let typ = match kind {
            AstBinaryKind::Add
            | AstBinaryKind::Sub
            | AstBinaryKind::Mul
            | AstBinaryKind::Div
            | AstBinaryKind::Mod => {
                self.typecheck_node(interp, lhs)?;
                self.typecheck_node(interp, rhs)?;

                let Some(lhs_type) = lhs.typ else {
                        return Err(SourceError::new("Type Mismatch.", lhs.token.loc, format!("Type should be `{}` or `{}` but found no type.", TypeKind::Int, TypeKind::Float)).into());
                    };

                let Some(rhs_type) = rhs.typ else {
                        return Err(SourceError::new("Type Mismatch.", rhs.token.loc, format!("Type should be `{}` or `{}` but found no type.", TypeKind::Int, TypeKind::Float)).into());
                    };

                if lhs_type.kind != TypeKind::Int && lhs_type.kind != TypeKind::Float {
                    return Err(SourceError::new(
                        "Type Mismatch.",
                        lhs.token.loc,
                        format!(
                            "Type should be `{}` or `{}` but found `{}`.",
                            TypeKind::Int,
                            TypeKind::Float,
                            lhs_type
                        ),
                    )
                    .into());
                }

                if rhs_type.kind != TypeKind::Int && rhs_type.kind != TypeKind::Float {
                    return Err(SourceError::new(
                        "Type Mismatch.",
                        rhs.token.loc,
                        format!(
                            "Type should be `{}` or `{}` but found `{}`.",
                            TypeKind::Int,
                            TypeKind::Float,
                            rhs_type
                        ),
                    )
                    .into());
                }

                if lhs_type.kind != rhs_type.kind {
                    return Err(SourceError2::new(
                        "Type Mistmatch.",
                        lhs.token.loc,
                        format!("`{}`.", lhs_type),
                        rhs.token.loc,
                        format!("`{}`", rhs_type),
                    )
                    .into());
                }

                Some(lhs_type)
            }
            AstBinaryKind::Lt | AstBinaryKind::Le | AstBinaryKind::Gt | AstBinaryKind::Ge => {
                self.typecheck_node(interp, lhs)?;
                self.typecheck_node(interp, rhs)?;

                let Some(lhs_type) = lhs.typ else {
                        return Err(SourceError::new("Type Mismatch.", lhs.token.loc, format!("Type should be `{}` or `{}` but found no type.", TypeKind::Int, TypeKind::Float)).into());
                    };

                let Some(rhs_type) = rhs.typ else {
                        return Err(SourceError::new("Type Mismatch.", rhs.token.loc, format!("Type should be `{}` or `{}` but found no type.", TypeKind::Int, TypeKind::Float)).into());
                    };

                if lhs_type.kind != TypeKind::Int && lhs_type.kind != TypeKind::Float {
                    return Err(SourceError::new(
                        "Type Mismatch.",
                        lhs.token.loc,
                        format!(
                            "Type should be `{}` or `{}` but found `{}`.",
                            TypeKind::Int,
                            TypeKind::Float,
                            lhs_type
                        ),
                    )
                    .into());
                }

                if rhs_type.kind != TypeKind::Int && rhs_type.kind != TypeKind::Float {
                    return Err(SourceError::new(
                        "Type Mismatch.",
                        rhs.token.loc,
                        format!(
                            "Type should be `{}` or `{}` but found `{}`.",
                            TypeKind::Int,
                            TypeKind::Float,
                            rhs_type
                        ),
                    )
                    .into());
                }

                if lhs_type.kind != rhs_type.kind {
                    return Err(SourceError2::new(
                        "Type Mistmatch.",
                        lhs.token.loc,
                        format!("`{}`.", lhs_type),
                        rhs.token.loc,
                        format!("`{}`", rhs_type),
                    )
                    .into());
                }

                Some(Type::of(TypeKind::Bool))
            }
            AstBinaryKind::Assign => {
                self.typecheck_node(interp, lhs)?;
                self.typecheck_node(interp, rhs)?;

                let Some(lhs_type) = lhs.typ else {
                    return Err(SourceError::new("Bad assignment target.", lhs.token.loc, "This should have a type but it doesn't.").into());
                };

                let Some(rhs_type) = rhs.typ else {
                    return Err(SourceError::new("Type Mistmatch.", rhs.token.loc, "This should have a type but it doesn't.").into());
                };

                if !lhs_type.mutable {
                    return Err(SourceError::new(
                        "Assignment to immutable variable.",
                        lhs.token.loc,
                        "Cannot assign to immutable variable.",
                    )
                    .into());
                }

                if lhs_type.kind != rhs_type.kind {
                    return Err(SourceError2::new(
                        "Type mismatch.",
                        lhs.token.loc,
                        format!("`{}`.", lhs_type),
                        rhs.token.loc,
                        format!("`{}`", rhs_type),
                    )
                    .into());
                }

                None
            }
            AstBinaryKind::Call => {
                self.typecheck_node(interp, lhs)?;
                self.typecheck_node(interp, rhs)?;

                let AstInfo::Block(AstBlockKind::Args, args) = &rhs.info else {
                    panic!("[INTERNAL ERR] `rhs` node of `Call` node is not a `Params` node.");
                };

                let return_type: Option<Type>;

                match lhs.typ {
                    Some(Type {
                        mutable: _,
                        kind: TypeKind::Composite(lhs_type),
                    }) => {
                        let type_info = &interp.types[lhs_type];
                        if let TypeInfo::Function(fn_info) = type_info {
                            self.typecheck_arguments(
                                rhs.token.loc,
                                fn_info.params.iter().copied(),
                                args,
                            )?;
                            return_type = fn_info.returns;
                        } else {
                            return Err(SourceError::new(
                                format!("Cannot call `{}`", TypeKind::Composite(lhs_type)),
                                lhs.token.loc,
                                "This isn't a callable value.",
                            )
                            .into());
                        }
                    }
                    Some(Type {
                        mutable: _,
                        kind: TypeKind::Type,
                    }) => {
                        let AstInfo::TypeValue(Type { mutable: _, kind: TypeKind::Composite(lhs_type) }) = &lhs.info else {
                            panic!("[INTERNAL ERR] lhs of `Call` node is of type `Type` but is not a `TypeValue` node.");
                        };

                        let type_info = &interp.types[*lhs_type];
                        if let TypeInfo::Struct(struct_info) = type_info {
                            self.typecheck_arguments(
                                rhs.token.loc,
                                struct_info.fields.iter().map(|f| f.typ),
                                args,
                            )?;
                            return_type = Some(Type::of(TypeKind::Composite(*lhs_type)));
                        } else {
                            return Err(SourceError::new(
                                format!("Cannot call `{}`", TypeKind::Composite(*lhs_type)),
                                lhs.token.loc,
                                "This isn't a callable value.",
                            )
                            .into());
                        }
                    }
                    Some(lhs_type) => {
                        return Err(SourceError::new(
                            format!("Cannot call `{}`", lhs_type),
                            lhs.token.loc,
                            "This isn't a callable value.",
                        )
                        .into());
                    }
                    None => {
                        return Err(SourceError::new(
                            "Cannot call typeless value",
                            lhs.token.loc,
                            "This isn't a callable value.",
                        )
                        .into());
                    }
                }

                return_type
            }
            AstBinaryKind::Subscript => {
                todo!()
            }
            AstBinaryKind::MemberAccess => {
                self.typecheck_node(interp, lhs)?;

                let result_type: Type;
                match lhs
                    .typ
                    .expect("[INTERNAL ERR] lhs of `MemberAccess` node does not have a type.")
                    .kind
                {
                    TypeKind::Type => todo!(),
                    TypeKind::Composite(idx) => {
                        result_type = self.typecheck_member_access_composite(
                            interp,
                            idx,
                            rhs,
                            lhs.token.loc,
                        )?;
                    }
                    _ => {
                        return Err(SourceError::new(
                            "Cannot access member of this type.",
                            lhs.token.loc,
                            format!(
                                "This is of type `{}` which doesn't support member access.",
                                lhs.typ.unwrap()
                            ),
                        )
                        .into())
                    }
                }

                Some(result_type)
            }
            AstBinaryKind::Param => {
                unreachable!("`Param` nodes get special handling when typechecking functions.");
            }
            AstBinaryKind::ConstrainedVarDeclTarget => {
                unreachable!("`ConstrainedVarDeclTarget` nodes get special handling when typechecking variable declarations.");
            }
            AstBinaryKind::Field => {
                unreachable!(
                    "`Field` nodes get special handling when typechecking struct declarations."
                );
            }
            AstBinaryKind::In => todo!(),
        };

        Ok(typ)
    }

    fn typecheck_arguments(
        &mut self,
        call_loc: CodeLocation,
        params: impl ExactSizeIterator<Item = Type>,
        args: &[Ast],
    ) -> Result<()> {
        if params.len() != args.len() {
            if args.is_empty() {
                return Err(SourceError::new(
                    "Incorrect number of arguments.",
                    call_loc,
                    format!("Expected {} argument(s) but was given 0.", params.len()),
                )
                .into());
            } else {
                let first_bad = std::cmp::min(params.len(), args.len());
                let arg = &args[first_bad];
                return Err(SourceError::new(
                    "Incorrect number of arguments.",
                    arg.token.loc,
                    format!(
                        "Expected {} argument(s) but was given {}.",
                        params.len(),
                        args.len()
                    ),
                )
                .into());
            }
        }

        for (arg, expected) in args.iter().zip(params) {
            let given = arg
                .typ
                .expect("[INTERNAL ERR] argument doesn't have a type.");

            if given != expected {
                return Err(SourceError::new(
                    "Type mismatch.",
                    arg.token.loc,
                    format!("Type should be `{}` but is `{}`.", expected, given),
                )
                .into());
            }
        }

        Ok(())
    }

    fn typecheck_member_access_composite(
        &mut self,
        interp: &Interpreter,
        composite_idx: usize,
        member: &Ast,
        object_location: CodeLocation,
    ) -> Result<Type> {
        let typ = &interp.types[composite_idx];
        let result = match typ {
            TypeInfo::Struct(info) => {
                let TokenInfo::Ident(member_ident) = &member.token.info else {
                    panic!("[INTERNAL ERR] rhs node of `MemberAccess` node was not an `Ident` node.");
                };

                let field = info
                    .fields
                    .iter()
                    .find(|f| f.name == *member_ident)
                    .ok_or_else(|| {
                        SourceError::new(
                            "Unknown field",
                            member.token.loc,
                            format!(
                                "There is no field `{}` for type `{}`.",
                                member_ident,
                                TypeKind::Composite(composite_idx)
                            ),
                        )
                    })?;

                field.typ
            }
            _ => {
                return Err(SourceError::new(
                    "Cannot access member of this type.",
                    object_location,
                    format!(
                        "This is of type `{}` which doesn't support member access.",
                        TypeKind::Composite(composite_idx),
                    ),
                )
                .into())
            }
        };

        Ok(result)
    }

    fn typecheck_optional(
        &mut self,
        kind: AstOptionalKind,
        token: &Token,
        mut sub: Option<&mut Ast>,
    ) -> Result<()> {
        let interp = Interpreter::get_mut();

        match kind {
            AstOptionalKind::Break | AstOptionalKind::Continue => todo!(),
            AstOptionalKind::Return => {
                let sub_type = if let Some(expr) = &mut sub {
                    self.typecheck_node(interp, expr)?;
                    expr.typ
                } else {
                    None
                };

                let expected_type = self.returns.last().ok_or(SourceError::new(
                    "Return statement outside of function body.",
                    token.loc,
                    "Return statements outside function bodies aren't allowed.",
                ))?;

                if sub_type != *expected_type {
                    if let Some(expr) = &mut sub {
                        let given_type = expr
                            .typ
                            .map_or_else(|| "Nothing".to_string(), |t| format!("{}", t));

                        let expected_type = expected_type
                            .map_or_else(|| "Nothing".to_string(), |t| format!("{}", t));

                        return Err(SourceError::new(
                            "Type mismatch",
                            expr.token.loc,
                            format!("Expected `{}` but found `{}`.", expected_type, given_type),
                        )
                        .into());
                    } else {
                        let given_type = sub
                            .map(|e| e.typ)
                            .flatten()
                            .map_or_else(|| "Nothing".to_string(), |t| format!("{}", t));

                        return Err(SourceError::new(
                            "Type mismatch",
                            token.loc,
                            format!("Expected `Nothing` but found `{}`", given_type),
                        )
                        .into());
                    }
                }
            }
        }

        Ok(())
    }

    fn typecheck_block(
        &mut self,
        scope: &mut Scope,
        token: &Token,
        kind: AstBlockKind,
        nodes: &mut [Ast],
    ) -> Result<()> {
        let interp = Interpreter::get_mut();

        for node in nodes {
            self.typecheck_node(interp, node)?;
        }

        Ok(())
    }

    fn typecheck_c_like_for_loop(
        &mut self,
        interp: &mut Interpreter,
        control: &mut AstInfoForControl,
        body: &mut Ast,
    ) -> Result<()> {
        let AstInfo::Var(info) = &mut control.initializer.info else {
            panic!("[INTERNAL ERR] `initializer` of `ForControl` node is not an `Assign` node.");
        };

        assert_eq!(info.targets.len(), 1);
        assert_eq!(info.initializers.len(), 1);

        let ident = &mut info.targets[0];
        let init = &mut info.initializers[0];

        self.typecheck_node(interp, init)?;
        let init_type = init.typ.ok_or_else(|| {
            SourceError::new(
                "Initializer expression of for loop has no type.",
                init.token.loc,
                "This doesn't have a type.",
            )
        })?;

        let scope = &mut interp.scopes[init.scope.0];
        self.typecheck_variable_for_binding(scope, true, ident, init_type)?;

        if let Some(cond) = &mut control.condition {
            self.typecheck_node(interp, cond)?;
            let Some(cond_type) = cond.typ else {
                return Err(SourceError::new("Type Mistmatch", cond.token.loc, "Expected type `Bool` but found no type.").into());
            };

            if cond_type.kind != TypeKind::Bool {
                return Err(SourceError::new(
                    "Type mismatch",
                    cond.token.loc,
                    format!("Expected type `Bool` but found type `{}`.", cond_type.kind),
                )
                .into());
            }
        }

        if let Some(step) = &mut control.step {
            self.typecheck_node(interp, step)?;
        }

        self.typecheck_node(interp, body)?;

        Ok(())
    }

    fn typecheck_for_loop_condition(&mut self, cond: &mut Ast, body: &mut Ast) -> Result<()> {
        let interp = Interpreter::get_mut();

        self.typecheck_node(interp, cond)?;

        let Some(cond_type) = cond.typ else {
            return Err(SourceError::new("Type mismatch", cond.token.loc, "Expected to be a type `Bool` but value has no value.").into());
        };

        if cond_type.kind != TypeKind::Bool {
            return Err(SourceError::new(
                "Type mismatch",
                cond.token.loc,
                format!(
                    "Expected to be type `Bool` but is actually type `{}`.",
                    cond_type
                ),
            )
            .into());
        }

        self.typecheck_node(interp, body)
    }
}
