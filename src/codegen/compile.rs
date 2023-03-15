use crate::{
    canon::scoping::{FuncID, Scope, ScopeBinding, ScopeIndex},
    interp::{Interpreter, ParsedFile},
    ir::{
        annotations::Annotations,
        ast::{
            Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoFor, AstInfoForControl,
            AstInfoIf, AstInfoVar, AstOptionalKind, AstUnaryKind, Queued, QueuedProgress,
        },
    },
    parsing::tokenization::{Token, TokenInfo},
    runtime::{
        builtins::{self, Builtin},
        vm::{Addr, Size},
    },
    typing::value_type::{
        runtime_type::{Bool, Char, Float, Int, Pointer},
        Type, TypeInfo, TypeInfoEnum, TypeInfoFunction, TypeInfoStruct, TypeKind,
    },
    util::errors::{Result, SourceError, SourceError2},
};

use super::{
    exe::{Executable, ExecutableBuilder},
    inst::Instruction,
};

pub fn compile_executable(files: &mut [ParsedFile]) -> Result<Executable> {
    let mut compiler = Compiler::new();

    loop {
        let mut all_compiled = true;

        for queued in files
            .iter_mut()
            .flat_map(|file| file.ast.iter_mut())
            .filter(|queued| is_node_compilable(&queued.node))
        {
            if !queued_ready_for_compile(queued) {
                all_compiled = false;
                continue;
            } else if queued.is_compiled() {
                continue;
            }

            compiler.compile_node(&queued.node)?;
            queued.progress = QueuedProgress::Compiled;
        }

        if all_compiled {
            break;
        }
    }

    let global_scope = compiler
        .func_builders
        .pop()
        .expect("[INTERNAL ERR] No global scope remaining after compilation.");
    assert!(
        compiler.func_builders.is_empty(),
        "FunctionBuilder's still left over after compilation."
    );

    global_scope.build();

    compiler.exe.build()
}

fn queued_ready_for_compile(queued: &Queued) -> bool {
    let interp = Interpreter::get();

    for dep in &queued.deps {
        let dep = &interp.parsed_files[dep.parsed_file_idx].ast[dep.queued_idx];

        if !is_node_compilable(&dep.node) {
            continue;
        }

        if dep.progress < QueuedProgress::Compiled {
            return false;
        }
    }

    true
}

fn is_node_compilable(node: &Ast) -> bool {
    match node.info {
        AstInfo::Import(_)
        | AstInfo::TypeSignature(_)
        | AstInfo::Struct(_)
        | AstInfo::Enum(_)
        | AstInfo::EnumVariant(_) => false,
        _ => true,
    }
}

#[derive(Default)]
struct FunctionBuilder {
    func_id: FuncID,
    stack_top: Addr,
    code: Vec<u8>,
}

impl FunctionBuilder {
    fn build(self) {
        let interp = Interpreter::get_mut();
        let info = &mut interp.funcs[self.func_id.0];
        info.code = Some(self.code.into_boxed_slice());
    }
}

struct Compiler {
    exe: ExecutableBuilder,
    func_builders: Vec<FunctionBuilder>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            exe: ExecutableBuilder::new(),
            func_builders: vec![Default::default()],
        }
    }

    fn current_function_mut(&mut self) -> &mut FunctionBuilder {
        self.func_builders
            .last_mut()
            .expect("[INTERNAL ERR] No function builders.")
    }

    fn current_function(&self) -> &FunctionBuilder {
        self.func_builders
            .last()
            .expect("[INTERNAL ERR] No function builders.")
    }

    fn begin_building_function(&mut self, id: FuncID) {
        let new = FunctionBuilder {
            func_id: id,
            ..Default::default()
        };

        self.func_builders.push(new);
    }

    fn finish_building_function(&mut self, id: FuncID) {
        let finished = self
            .func_builders
            .pop()
            .expect("[INTERNAL ERR] No builder's to end.");
        assert_eq!(
            finished.func_id, id,
            "[INTERNAL ERR] Attempted to end a function builder with the wrong ID. {:?} vs. {:?}",
            finished.func_id, id
        );

        finished.build();
    }

    fn stack_top(&self) -> Addr {
        self.current_function().stack_top
    }

    fn set_stack_top(&mut self, new_top: Addr) {
        self.current_function_mut().stack_top = new_top;
    }
}

impl Compiler {
    fn emit_byte(&mut self, byte: u8) {
        let func = self.current_function_mut();
        func.code.push(byte);
    }

    fn emit_inst(&mut self, inst: Instruction) {
        assert_eq!(
            std::mem::size_of::<Instruction>(),
            std::mem::size_of::<u8>()
        );
        let byte = inst as u8;
        self.emit_byte(byte);
    }

    fn emit_value<T: Copy>(&mut self, value: T) {
        let size = std::mem::size_of_val(&value);

        unsafe {
            let mut p = &value as *const T as *const u8;
            for _ in 0..size {
                self.emit_byte(*p);
                p = p.add(1);
            }
        }
    }

    fn emit_bool(&mut self, value: Bool) {
        if value {
            self.emit_inst(Instruction::Lit_True);
        } else {
            self.emit_inst(Instruction::Lit_False);
        }
    }

    fn emit_char(&mut self, value: Char) {
        self.emit_inst(Instruction::Lit_Char);
        self.emit_value(value);
    }

    fn emit_int(&mut self, value: Int) {
        match value {
            0 => self.emit_inst(Instruction::Lit_0),
            1 => self.emit_inst(Instruction::Lit_1),
            _ => {
                self.emit_inst(Instruction::Lit_Int);
                self.emit_value(value);
            }
        }
    }

    fn emit_float(&mut self, value: Float) {
        self.emit_inst(Instruction::Lit_Float);
        self.emit_value(value);
    }

    fn emit_ptr(&mut self, value: Pointer) {
        self.emit_inst(Instruction::Lit_Pointer);
        self.emit_value(value);
    }

    fn emit_push_const(&mut self, idx: usize, size: Size) {
        self.emit_inst(Instruction::PushConst);
        self.emit_value(size);
        self.emit_value(idx);
    }

    fn emit_push_const_value<T: Copy>(&mut self, value: T) {
        let size = std::mem::size_of::<T>();
        let bytes = unsafe { std::slice::from_raw_parts(&value as *const T as *const u8, size) };
        let idx = self.exe.add_constant(bytes);
        self.emit_push_const(
            idx,
            size.try_into()
                .expect("[INTERNAL ERR] size doesn't fit in a `Size`."),
        );
    }

    fn emit_push_const_str(&mut self, idx: usize) {
        self.emit_inst(Instruction::PushConst_Str);
        self.emit_value(idx);
    }

    fn emit_push_ptr(&mut self, is_global: bool, addr: Addr) {
        if is_global {
            self.emit_inst(Instruction::PushPtrGlobal)
        } else {
            self.emit_inst(Instruction::PushPtr)
        }

        self.emit_value(addr);
    }

    fn emit_move(&mut self, size: Size) {
        self.emit_inst(Instruction::Move);
        self.emit_value(size);
    }

    fn emit_move_imm(&mut self, global: bool, size: Size, addr: Addr) {
        if global {
            self.emit_inst(Instruction::MoveImmGlobal);
        } else {
            self.emit_inst(Instruction::MoveImm);
        }

        self.emit_value(size);
        self.emit_value(addr);
    }

    fn emit_dup(&mut self, global: bool, size: Size, addr: Addr) {
        if global {
            self.emit_inst(Instruction::DupGlobal);
        } else {
            self.emit_inst(Instruction::Dup);
        }

        self.emit_value(size);
        self.emit_value(addr);
    }

    fn emit_load(&mut self, size: Size) {
        self.emit_inst(Instruction::Load);
        self.emit_value(size);
    }

    fn emit_pop(&mut self, size: Size) {
        self.emit_inst(Instruction::Pop);
        self.emit_value(size);
        self.set_stack_top(self.stack_top() - size);
    }

    fn emit_flush(&mut self, addr: Addr) {
        let stack_top = self.stack_top();
        if stack_top != addr {
            self.emit_inst(Instruction::Flush);
            self.emit_value(addr);
            self.set_stack_top(addr);
        }
    }

    fn emit_alloc(&mut self, size: Size) {
        self.emit_inst(Instruction::Alloc);
        self.emit_value(size);
    }

    fn emit_allocz(&mut self, size: Size) {
        self.emit_inst(Instruction::AllocZ);
        self.emit_value(size);
    }

    fn emit_call(&mut self, size: Size) {
        if size == 0 {
            self.emit_inst(Instruction::Call_0);
        } else {
            self.emit_inst(Instruction::Call);
            self.emit_value(size);
        }
    }

    fn emit_call_builtin(&mut self, size: Size, builtin: Builtin) {
        self.emit_inst(Instruction::CallBuiltin);
        self.emit_value(size);
        self.emit_value(builtin);
    }

    fn emit_jump(&mut self, jump_inst: Instruction) -> usize {
        self.emit_inst(jump_inst);

        let jump = self.current_function().code.len();
        self.emit_value(0 as Addr);

        if jump_inst == Instruction::JumpTrue || jump_inst == Instruction::JumpFalse {
            let stack_top = self.stack_top();
            self.set_stack_top(stack_top - TypeKind::Bool.size());
        }

        jump
    }

    fn patch_jump(&mut self, jump: usize) {
        let to = self.current_function().code.len();
        let jump_size: &mut Addr =
            unsafe { std::mem::transmute(&mut self.current_function_mut().code[jump]) };
        *jump_size = (to - jump - std::mem::size_of::<Addr>())
            .try_into()
            .expect("[INTERNAL ERR] jump too big to fit in an `Addr`.");
    }

    fn emit_jump_back(&mut self, loop_start: usize) {
        self.emit_inst(Instruction::JumpBack);
        let jump = self.current_function().code.len() - loop_start + std::mem::size_of::<Addr>();
        self.emit_value(jump as Addr);
    }

    fn emit_return(&mut self, size: Size) {
        if size == 0 {
            self.emit_inst(Instruction::Ret_0);
        } else {
            self.emit_inst(Instruction::Ret);
            self.emit_value(size);
        }
    }
}

struct FindStaticAddressResult {
    addr: Addr,
    is_global: bool,
}

impl Compiler {
    fn find_static_address(&self, node: &Ast) -> Option<FindStaticAddressResult> {
        let interp = Interpreter::get();

        match &node.info {
            AstInfo::Literal => match &node.token.info {
                TokenInfo::Ident(ident) => {
                    let scope = &interp.scopes[node.scope.0];
                    let ScopeBinding::Var(var) = scope.find_binding(ident).expect("[INTERNAL ERR] Unresolved identifier reached compilation.") else {
                        panic!("[INTERNAL ERR] Attempt to find static address of a `Literal` node that isn't an `Ident` node.");
                    };

                    Some(FindStaticAddressResult {
                        addr: var.addr,
                        is_global: var.is_global,
                    })
                }
                _ => None,
            },
            AstInfo::Binary(AstBinaryKind::Subscript, lhs, rhs) => {
                let array_addr = self.find_static_address(lhs)?;
                todo!()
                //         if (array_status == Find_Static_Address_Result::Not_Found ||
                //             sub->lhs->type.kind != Value_Type_Kind::Array ||
                //             !sub->rhs->is_constant(c))
                //         {
                //             status = Find_Static_Address_Result::Not_Found;
                //             break;
                //         }
                //
                //         if (array_status == Find_Static_Address_Result::Found_Global) {
                //             status = Find_Static_Address_Result::Found_Global;
                //         }
                //
                //         runtime::Int index;
                //         c.evaluate_unchecked(sub->rhs, index);
                //
                //         address = array_address + index * sub->type.size();
            }
            AstInfo::Binary(AstBinaryKind::MemberAccess, lhs, rhs) => {
                let lhs_type = lhs
                    .typ
                    .expect("[INTERNAL ERR] lhs node of `MemberAccess` node does not have a type.");
                if lhs_type.is_pointer() {
                    return None;
                }

                let TypeKind::Composite(lhs_type_idx) = lhs_type.kind else {
                    panic!("[INTERNAL ERR] lhs's type of `MemberAccess` is not a composite type.");
                };
                let lhs_type = &interp.types[lhs_type_idx];

                let TokenInfo::Ident(field_ident) = &rhs.token.info else {
                    panic!("[INTERNAL ERR] rhs node of `MemberAccess` node is not an `Ident` node.");
                };

                let field_offset = if let TypeInfo::Struct(info) = lhs_type {
                    info.offset_of(field_ident)
                } else {
                    panic!(
                        "[INTERNAL ERR] lhs's type of `MemberAccess` node is not a struct type."
                    );
                };

                let obj_addr = self.find_static_address(lhs)?;
                Some(FindStaticAddressResult {
                    addr: obj_addr.addr + field_offset,
                    ..obj_addr
                })
            }
            _ => None,
        }
        //     case Typed_AST_Kind::Address_Of: {
        //         todo("Implement finding the static address of constant pointer values.");
        //     } break;
    }

    fn compile_addr_code(&mut self, node: &Ast) -> Result<()> {
        if let Some(FindStaticAddressResult { addr, is_global }) = self.find_static_address(node) {
            self.emit_push_ptr(is_global, addr);
        } else {
            self.compile_dynamic_addr_code(node)?;
        }

        Ok(())
    }

    fn compile_dynamic_addr_code(&mut self, node: &Ast) -> Result<bool> {
        let interp = Interpreter::get();

        match &node.info {
            AstInfo::Literal => match &node.token.info {
                TokenInfo::Ident(ident) => {
                    let scope = &interp.scopes[node.scope.0];
                    let Some(binding) = scope.find_binding(ident) else {
                        panic!("[INTERNAL ERR] Unresolved identifier `{}` reached compilation.", ident);
                    };

                    match binding {
                        ScopeBinding::Var(var) => {
                            self.emit_push_ptr(var.is_global, var.addr);
                        }
                        ScopeBinding::Fn(func) => todo!(),
                        ScopeBinding::Type(typ) => todo!(),
                    }

                    Ok(true)
                }
                _ => Ok(false),
            },
            AstInfo::Unary(AstUnaryKind::Deref, sub_node) => {
                self.compile_node(sub_node)?;
                Ok(true)
            }
            AstInfo::Binary(AstBinaryKind::Subscript, lhs, rhs) => {
                todo!()
                //     case Typed_AST_Kind::Subscript: {
                //         auto sub = dynamic_cast<Typed_AST_Binary *>(&node);
                //         internal_verify(sub, "Failed to cast node to Binary* in emit_dynamic_address_code().");
                //
                //         Size element_size = sub->lhs->type.child_type()->size();
                //
                //         if (!emit_address_code(c, *sub->lhs)) {
                //             return false;
                //         }
                //
                //         if (sub->lhs->type.kind == Value_Type_Kind::Slice) {
                //             c.emit_opcode(Opcode::Load);
                //             c.emit_size(value_types::Ptr.size());
                //         }
                //
                //         // offset = rhs * element_size
                //         sub->rhs->compile(c);
                //         c.emit_opcode(Opcode::Lit_Int);
                //         c.emit_value<runtime::Int>(element_size);
                //         c.emit_opcode(Opcode::Int_Mul);
                //
                //         // address = &lhs + offset
                //         c.emit_opcode(Opcode::Int_Add);
                //     } break;
            }
            AstInfo::Binary(AstBinaryKind::MemberAccess, lhs, rhs) => {
                let lhs_type = lhs
                    .typ
                    .expect("[INTERNAL ERR] `lhs` of `MemberAccess` node does not have a type.");
                if lhs_type.is_pointer() {
                    self.compile_node(lhs)?;
                } else {
                    self.compile_addr_code(lhs)?;
                }

                let TypeKind::Composite(lhs_type_idx) = lhs_type.kind else {
                    panic!("[INTERNAL ERR] lhs's type of `MemberAccess` is not a composite type.");
                };
                let lhs_type = &interp.types[lhs_type_idx];

                let TokenInfo::Ident(field_ident) = &rhs.token.info else {
                    panic!("[INTERNAL ERR] rhs node of `MemberAccess` node is not an `Ident` node.");
                };

                let field_offset = if let TypeInfo::Struct(info) = lhs_type {
                    info.offset_of(field_ident)
                } else {
                    panic!(
                        "[INTERNAL ERR] lhs's type of `MemberAccess` node is not a struct type."
                    );
                };

                self.emit_int(field_offset as Int);
                self.emit_inst(Instruction::Int_Add);
                Ok(true)
            }
            _ => Ok(false),
        }
        // switch (node.kind) {
        //     case Typed_AST_Kind::Negative_Subscript: {
        //         auto sub = dynamic_cast<Typed_AST_Binary *>(&node);
        //         internal_verify(sub, "Failed to cast node to Binary* in emit_dynamic_address_code().");
        //         internal_verify(sub->lhs->type.kind == Value_Type_Kind::Slice, "sub->lhs is not a slice in Negative_Subscript part of emit_dynamic_address_code().");
        //
        //         Size element_size = sub->type.size();
        //         runtime::Int index = -sub->rhs.cast<Typed_AST_Int>()->value;
        //
        //         // (data, count) = result of compiling lhs
        //         sub->lhs->compile(c);
        //
        //         // offset = (count - index) * element_size
        //         c.emit_opcode(Opcode::Lit_Int);
        //         c.emit_value<runtime::Int>(index);
        //
        //         c.emit_opcode(Opcode::Int_Sub);
        //
        //         c.emit_opcode(Opcode::Lit_Int);
        //         c.emit_value<runtime::Int>(element_size);
        //
        //         c.emit_opcode(Opcode::Int_Mul);
        //
        //         // element_ptr = data + offset
        //         c.emit_opcode(Opcode::Int_Add);
        //     } break;
        //
        //     default:
        //         return false;
        // }
        //
        // return true;
    }
}

impl Compiler {
    fn compile_node(&mut self, node: &Ast) -> Result<()> {
        match &node.info {
            AstInfo::Literal => self.compile_literal(node.scope, &node.token, node.typ),
            AstInfo::Unary(kind, expr) => self.compile_unary(*kind, node.typ, expr),
            AstInfo::Binary(kind, lhs, rhs) => self.compile_binary(*kind, node, lhs, rhs),
            AstInfo::Optional(kind, expr) => {
                self.compile_optional(*kind, expr.as_ref().map(|e| e.as_ref()))
            }
            AstInfo::Block(kind, nodes) => self.compile_block(*kind, nodes),
            AstInfo::Fn(info) => self.compile_fn_decl(&node.token, info),
            AstInfo::Var(info) => self.compile_var_decl(node.scope, &node.token, info),
            AstInfo::EnumVariantLiteral(value) => {
                self.emit_int(*value);
                Ok(())
            }
            AstInfo::TypeValue(typ) => todo!(),
            AstInfo::If(info) => self.compile_if_statement(info),
            AstInfo::For(info) => self.compile_for_loop(info),
            AstInfo::ForControl(_) => {
                panic!("`ForControl` not being handled in `compile_for_loop`.")
            }

            AstInfo::Import(_)
            | AstInfo::TypeSignature(_)
            | AstInfo::Struct(_)
            | AstInfo::Enum(_)
            | AstInfo::EnumVariant(_) => unreachable!(),
        }
    }

    fn compile_literal(
        &mut self,
        scope: ScopeIndex,
        token: &Token,
        typ: Option<Type>,
    ) -> Result<()> {
        let stack_top = self.stack_top();

        match &token.info {
            TokenInfo::Ident(ident) => self.compile_ident(scope, ident)?,
            TokenInfo::Bool(value) => self.emit_bool(*value),
            TokenInfo::Char(value) => self.emit_char(*value),
            TokenInfo::Int(value) => self.emit_int(*value),
            TokenInfo::Float(value) => self.emit_float(*value),
            TokenInfo::String(value) => {
                let constant_idx = self.exe.add_str_constant(value.as_bytes());
                self.emit_push_const_str(constant_idx);
            }
            _ => panic!("Invalid literal token info `{:?}`", token.info),
        }

        let size = typ.map_or(0, |t| t.size());
        self.set_stack_top(stack_top + size);

        Ok(())
    }

    fn compile_ident(&mut self, scope_idx: ScopeIndex, ident: &str) -> Result<()> {
        let stack_top = self.stack_top();

        let interp = Interpreter::get();
        let scope = &interp.scopes[scope_idx.0];

        let Some(binding) = scope.find_binding(ident) else {
            panic!("[INTERNAL ERR] Unresolvable identifier `{}`.", ident);
        };

        let size: Size;
        match binding {
            ScopeBinding::Var(var) => {
                let global = var.is_global;
                size = var.typ.size();
                let addr = var.addr;
                self.emit_dup(global, size, addr);
            }
            ScopeBinding::Fn(func) => {
                let fn_ref = &interp.funcs[func.id.0];
                let fn_ptr = fn_ref as *const _ as Pointer;
                self.emit_ptr(fn_ptr);

                size = std::mem::size_of::<Pointer>()
                    .try_into()
                    .expect("[INTERNAL ERR] Size of `Pointer` doesn't fit in a `Size`.");
            }
            ScopeBinding::Type(typ) => {
                todo!()
            }
        }

        self.set_stack_top(stack_top + size);

        Ok(())
    }

    fn compile_fn_decl(&mut self, token: &Token, info: &AstInfoFn) -> Result<()> {
        let interp = Interpreter::get_mut();

        let id = info
            .id
            .expect("[INTERNAL ERR] Attempt to compile `AstInfoFn` without an ID.");
        let func = &mut interp.funcs[id.0];

        let is_entry_point = info.annons.contains(Annotations::FN_ENTRY);
        if is_entry_point {
            let ok = self.exe.set_entry_point(func.id.0, token.loc);
            if !ok {
                return Err(SourceError2::new(
                    "Multiple entry points declared.",
                    token.loc,
                    "This was declared as the entry point when another function already has been.",
                    self.exe.entry_point().unwrap().loc,
                    "This is the previously declared entry point.",
                )
                .into());
            }
        }

        self.begin_building_function(id);

        let AstInfo::Block(AstBlockKind::Params, params) = &info.params.info else {
            panic!("[INTERNAL ERR] `params` node is not a `Params` node.");
        };

        for param in params {
            let scope = &mut interp.scopes[param.scope.0];

            let AstInfo::Binary(AstBinaryKind::Param, ident, _) = &param.info else {
                panic!("[INTERNAL ERR] `param` node is not a `Param` node.");
            };

            let TokenInfo::Ident(ident) = &ident.token.info else {
                panic!("[INTERNAL ERR] `ident` of parameter was not an `Ident` node.");
            };

            let Some(binding) = scope.find_binding_mut(ident) else {
                panic!("[INTERNAL ERR] Unresolvable identifier `{}`.", ident);
            };

            let ScopeBinding::Var(var) = binding else {
                panic!("[INTERNAL ERR] ident `{}` doesn't bind to a variable but a `{:?}`.", ident, binding);
            };

            let stack_top = self.stack_top();
            var.addr = stack_top;

            self.set_stack_top(stack_top + var.typ.size());
        }

        self.compile_node(&info.body)?;

        // @TODO:
        // Do not emit superfluous return instructions.
        self.emit_inst(Instruction::Ret_0);

        self.finish_building_function(id);

        Ok(())
    }

    fn compile_var_decl(
        &mut self,
        scope: ScopeIndex,
        token: &Token,
        info: &AstInfoVar,
    ) -> Result<()> {
        match info.initializers.as_slice() {
            [] => self.compile_var_decl_no_initializer(scope, &info.targets),
            [init] => self.compile_var_decl_one_initalizer(scope, &info.targets, init),
            inits => self.compile_var_decl_many_initializers(scope, &info.targets, inits),
        }
    }

    fn compile_var_decl_no_initializer(
        &mut self,
        scope: ScopeIndex,
        targets: &[Ast],
    ) -> Result<()> {
        let interp = Interpreter::get_mut();
        let scope = &mut interp.scopes[scope.0];

        let stack_top = self.stack_top();
        let mut total_size: Size = 0;

        for target in targets {
            let AstInfo::Binary(AstBinaryKind::ConstrainedVarDeclTarget, ident, typ) = &target.info else {
                panic!("[INTERNAL ERR] target node is not a `ConstrainedVarDeclTarget` node.");
            };

            let TokenInfo::Ident(ident) = &ident.token.info else {
                panic!("[INTERNAL ERR] `ident` node of `ConstrainedVarDeclTarget` node not an `Ident` node.");
            };

            let AstInfo::TypeValue(typ) = typ.info else {
                panic!("[INTERNAL ERR] `typ` node of `ConstrainedVarDeclTarget` node not a type.");
            };

            self.bind_variable_address(stack_top + total_size, scope, ident);
            total_size += typ.size();
        }

        self.emit_allocz(total_size);
        self.set_stack_top(stack_top + total_size);

        Ok(())
    }

    fn compile_var_decl_one_initalizer(
        &mut self,
        scope: ScopeIndex,
        targets: &[Ast],
        init: &Ast,
    ) -> Result<()> {
        let interp = Interpreter::get_mut();
        let scope = &mut interp.scopes[scope.0];
        let is_global_scope = scope.is_global();

        let stack_top = self.stack_top();
        let typ = init
            .typ
            .expect("[INTERNAL ERR] variable initalizer doesn't have a type.");

        self.compile_node(init)?;
        for _ in 1..targets.len() {
            self.emit_dup(is_global_scope, typ.size(), stack_top);
        }

        for (i, target) in targets.iter().enumerate() {
            let i: Size = i
                .try_into()
                .expect("[INTERNAL ERR] Too many targets to fit in a `Size`.");

            let ident = match &target.info {
                AstInfo::Literal => match &target.token.info {
                    TokenInfo::Ident(ident) => ident,
                    _ => panic!("[INTERNAL ERR] target node in `Var` node is a `Literal` but not an `Ident`."),
                },
                AstInfo::Binary(AstBinaryKind::ConstrainedVarDeclTarget, ident, _) => match &ident.token.info {
                    TokenInfo::Ident(ident) => ident,
                    _ => panic!("[INTERNAL ERR] target node in `Var` node is a `ConstrainedVarDeclTarget` node where the lhs is not an `Ident` node."),
                },
                _ => panic!("[INTERNAL ERR] target node in `Var` node is not an `Ident` node or a `ConstrainedVarDeclTarget` node."),
            };

            self.bind_variable_address(stack_top + i * typ.size(), scope, ident);
        }

        self.set_stack_top(stack_top + targets.len() as Size * typ.size());

        Ok(())
    }

    fn compile_var_decl_many_initializers(
        &mut self,
        scope: ScopeIndex,
        targets: &[Ast],
        inits: &[Ast],
    ) -> Result<()> {
        let interp = Interpreter::get_mut();
        let scope = &mut interp.scopes[scope.0];

        assert_eq!(
            targets.len(),
            inits.len(),
            "[INTERNAL ERR] Incorrect number of targets for number of initalizer expressions."
        );
        for (target, init) in targets.iter().zip(inits) {
            let stack_top = self.stack_top();
            let typ = init
                .typ
                .expect("[INTERNAL ERR] init expression doesn't have a type.");

            let ident = match &target.info {
                AstInfo::Literal => match &target.token.info {
                    TokenInfo::Ident(ident) => ident,
                    _ => panic!("[INTERNAL ERR] target node in `Var` node is a `Literal` but not an `Ident`."),
                },
                AstInfo::Binary(AstBinaryKind::ConstrainedVarDeclTarget, ident, _) => match &ident.token.info {
                    TokenInfo::Ident(ident) => ident,
                    _ => panic!("[INTERNAL ERR] target node in `Var` node is a `ConstrainedVarDeclTarget` node where the lhs is not an `Ident` node."),
                },
                _ => panic!("[INTERNAL ERR] target node in `Var` node is not an `Ident` node or a `ConstrainedVarDeclTarget` node."),
            };

            self.bind_variable_address(stack_top, scope, ident);

            self.compile_node(init)?;

            self.set_stack_top(stack_top + typ.size());
        }

        Ok(())
    }

    fn bind_variable_address(&mut self, addr: Addr, scope: &mut Scope, ident: &str) {
        let Some(binding) = scope.find_binding_mut(ident) else {
            panic!("[INTERNAL ERR] Unresolvable identifier `{}`.", ident);
        };

        let ScopeBinding::Var(var) = binding else {
            panic!("[INTERNAL ERR] ident `{}` doesn't bind to a variable but a `{:?}`.", ident, binding);
        };

        var.addr = addr;
    }

    fn compile_if_statement(&mut self, info: &AstInfoIf) -> Result<()> {
        self.compile_node(&info.condition)?;
        let else_jump = self.emit_jump(Instruction::JumpFalse);
        let mut exit_jump = 0;

        self.compile_node(&info.then_block)?;

        if info.else_block.is_some() {
            exit_jump = self.emit_jump(Instruction::Jump);
        }

        self.patch_jump(else_jump);

        if let Some(else_block) = &info.else_block {
            self.compile_node(else_block)?;
            self.patch_jump(exit_jump);
        }

        Ok(())
    }

    fn compile_for_loop(&mut self, info: &AstInfoFor) -> Result<()> {
        match &info.control {
            Some(Ast {
                token: _,
                scope: _,
                typ: _,
                info: AstInfo::ForControl(control_info),
            }) => self.compile_c_like_for_loop(control_info, &info.body),
            Some(Ast {
                token: _,
                scope: _,
                typ: _,
                info: AstInfo::Binary(AstBinaryKind::In, it, seq),
            }) => todo!(),
            Some(control) => self.compile_for_loop_condition(control, &info.body),
            _ => self.compile_forever_loop(&info.body),
        }
    }

    fn compile_c_like_for_loop(&mut self, control: &AstInfoForControl, body: &Ast) -> Result<()> {
        let stack_top = self.stack_top();

        self.compile_node(&control.initializer)?;

        let loop_start = self.current_function().code.len();

        let exit_jump = if let Some(cond) = &control.condition {
            self.compile_node(cond)?;
            let exit_jump = self.emit_jump(Instruction::JumpFalse);
            Some(exit_jump)
        } else {
            None
        };

        self.compile_node(body)?;

        if let Some(step) = &control.step {
            self.compile_node(step)?;
        }

        self.emit_jump_back(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
        }

        self.set_stack_top(stack_top);
        Ok(())
    }

    fn compile_for_loop_condition(&mut self, cond: &Ast, body: &Ast) -> Result<()> {
        let loop_start = self.current_function().code.len();
        let stack_top = self.stack_top();

        // auto label = this->label ? this->label->id : String{};
        // c.begin_loop(label, location);

        self.compile_node(cond)?;
        let exit_jump = self.emit_jump(Instruction::JumpFalse);

        self.compile_node(body)?;

        // auto loop = c.loops.back();

        // c.patch_loop_controls(loop.continues);
        // c.emit_loop(loop_start);

        self.emit_jump_back(loop_start);

        self.patch_jump(exit_jump);

        // c.patch_loop_controls(loop.breaks);
        // c.end_loop();

        self.set_stack_top(stack_top);
        Ok(())
    }

    fn compile_forever_loop(&mut self, body: &Ast) -> Result<()> {
        let loop_start = self.current_function().code.len();
        let stack_top = self.stack_top();

        self.compile_node(body)?;
        self.emit_jump_back(loop_start);

        self.set_stack_top(stack_top);
        Ok(())
    }

    fn compile_unary(&mut self, kind: AstUnaryKind, typ: Option<Type>, expr: &Ast) -> Result<()> {
        match kind {
            AstUnaryKind::Neg | AstUnaryKind::Not | AstUnaryKind::XXXPrint => {
                self.compile_unary_typical(kind, typ, expr)
            }
            AstUnaryKind::Ref | AstUnaryKind::RefMut => {
                let stack_top = self.stack_top();
                let size = typ.map_or(0, |t| t.size());

                self.compile_addr_code(expr)?;

                self.set_stack_top(stack_top + size);
                Ok(())
            }
            AstUnaryKind::Deref => {
                let stack_top = self.stack_top();
                let size = typ.map_or(0, |t| t.size());

                // @TODO: Check for static stuff
                self.compile_node(expr)?;
                self.emit_load(size);

                self.set_stack_top(stack_top + size);
                Ok(())
            }
        }
    }

    fn compile_unary_typical(
        &mut self,
        kind: AstUnaryKind,
        typ: Option<Type>,
        expr: &Ast,
    ) -> Result<()> {
        let stack_top = self.stack_top();
        let expr_type = expr.typ.unwrap();

        self.compile_node(expr)?;

        match (expr_type.kind, kind) {
            (TypeKind::Int, AstUnaryKind::Neg) => self.emit_inst(Instruction::Int_Neg),
            (TypeKind::Float, AstUnaryKind::Neg) => self.emit_inst(Instruction::Float_Neg),

            (TypeKind::Bool, AstUnaryKind::Not) => self.emit_inst(Instruction::Not),
            (TypeKind::Int, AstUnaryKind::Not) => self.emit_inst(Instruction::Bit_Not),

            (TypeKind::Bool, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(expr_type.size(), builtins::XXXprint_Bool);
            }
            (TypeKind::Char, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(expr_type.size(), builtins::XXXprint_Char);
            }
            (TypeKind::Int, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(expr_type.size(), builtins::XXXprint_Int);
            }
            (TypeKind::Float, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(expr_type.size(), builtins::XXXprint_Float);
            }
            (TypeKind::String, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(expr_type.size(), builtins::XXXprint_String);
            }
            (TypeKind::Range, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(expr_type.size(), builtins::XXXprint_Range);
            }
            (_, AstUnaryKind::XXXPrint) if expr_type.is_pointer() => {
                self.emit_call_builtin(expr_type.size(), builtins::XXXprint_Pointer);
            }
            (TypeKind::Composite(type_idx), AstUnaryKind::XXXPrint) => {
                let interp = Interpreter::get();
                let type_info = &interp.types[type_idx];
                match type_info {
                    TypeInfo::Pointer(_) => unreachable!(),
                    TypeInfo::Array(info) => {
                        self.emit_int(info.size as Int);
                        self.emit_push_const_value(info.element_type);

                        let arg_size = std::mem::size_of_val(&info.size)
                            + std::mem::size_of_val(&info.element_type)
                            + info.element_type.size() as usize * info.size;
                        let arg_size: Size = arg_size
                            .try_into()
                            .expect("[INTERNAL ERR] `arg_size` doesn't fit into a `Size`.");
                        self.emit_call_builtin(arg_size, builtins::XXXprint_Array);
                    }
                    TypeInfo::Struct(info) => {
                        self.emit_ptr(info as *const TypeInfoStruct as Pointer);
                        self.emit_call_builtin(
                            expr_type.size() + TypeKind::Int.size(),
                            builtins::XXXprint_struct,
                        );
                    }
                    TypeInfo::Enum(info) => {
                        self.emit_ptr(info as *const TypeInfoEnum as Pointer);
                        self.emit_call_builtin(
                            expr_type.size() + TypeKind::Int.size(),
                            builtins::XXXprint_enum,
                        );
                    }
                    TypeInfo::Function(info) => {
                        self.emit_ptr(info as *const TypeInfoFunction as Pointer);
                        self.emit_call_builtin(
                            expr_type.size() + TypeKind::Int.size(),
                            builtins::XXXprint_function,
                        );
                    }
                }
            }

            _ => panic!(
                "[INTERNAL ERR] Invalid combination of type and unary kind (`{:?}`, `{:?}`).",
                expr_type, kind
            ),
        }

        let size = typ.map_or(0, |t| t.size());
        self.set_stack_top(stack_top + size);

        Ok(())
    }

    fn compile_binary(
        &mut self,
        kind: AstBinaryKind,
        node: &Ast,
        lhs: &Ast,
        rhs: &Ast,
    ) -> Result<()> {
        match kind {
            AstBinaryKind::Add
            | AstBinaryKind::Sub
            | AstBinaryKind::Mul
            | AstBinaryKind::Div
            | AstBinaryKind::Mod
            | AstBinaryKind::Lt
            | AstBinaryKind::Le
            | AstBinaryKind::Gt
            | AstBinaryKind::Ge => self.compile_binary_typical(kind, node.typ, lhs, rhs),
            AstBinaryKind::Assign => match self.find_static_address(lhs) {
                Some(FindStaticAddressResult { addr, is_global }) => {
                    let size = lhs
                        .typ
                        .expect("[INTERNAL ERR] `lhs` node of assignment doesn't have a type.")
                        .size();

                    let stack_top = self.stack_top();

                    self.compile_node(rhs)?;
                    self.emit_move_imm(is_global, size, addr);

                    self.set_stack_top(stack_top);
                    Ok(())
                }
                _ => {
                    let size = lhs
                        .typ
                        .expect("[INTERNAL ERR] `lhs` node of assignment doesn't have a type.")
                        .size();

                    let stack_top = self.stack_top();

                    self.compile_node(rhs)?;
                    self.compile_dynamic_addr_code(lhs)?;
                    self.emit_move(size);

                    self.set_stack_top(stack_top);
                    Ok(())
                }
            },
            AstBinaryKind::Call => {
                if lhs
                    .typ
                    .expect("[INTERNAL ERR] lhs of `Call` node doesn't have a type.")
                    .kind
                    == TypeKind::Type
                {
                    self.compile_node(rhs)
                } else {
                    self.compile_call(node.typ, lhs, rhs)
                }
            }
            AstBinaryKind::Subscript => todo!(),
            AstBinaryKind::MemberAccess => {
                let find_result = self.find_static_address(node);
                match find_result {
                    Some(FindStaticAddressResult { addr, is_global }) => {
                        self.emit_dup(
                            is_global,
                            node.typ
                                .expect("[INTERNAL ERR] `MemberAccess` node has no type.")
                                .size(),
                            addr,
                        );
                    }
                    _ => {
                        let size = lhs
                            .typ
                            .expect("[INTERNAL ERR] `lhs` of `MemberAccess` node has no type.")
                            .size();
                        self.compile_dynamic_addr_code(lhs)?;
                        self.emit_load(size);
                    }
                }

                Ok(())
            }
            AstBinaryKind::Range => {
                self.compile_node(lhs)?;
                self.compile_node(rhs)?;
                Ok(())
            }
            AstBinaryKind::Param => {
                panic!("[INTERNAL ERR] `Param` node not being handled by `compile_fn_decl()`.")
            }
            AstBinaryKind::ConstrainedVarDeclTarget => {
                panic!("[INTERNAL ERR] `ConstrainedVarDeclTarget` node not being handled in `compile_var_decl()`.")
            }
            AstBinaryKind::Field => {
                panic!("[INTERNAL ERR] `Field` node not being handled in `compile_struct_body()`.")
            }
            AstBinaryKind::In => todo!(),
        }
    }

    fn compile_binary_typical(
        &mut self,
        kind: AstBinaryKind,
        typ: Option<Type>,
        lhs: &Ast,
        rhs: &Ast,
    ) -> Result<()> {
        let stack_top = self.stack_top();
        let lhs_type = lhs.typ.unwrap();
        let rhs_type = rhs.typ.unwrap();

        self.compile_node(lhs)?;
        self.compile_node(rhs)?;

        match (lhs_type.kind, rhs_type.kind, kind) {
            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Add) => self.emit_inst(Instruction::Int_Add),
            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Sub) => self.emit_inst(Instruction::Int_Sub),
            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Mul) => self.emit_inst(Instruction::Int_Mul),
            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Div) => self.emit_inst(Instruction::Int_Div),
            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Mod) => self.emit_inst(Instruction::Int_Mod),

            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Lt) => self.emit_inst(Instruction::Int_Lt),
            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Le) => self.emit_inst(Instruction::Int_Le),
            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Gt) => self.emit_inst(Instruction::Int_Gt),
            (TypeKind::Int, TypeKind::Int, AstBinaryKind::Ge) => self.emit_inst(Instruction::Int_Ge),

            (TypeKind::Float, TypeKind::Float, AstBinaryKind::Add) => self.emit_inst(Instruction::Float_Add),
            (TypeKind::Float, TypeKind::Float, AstBinaryKind::Sub) => self.emit_inst(Instruction::Float_Sub),
            (TypeKind::Float, TypeKind::Float, AstBinaryKind::Mul) => self.emit_inst(Instruction::Float_Mul),
            (TypeKind::Float, TypeKind::Float, AstBinaryKind::Div) => self.emit_inst(Instruction::Float_Div),

            (TypeKind::Float, TypeKind::Float, AstBinaryKind::Lt) => self.emit_inst(Instruction::Float_Lt),
            (TypeKind::Float, TypeKind::Float, AstBinaryKind::Le) => self.emit_inst(Instruction::Float_Le),
            (TypeKind::Float, TypeKind::Float, AstBinaryKind::Gt) => self.emit_inst(Instruction::Float_Gt),
            (TypeKind::Float, TypeKind::Float, AstBinaryKind::Ge) => self.emit_inst(Instruction::Float_Ge),

            _ => panic!("[INTERNAL ERR] Unhandled combination of type and binary kind (`{:?}`, `{:?}`, `{:?}`).", lhs_type, kind, rhs_type),
        }

        let size = typ.map_or(0, |t| t.size());
        self.set_stack_top(stack_top + size);

        Ok(())
    }

    fn compile_call(&mut self, return_type: Option<Type>, callee: &Ast, args: &Ast) -> Result<()> {
        let stack_top_before_args = self.stack_top();

        self.compile_node(args)?;

        let stack_top_after_args = self.stack_top();
        let arg_size = stack_top_after_args - stack_top_before_args;

        self.compile_node(callee)?;
        self.emit_call(arg_size);

        let size = return_type.map_or(0, |t| t.size());
        self.set_stack_top(stack_top_before_args + size);

        Ok(())
    }

    fn compile_optional(&mut self, kind: AstOptionalKind, expr: Option<&Ast>) -> Result<()> {
        match kind {
            AstOptionalKind::Break => todo!(),
            AstOptionalKind::Continue => todo!(),
            AstOptionalKind::Return => {
                if let Some(expr) = expr {
                    let size = expr
                        .typ
                        .expect(
                            "[INTERNAL ERR] `sub_expression` of `Return` node doesn't have a type.",
                        )
                        .size();
                    self.compile_node(expr)?;
                    self.emit_return(size);
                } else {
                    self.emit_inst(Instruction::Ret_0);
                }
            }
        }

        Ok(())
    }

    fn compile_block(&mut self, kind: AstBlockKind, nodes: &[Ast]) -> Result<()> {
        let stack_top = self.stack_top();

        for node in nodes {
            self.compile_node(node)?;
        }

        if kind == AstBlockKind::Block {
            self.emit_flush(stack_top);
        }

        Ok(())
    }
}
