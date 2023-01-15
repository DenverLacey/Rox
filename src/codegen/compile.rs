use crate::{
    canon::scoping::{FuncID, Scope, ScopeBinding, ScopeIndex},
    interp::{Interpreter, ParsedFile},
    ir::{
        annotations::Annotations,
        ast::{
            Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoVar, AstUnaryKind, Queued,
            QueuedProgress, AstInfoIf,
        },
    },
    parsing::tokenization::{Token, TokenInfo},
    runtime::{
        builtins::{self, Builtin},
        vm::{Addr, Size},
    },
    typing::value_type::{
        runtime_type::{Bool, Char, Float, Int, Pointer},
        Type,
    },
    util::errors::{Result, SourceError},
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
            if !queued_ready_for_compile(&queued) {
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

    let deps = if matches!(queued.node.info, AstInfo::Fn(_)) {
        &queued.inner_deps
    } else {
        &queued.deps
    };

    for dep in deps {
        let dep = &interp.parsed_files[dep.parsed_file_idx].ast[dep.queued_idx];
        if dep.progress < QueuedProgress::Compiled {
            return false;
        }
    }

    true
}

fn is_node_compilable(node: &Ast) -> bool {
    match node.info {
        AstInfo::Import(_) | AstInfo::TypeSignature(_) => false,
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
        self.current_function_mut().code.push(byte);
    }

    fn emit_inst(&mut self, inst: Instruction) {
        assert_eq!(
            std::mem::size_of::<Instruction>(),
            std::mem::size_of::<u8>()
        );
        let byte = inst as u8;
        self.emit_byte(byte);
    }

    fn emit_value<T>(&mut self, value: T)
    where
        T: Copy + Sized,
    {
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

    fn emit_push_const_str(&mut self, idx: usize) {
        self.emit_inst(Instruction::PushConst_Str);
        self.emit_value(idx);
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

    fn emit_pop(&mut self, size: Size) {
        self.emit_inst(Instruction::Pop);
        self.emit_value(size);
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
        self.emit_inst(Instruction::Call);
        self.emit_value(size);
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
            self.set_stack_top(stack_top - Type::Bool.size());
        }

        jump
    }

    fn patch_jump(&mut self, jump: usize) {
        let to = self.current_function().code.len();
        let jump_size: &mut Addr = unsafe { std::mem::transmute(&mut self.current_function_mut().code[jump]) };
        *jump_size = (to - jump - std::mem::size_of::<Addr>()).try_into().expect("[INTERNAL ERR] jump too big to fit in an `Addr`.");
    }
}

impl Compiler {
    fn compile_node(&mut self, node: &Ast) -> Result<()> {
        match &node.info {
            AstInfo::Literal => self.compile_literal(node.scope, &node.token, node.typ),
            AstInfo::Unary(kind, expr) => self.compile_unary(*kind, node.typ, expr),
            AstInfo::Binary(kind, lhs, rhs) => self.compile_binary(*kind, node.typ, lhs, rhs),
            AstInfo::Block(kind, nodes) => self.compile_block(*kind, nodes),
            AstInfo::Fn(info) => self.compile_fn_decl(&node.token, info),
            AstInfo::Var(info) => self.compile_var_decl(node.scope, &node.token, info),
            AstInfo::TypeValue(typ) => todo!(),
            AstInfo::If(info) => self.compile_if_statement(info),

            AstInfo::Import(_) | AstInfo::TypeSignature(_) => unreachable!(),
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
                let fn_ptr = fn_ref as *const _ as *const ();
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
            if !self.exe.set_entry_point(func.id.0) {
                return Err(SourceError::new(
                    "Multiple entry points declared.",
                    token.loc,
                    "This was declared as the entry point when another function already has.",
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

    fn compile_unary(&mut self, kind: AstUnaryKind, typ: Option<Type>, expr: &Ast) -> Result<()> {
        match kind {
            AstUnaryKind::Neg | AstUnaryKind::Not | AstUnaryKind::XXXPrint => {
                self.compile_unary_typical(kind, typ, expr)
            }
            AstUnaryKind::Ref | AstUnaryKind::RefMut => todo!(),
            AstUnaryKind::Deref => todo!(),
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

        match (expr_type, kind) {
            (Type::Int, AstUnaryKind::Neg) => self.emit_inst(Instruction::Int_Neg),
            (Type::Float, AstUnaryKind::Neg) => self.emit_inst(Instruction::Float_Neg),

            (Type::Bool, AstUnaryKind::Not) => self.emit_inst(Instruction::Not),
            (Type::Int, AstUnaryKind::Not) => self.emit_inst(Instruction::Bit_Not),

            (Type::Bool, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(Type::Bool.size(), builtins::XXXprint_Bool)
            }
            (Type::Char, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(Type::Char.size(), builtins::XXXprint_Char)
            }
            (Type::Int, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(Type::Int.size(), builtins::XXXprint_Int)
            }
            (Type::Float, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(Type::Float.size(), builtins::XXXprint_Float)
            }
            (Type::String, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(Type::String.size(), builtins::XXXprint_String)
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
        typ: Option<Type>,
        lhs: &Ast,
        rhs: &Ast,
    ) -> Result<()> {
        match kind {
            AstBinaryKind::Add
            | AstBinaryKind::Sub
            | AstBinaryKind::Mul
            | AstBinaryKind::Div
            | AstBinaryKind::Mod => self.compile_binary_typical(kind, typ, lhs, rhs),
            AstBinaryKind::Assign => todo!(),
            AstBinaryKind::Call => self.compile_call(typ, lhs, rhs),
            AstBinaryKind::Subscript => todo!(),
            AstBinaryKind::Param => {
                panic!("[INTERNAL ERR] `Param` node not being handled by `compile_fn_decl()`.")
            }
            AstBinaryKind::ConstrainedVarDeclTarget => {
                panic!("[INTERNAL ERR] `ConstrainedVarDeclTarget` node not being handled in `compile_var_decl()`.")
            }
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

        match (lhs_type, rhs_type, kind) {
            (Type::Int, Type::Int, AstBinaryKind::Add) => self.emit_inst(Instruction::Int_Add),
            (Type::Int, Type::Int, AstBinaryKind::Sub) => self.emit_inst(Instruction::Int_Sub),
            (Type::Int, Type::Int, AstBinaryKind::Mul) => self.emit_inst(Instruction::Int_Mul),
            (Type::Int, Type::Int, AstBinaryKind::Div) => self.emit_inst(Instruction::Int_Div),
            (Type::Int, Type::Int, AstBinaryKind::Mod) => self.emit_inst(Instruction::Int_Mod),

            (Type::Float, Type::Float, AstBinaryKind::Add) => self.emit_inst(Instruction::Float_Add),
            (Type::Float, Type::Float, AstBinaryKind::Sub) => self.emit_inst(Instruction::Float_Sub),
            (Type::Float, Type::Float, AstBinaryKind::Mul) => self.emit_inst(Instruction::Float_Mul),
            (Type::Float, Type::Float, AstBinaryKind::Div) => self.emit_inst(Instruction::Float_Div),

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

    fn compile_block(&mut self, kind: AstBlockKind, nodes: &[Ast]) -> Result<()> {
        let stack_top = self.stack_top();

        for node in nodes {
            self.compile_node(node)?;
        }

        if kind == AstBlockKind::Block {
            let size = self.stack_top() - stack_top;
            if size != 0 {
                self.emit_pop(size);
            }
        }

        Ok(())
    }
}
