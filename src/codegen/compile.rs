use crate::{
    canon::scoping::{ScopeBinding, ScopeIndex, FuncID},
    interp::{Interpreter, ParsedFile},
    ir::{
        annotations::Annotations,
        ast::{
            Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoVar, AstUnaryKind,
            QueuedProgress, VariableInitializer, Queued,
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

    let global_scope = compiler.func_builders.pop().expect("[INTERNAL ERR] No global scope remaining after compilation.");
    assert!(compiler.func_builders.is_empty(), "FunctionBuilder's still left over after compilation.");

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
        self.func_builders.last_mut().expect("[INTERNAL ERR] No function builders.")
    }

    fn current_function(&self) -> &FunctionBuilder {
        self.func_builders.last().expect("[INTERNAL ERR] No function builders.")
    }

    fn begin_build_function(&mut self, id: FuncID) {
        let new = FunctionBuilder {
            func_id: id,
            ..Default::default()
        };

        self.func_builders.push(new);
    }

    fn end_function(&mut self, id: FuncID) {
        let last = self.func_builders.pop().expect("[INTERNAL ERR] No builder's to end.");
        assert_eq!(last.func_id, id, "[INTERNAL ERR] Attempted to end a function builder with the wrong ID. {:?} vs. {:?}", last.func_id, id);

        last.build();
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

    fn emit_call(&mut self, size: Size) {
        self.emit_inst(Instruction::Call);
        self.emit_value(size);
    }

    fn emit_call_builtin(&mut self, size: Size, builtin: Builtin) {
        self.emit_inst(Instruction::CallBuiltin);
        self.emit_value(size);
        self.emit_value(builtin);
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

            AstInfo::Import(_) | AstInfo::TypeSignature(_) => unreachable!(),
        }
    }

    fn compile_literal(&mut self, scope: ScopeIndex, token: &Token, typ: Option<Type>) -> Result<()> {
        let stack_top = self.stack_top();

        match &token.info {
            TokenInfo::Ident(ident) => self.compile_ident(scope, ident)?,
            TokenInfo::Bool(value) => self.emit_bool(*value),
            TokenInfo::Int(value) => self.emit_int(*value),
            TokenInfo::Float(value) => self.emit_float(*value),
            TokenInfo::String(value) => todo!(),
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

                size = std::mem::size_of::<Pointer>().try_into().expect("[INTERNAL ERR] Size of `Pointer` doesn't fit in a `Size`.");
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

        self.begin_build_function(id);

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

        self.end_function(id);

        Ok(())
    }

    fn compile_var_decl(
        &mut self,
        scope: ScopeIndex,
        token: &Token,
        info: &AstInfoVar,
    ) -> Result<()> {
        match &info.targets.info {
            AstInfo::Literal => {
                let TokenInfo::Ident(ident) = &info.targets.token.info else {
                    panic!("[INTERNAL ERR] `targets` node in var decl node not an `Ident` node.");
                };

                let init_expr = match &info.initializer {
                    VariableInitializer::Expr(expr) => expr,
                    _ => todo!(),
                };
                self.compile_var_decl_one_target(scope, ident, init_expr)
            }
            AstInfo::Block(AstBlockKind::VarDeclTargets, target_nodes) => {
                todo!()
            }
            _ => panic!(
                "[INTERNAL ERR] Invalid node kind `{:?}` for `targets` node in var decl.",
                info.targets.info
            ),
        }
    }

    fn compile_var_decl_one_target(
        &mut self,
        scope: ScopeIndex,
        ident: &str,
        init_expr: &Ast,
    ) -> Result<()> {
        let interp = Interpreter::get_mut();
        let scope = &mut interp.scopes[scope.0];

        let Some(binding) = scope.find_binding_mut(ident) else {
            panic!("[INTERNAL ERR] Unresolvable identifier `{}`.", ident);
        };

        let ScopeBinding::Var(var) = binding else {
            panic!("[INTERNAL ERR] ident `{}` doesn't bind to a variable but a `{:?}`.", ident, binding);
        };

        let stack_top = self.stack_top();
        var.addr = stack_top;

        self.compile_node(init_expr)?;

        let size = init_expr.typ.unwrap().size();
        self.set_stack_top(stack_top + size);

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

    fn compile_unary_typical(&mut self, kind: AstUnaryKind, typ: Option<Type>, expr: &Ast) -> Result<()> {
        let stack_top = self.stack_top();
        let expr_type = expr.typ.unwrap();

        self.compile_node(expr)?;

        match (expr_type, kind) {
            (Type::Int, AstUnaryKind::Neg) => self.emit_inst(Instruction::Int_Neg),
            (Type::Float, AstUnaryKind::Neg) => self.emit_inst(Instruction::Float_Neg),

            (Type::Bool, AstUnaryKind::Not) => self.emit_inst(Instruction::Not),
            (Type::Int, AstUnaryKind::Not) => self.emit_inst(Instruction::Bit_Not),

            (Type::Int, AstUnaryKind::XXXPrint) => {
                self.emit_call_builtin(Type::Int.size(), builtins::XXXprint)
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

    fn compile_binary(&mut self, kind: AstBinaryKind, typ: Option<Type>, lhs: &Ast, rhs: &Ast) -> Result<()> {
        match kind {
            AstBinaryKind::Add
            | AstBinaryKind::Sub
            | AstBinaryKind::Mul
            | AstBinaryKind::Div
            | AstBinaryKind::Mod => self.compile_binary_typical(kind, typ, lhs, rhs),
            AstBinaryKind::Assign => todo!(),
            AstBinaryKind::Call => self.compile_call(typ, lhs, rhs),
            AstBinaryKind::Subscript => todo!(),
            AstBinaryKind::Param => panic!("[INTERNAL ERR] `Param` node not being handled by `compile_fn_decl()`."),
        }
    }

    fn compile_binary_typical(&mut self, kind: AstBinaryKind, typ: Option<Type>, lhs: &Ast, rhs: &Ast) -> Result<()> {
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

    fn compile_call(&mut self, typ: Option<Type>, callee: &Ast, args: &Ast) -> Result<()> {
        let stack_top_before_args = self.stack_top();

        self.compile_node(args)?;

        let stack_top_after_args = self.stack_top();
        let arg_size = stack_top_after_args - stack_top_before_args;

        self.compile_node(callee)?;
        self.emit_call(arg_size);

        let size = typ.map_or(0, |t| t.size());
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
