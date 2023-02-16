use std::cell::UnsafeCell;

use crate::{
    codegen::{exe::Executable, inst::Instruction},
    interp::{FunctionInfo, Interpreter},
    runtime::builtins::Builtin,
    typing::value_type::{
        runtime_type::{self, Bool, Char, Float, Int, Pointer},
        Type, TypeInfo, TypeKind,
    },
    util::{byte_reader::ByteReader, errors::Result},
};

const STACK_SIZE: usize = std::u16::MAX as usize;

pub type Size = u16;
pub type Addr = u16;

struct StackInner {
    buffer: [u8; STACK_SIZE],
    top: usize,
}

pub struct Stack {
    inner: UnsafeCell<StackInner>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            inner: UnsafeCell::new(StackInner {
                buffer: [0; STACK_SIZE],
                top: 0,
            }),
        }
    }

    pub fn top(&self) -> usize {
        let me = unsafe { &*self.inner.get() };
        me.top
    }

    pub fn get_buffer(&self) -> &[u8] {
        let me = unsafe { &*self.inner.get() };
        me.buffer.as_slice()
    }

    pub fn get(&self, size: Size, addr: Addr) -> &[u8] {
        let size = size as usize;
        let addr = addr as usize;

        let me = unsafe { &*self.inner.get() };
        me.buffer
            .get(addr..(addr + size))
            .expect("Bad access to stack")
    }

    pub fn alloc(&self, size: Size) {
        let me = unsafe { &mut *self.inner.get() };

        if me.top + size as usize >= STACK_SIZE {
            panic!("Stack overflow.");
        }

        me.top += size as usize;
    }

    pub fn calloc(&self, size: Size) {
        let me = unsafe { &mut *self.inner.get() };

        me.buffer
            .get_mut(me.top..(me.top + size as usize))
            .expect("Stack overflow.")
            .fill(0);

        me.top += size as usize;
    }

    pub fn flush(&self, new_top: Addr) {
        let me = unsafe { &mut *self.inner.get() };
        me.top = new_top as usize;
    }

    pub fn push(&self, data: &[u8]) {
        let me = unsafe { &mut *self.inner.get() };

        if me.top + data.len() >= STACK_SIZE {
            panic!("Stack overflow.");
        }

        let range = me.top..(me.top + data.len());
        me.buffer[range].copy_from_slice(data);
        me.top += data.len();
    }

    pub fn pop(&self, size: Size) -> &[u8] {
        let me = unsafe { &mut *self.inner.get() };
        let size = size as usize;

        if me.top < size {
            panic!("Stack underflow.");
        }

        me.top -= size;
        let data = &me.buffer[me.top..(me.top + size)];

        data
    }

    pub fn insert(&self, addr: Addr, data: &[u8]) {
        let me = unsafe { &mut *self.inner.get() };
        let addr = addr as usize;

        let dst = me
            .buffer
            .get_mut(addr..addr + data.len())
            .expect("Stack overflow.");
        dst.copy_from_slice(data);
    }

    pub fn push_value<T>(&self, value: T)
    where
        T: Copy + Sized,
    {
        let size = std::mem::size_of_val(&value);
        let ptr = &value as *const T as *const u8;
        let data: &[u8] = unsafe { std::slice::from_raw_parts(ptr, size) };
        self.push(data);
    }

    pub fn pop_value<T>(&self) -> T
    where
        T: Copy + Sized,
    {
        let size = std::mem::size_of::<T>();
        let size = size
            .try_into()
            .expect("[INTERAL ERR] sizeof T cannot fit into a `Size`.");
        let data = self.pop(size);
        let ptr = data.as_ptr() as *const T;
        unsafe { *ptr }
    }

    pub fn top_value<T>(&self) -> T
    where
        T: Copy + Sized,
    {
        let size = std::mem::size_of::<T>();
        let me = unsafe { &*self.inner.get() };
        let data = &me.buffer[me.top - size];
        let ptr = data as *const u8 as *const T;
        unsafe { *ptr }
    }
}

struct CallFrame {
    reader: ByteReader<'static>,
    stack_bottom: Addr,
}

macro_rules! un_op {
    ($T:ty, $TR:ty, $op:tt, $me:ident) => {{
        let a: $T = $me.stack.pop_value();
        let r: $TR = $op a;
        $me.stack.push_value(r);
    }};
}

macro_rules! bin_op {
    ($TA:ty, $TB:ty, $TR:ty, $op:tt, $me:ident) => {{
        let b: $TB = $me.stack.pop_value();
        let a: $TA = $me.stack.pop_value();
        let r: $TR = a $op b;
        $me.stack.push_value(r);
    }};
}

pub struct VM<'exe> {
    stack: Stack,
    frames: Vec<CallFrame>,
    exe: &'exe Executable,
}

impl<'exe> VM<'exe> {
    pub fn new(exe: &'exe Executable) -> Self {
        Self {
            stack: Stack::new(),
            frames: Vec::new(),
            exe,
        }
    }

    pub fn execute(&mut self) -> Result<()> {
        self.run_global_scope()?;
        if cfg!(debug_assertions) {
            println!("Stack after global scope run:");
            self.print_stack(&[Type::of(TypeKind::Int)]);
            println!();
        }

        self.run_from_entry()?;

        Ok(())
    }
}

impl<'exe> VM<'exe> {
    fn run_global_scope(&mut self) -> Result<()> {
        let interp = Interpreter::get();
        assert!(!interp.funcs.is_empty(), "No global scope to execute.");
        let global_scope = &interp.funcs[0];
        self.call(global_scope, 0);
        self.run()
    }

    fn run_from_entry(&mut self) -> Result<()> {
        let interp = Interpreter::get();
        let entry = &interp.funcs[self.exe.entry_point];
        self.call(entry, 0);
        self.run()
    }

    fn run(&mut self) -> Result<()> {
        let mut frame = self
            .frames
            .last_mut()
            .expect("[INTERNAL ERR] No frames to run.");

        while frame.reader.offset() < frame.reader.bytes().len() {
            let inst: Instruction = frame.reader.read();

            use Instruction::*;
            match inst {
                NoOp => {}

                // Literals
                Lit_True => self.stack.push_value(true),
                Lit_False => self.stack.push_value(false),
                Lit_0 => self.stack.push_value(0 as Int),
                Lit_1 => self.stack.push_value(1 as Int),
                Lit_Char => {
                    let c: Char = frame.reader.read();
                    self.stack.push_value(c);
                }
                Lit_Int => {
                    let k: Int = frame.reader.read();
                    self.stack.push_value(k);
                }
                Lit_Float => {
                    let f: Float = frame.reader.read();
                    self.stack.push_value(f);
                }
                Lit_Pointer => {
                    let p: Pointer = frame.reader.read();
                    self.stack.push_value(p);
                }

                // Constants
                PushConst => {
                    let size: Size = frame.reader.read();
                    let idx: usize = frame.reader.read();
                    let constant = self
                        .exe
                        .constants
                        .get(idx..(idx + size as usize))
                        .expect("[INTERNAL ERR] Bad constant!!!");
                    self.stack.push(constant);
                }
                PushConst_Str => {
                    let idx: usize = frame.reader.read();

                    let len =
                        unsafe { *std::mem::transmute::<&u8, &i64>(&self.exe.str_constants[idx]) };
                    let chars =
                        (&self.exe.str_constants[idx + std::mem::size_of::<i64>()]) as *const u8;
                    let s = runtime_type::String { len, chars };

                    self.stack.push_value(s);
                }

                // Arithmetic
                Int_Add => bin_op!(Int, Int, Int, +, self),
                Int_Sub => bin_op!(Int, Int, Int, -, self),
                Int_Mul => bin_op!(Int, Int, Int, *, self),
                Int_Div => bin_op!(Int, Int, Int, /, self),
                Int_Neg => un_op!(Int, Int, -, self),
                Int_Mod => bin_op!(Int, Int, Int, %, self),
                Int_Inc => todo!(),
                Int_Dec => todo!(),

                Float_Add => bin_op!(Float, Float, Float, +, self),
                Float_Sub => bin_op!(Float, Float, Float, -, self),
                Float_Mul => bin_op!(Float, Float, Float, *, self),
                Float_Div => bin_op!(Float, Float, Float, /, self),
                Float_Neg => un_op!(Float, Float, -, self),

                // Bitwise
                Bit_Not => un_op!(Int, Int, !, self),
                Bit_Shl => bin_op!(Int, Int, Int, <<, self),
                Bit_Shr => bin_op!(Int, Int, Int, >>, self),
                Bit_And => bin_op!(Int, Int, Int, &, self),
                Bit_Or => bin_op!(Int, Int, Int, |, self),
                Bit_Xor => bin_op!(Int, Int, Int, ^, self),

                // Logic
                And => bin_op!(Bool, Bool, Bool, &&, self),
                Or => bin_op!(Bool, Bool, Bool, ||, self),
                Not => un_op!(Bool, Bool, !, self),

                // Comparison
                Eq => {
                    let size: Size = frame.reader.read();
                    let b = self.stack.pop(size);
                    let a = self.stack.pop(size);
                    let result = a.iter().eq(b.iter());
                    self.stack.push_value(result);
                }
                Ne => {
                    let size: Size = frame.reader.read();
                    let b = self.stack.pop(size);
                    let a = self.stack.pop(size);
                    let result = a.iter().ne(b.iter());
                    self.stack.push_value(result);
                }
                Str_Eq => todo!(),
                Str_Ne => todo!(),
                Int_Lt => bin_op!(Int, Int, Bool, <, self),
                Int_Le => bin_op!(Int, Int, Bool, <=, self),
                Int_Gt => bin_op!(Int, Int, Bool, >, self),
                Int_Ge => bin_op!(Int, Int, Bool, >=, self),
                Float_Lt => bin_op!(Float, Float, Bool, <, self),
                Float_Le => bin_op!(Float, Float, Bool, <=, self),
                Float_Gt => bin_op!(Float, Float, Bool, >, self),
                Float_Ge => bin_op!(Float, Float, Bool, >=, self),

                // Stack Operations
                Move => {
                    let size: Size = frame.reader.read();
                    let dst: Pointer = self.stack.pop_value();
                    let src = self.stack.pop(size);

                    let dst =
                        unsafe { std::slice::from_raw_parts_mut(dst as *mut u8, size as usize) };
                    dst.copy_from_slice(src);
                }
                MoveImm => {
                    let size: Size = frame.reader.read();
                    let addr: Addr = frame.reader.read();
                    let data = self.stack.pop(size);
                    self.stack.insert(addr + frame.stack_bottom, data);
                }
                MoveImmGlobal => {
                    let size: Size = frame.reader.read();
                    let addr: Addr = frame.reader.read();
                    let data = self.stack.pop(size);
                    self.stack.insert(addr, data);
                }
                Dup => {
                    let size: Size = frame.reader.read();
                    let addr: Addr = frame.reader.read();
                    let data = self.stack.get(size, addr + frame.stack_bottom);
                    self.stack.push(data);
                }
                DupGlobal => {
                    let size: Size = frame.reader.read();
                    let addr: Addr = frame.reader.read();
                    let data = self.stack.get(size, addr);
                    self.stack.push(data);
                }
                Load => {
                    let size: Size = frame.reader.read();
                    let p: Pointer = self.stack.pop_value();

                    let src = unsafe { std::slice::from_raw_parts(p as *mut u8, size as usize) };
                    self.stack.push(src);
                }
                PushPtr => {
                    let addr: Addr = frame.reader.read();
                    let ptr = &self.stack.get_buffer()[(addr + frame.stack_bottom) as usize];
                    let ptr = ptr as *const u8 as Pointer;
                    self.stack.push_value(ptr);
                }
                PushPtrGlobal => {
                    let addr: Addr = frame.reader.read();
                    let ptr = &self.stack.get_buffer()[addr as usize];
                    let ptr = ptr as *const u8 as Pointer;
                    self.stack.push_value(ptr);
                }
                Pop => {
                    let size: Size = frame.reader.read();
                    self.stack.pop(size);
                }
                Flush => {
                    let addr: Addr = frame.reader.read();
                    self.stack.flush(addr);
                }
                Alloc => {
                    let size: Size = frame.reader.read();
                    self.stack.alloc(size);
                }
                AllocZ => {
                    let size: Size = frame.reader.read();
                    self.stack.calloc(size);
                }

                // Branching
                Jump => {
                    let jump: Addr = frame.reader.read();
                    frame.reader.jump(jump as usize);
                }
                JumpBack => {
                    let jump: Addr = frame.reader.read();
                    frame.reader.jump_back(jump as usize);
                }
                JumpTrue => {
                    let jump: Addr = frame.reader.read();
                    let cond: Bool = self.stack.pop_value();

                    if cond {
                        frame.reader.jump(jump as usize);
                    }
                }
                JumpFalse => {
                    let jump: Addr = frame.reader.read();
                    let cond: Bool = self.stack.pop_value();

                    if !cond {
                        frame.reader.jump(jump as usize);
                    }
                }
                JumpTrueNoPop => {
                    let jump: Addr = frame.reader.read();
                    let cond: Bool = self.stack.top_value();

                    if cond {
                        frame.reader.jump(jump as usize);
                    }
                }
                JumpFalseNoPop => {
                    let jump: Addr = frame.reader.read();
                    let cond: Bool = self.stack.top_value();

                    if !cond {
                        frame.reader.jump(jump as usize);
                    }
                }

                // Invocation
                Call => {
                    let size: Size = frame.reader.read();
                    let f: &FunctionInfo = self.stack.pop_value();
                    self.call(f, size);
                    frame = self
                        .frames
                        .last_mut()
                        .expect("[INTERNAL ERR] Function call did not push a `CallFrame`.");
                }
                Call_0 => {
                    let f: &FunctionInfo = self.stack.pop_value();
                    self.call(f, 0);
                    frame = self
                        .frames
                        .last_mut()
                        .expect("[INTERNAL ERR] Function call did not push a `CallFrame`.");
                }
                CallBuiltin => {
                    let size: Size = frame.reader.read();
                    let builtin: Builtin = frame.reader.read();
                    builtin(&mut self.stack, size);
                }
                Ret_0 => {
                    let reset_point = frame.stack_bottom;
                    self.stack.flush(reset_point);

                    self.frames.pop();
                    frame = self
                        .frames
                        .last_mut()
                        .expect("[INTERNAL ERR] No other frame to return to.");
                }
                Ret => {
                    let size: Size = frame.reader.read();
                    let reset_point = frame.stack_bottom;

                    let return_value = self.stack.pop(size);
                    self.stack.flush(reset_point);
                    self.stack.push(return_value);

                    self.frames.pop();
                    frame = self
                        .frames
                        .last_mut()
                        .expect("[INTERNAL ERR] No other frame to return to.");
                }
            }
        }

        Ok(())
    }

    fn call(&mut self, func: &FunctionInfo, arg_size: Size) {
        let stack_top: Addr = self
            .stack
            .top()
            .try_into()
            .expect("[INTERNAL ERR] top of stack cannot fit in a `Size`.");
        let stack_bottom = stack_top - arg_size;

        let frame = CallFrame {
            reader: ByteReader::new(unsafe {
                std::mem::transmute(func.code.as_ref().unwrap().as_ref())
            }),
            stack_bottom,
        };

        self.frames.push(frame);
    }

    fn print_stack(&self, fmt: &[Type]) {
        let stack = self.stack.get_buffer();
        let mut reader = ByteReader::new(stack);

        for &typ in fmt {
            let value_idx = reader.offset();

            match typ.kind {
                TypeKind::Bool => {
                    let value: Bool = reader.read();
                    println!("{:04X}: {:?}", value_idx, value);
                }
                TypeKind::Char => {
                    let value: Char = reader.read();
                    println!("{:04X}: {:?}", value_idx, value);
                }
                TypeKind::Int => {
                    let value: Int = reader.read();
                    println!("{:04X}: {:?}", value_idx, value);
                }
                TypeKind::Float => {
                    let value: Float = reader.read();
                    println!("{:04X}: {:?}", value_idx, value);
                }
                TypeKind::String => {
                    let value: runtime_type::String = reader.read();
                    let value =
                        unsafe { std::slice::from_raw_parts(value.chars, value.len as usize) };
                    let value = std::str::from_utf8(value)
                        .expect("[INTERNAL ERR] String value in stack not valid UTF-8.");
                    println!("{:04X}: {:?}", value_idx, value);
                }
                TypeKind::Type => todo!(),
                TypeKind::Composite(idx) => {
                    let interp = Interpreter::get();
                    let typ = &interp.types[idx];
                    match typ {
                        TypeInfo::Pointer(_) => {
                            let value: Pointer = reader.read();
                            println!("{:04X}: {:?}", value_idx, value);
                        }
                        TypeInfo::Array(info) => todo!(),
                        TypeInfo::Struct(info) => todo!(),
                        TypeInfo::Enum(info) => {
                            let value: Int = reader.read();
                            let variant = info.variants.iter().find(|var| var.value == value).expect(format!("[INTERNAL ERR] Integer value `{}` is not valid for enum type `{}`.", value, info.name).as_str());
                            println!("{:04X}: {}.{}", value_idx, info.name, variant.name);
                        }
                        TypeInfo::Function(info) => todo!(),
                    }
                }
            }
        }

        while reader.offset() < self.stack.top() {
            let byte_idx = reader.offset();
            let byte: u8 = reader.read();
            println!("{:04X}: {:?}", byte_idx, byte);
        }
    }
}
