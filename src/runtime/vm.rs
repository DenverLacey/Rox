use crate::{codegen::{exe::Executable, inst::Instruction}, typing::value_type::runtime_type::{Int, Char, Float, Pointer}, util::errors::Result, interp::FunctionInfo};

const STACK_SIZE: usize = std::u16::MAX as usize;

pub type Size = u16;
pub type Addr = u16;

pub struct Stack {
    buffer: [u8; STACK_SIZE],
    top: usize,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            buffer: [0; STACK_SIZE],
            top: 0,
        }
    }

    pub fn get(&self, size: Size, addr: Addr) -> &[u8] {
        let size = size as usize;
        let addr = addr as usize;
        self.buffer.get(addr..(addr + size)).expect("Bad access to stack.")
    }

    pub fn push(&mut self, data: &[u8]) {
        if self.top + data.len() >= STACK_SIZE {
            panic!("Stack overflow.");
        }

        let range = self.top..(self.top + data.len());
        self.buffer[range].copy_from_slice(data);
        self.top += data.len();
    }

    pub fn pop(&mut self, size: Size) -> &[u8] {
        let size = size as usize;
        
        if self.top < size {
            panic!("Stack underflow.");
        }

        self.top -= size;
        let data = &self.buffer[self.top..(self.top + size)];
        data
    }

    fn push_value<T>(&mut self, value: T)
    where
        T: Copy + Sized,
    {
        let size = std::mem::size_of_val(&value);
        let ptr = &value as *const T as *const u8;
        let data: &[u8] = unsafe { std::slice::from_raw_parts(ptr, size) };
        self.push(data);
    }

    fn pop_value<T>(&mut self) -> T
    where
        T: Copy + Sized,
    {
        let size = std::mem::size_of::<T>();
        let size = size.try_into().expect("sizeof T cannot fit into a `Size`.");
        let data = self.pop(size);
        let ptr = data.as_ptr() as *const T;
        unsafe { *ptr }
    }
}

struct CallFrame {
    ip: usize,
    code: &'static [u8],
    stack_bottom: Addr,
}

impl CallFrame {
    fn read<T: Copy + Sized>(&mut self) -> T {
        let size = std::mem::size_of::<T>();
        let data = self.code.get(self.ip..(self.ip + size)).expect("[INTERNAL ERR] Attempt to read value failed due to not enough bytes in bytecode remaining.");
        let ptr = data.as_ptr() as *const T;
        let value = unsafe { *ptr };

        self.ip += size;
        value
    }
}

pub struct VM<'exe> {
    stack: Stack,
    frames: Vec<CallFrame>,
    exe: &'exe Executable,
    ip: usize,
}

impl<'exe> VM<'exe> {
    pub fn new(exe: &'exe Executable) -> Self {
        Self {
            stack: Stack::new(),
            frames: Vec::new(),
            exe,
            ip: 0,
        }
    }

    pub fn execute(&mut self) -> Result<()> {
        self.run_global_scope()
    }
}

impl<'exe> VM<'exe> {
    fn run_global_scope(&mut self) -> Result<()> {
        let global_scope = &self.exe.funcs[0];
        self.call(global_scope, 0);
        self.run()
    }

    fn run(&mut self) -> Result<()> {
        let mut frame = self.frames.last_mut().expect("[INTERNAL ERR] No frames to run.");

        while frame.ip < frame.code.len() {
            let inst: Instruction = frame.read();

            use Instruction::*;
            match inst {
                NoOp => {}

                // Literals
                Lit_True => self.stack.push_value(true),
                Lit_False => self.stack.push_value(false),
                Lit_0 => self.stack.push_value(0 as Int),
                Lit_1 => self.stack.push_value(1 as Int),
                Lit_Char => {
                    let c: Char = frame.read();
                    self.stack.push_value(c);
                }
                Lit_Int => {
                    let k: Int = frame.read();
                    self.stack.push_value(k);
                }
                Lit_Float => {
                    let f: Float = frame.read();
                    self.stack.push_value(f);
                }
                Lit_Pointer => {
                    let p: Pointer = frame.read();
                    self.stack.push_value(p);
                }

                // Constants
                PushConst => {
                    let size: Size = frame.read();
                    let idx: usize = frame.read();
                    let constant = self.exe.constants.get(idx..(idx + size as usize)).expect("[INTERNAL ERR] Bad constant!!!");
                    self.stack.push(constant);
                }
                PushConst_Str => todo!(),

                // Arithmetic
                Int_Add => todo!(),
                Int_Sub => todo!(),
                Int_Mul => todo!(),
                Int_Div => todo!(),
                Int_Neg => todo!(),
                Int_Mod => todo!(),
                Int_Inc => todo!(),
                Int_Dec => todo!(),

                Float_Add => todo!(),
                Float_Sub => todo!(),
                Float_Mul => todo!(),
                Float_Div => todo!(),
                Float_Neg => todo!(),

                // Bitwise
                Bit_Not => todo!(),
                Bit_Shl => todo!(),
                Bit_Shr => todo!(),
                Bit_And => todo!(),
                Bit_Or => todo!(),
                Bit_Xor => todo!(),

                // Logic
                And => todo!(),
                Or => todo!(),
                Not => todo!(),

                // Comparison
                Eq => {
                    let size: Size = frame.read();
                    let b = self.stack.pop(size);
                    let a = self.stack.pop(size);
                    let result = a.iter().eq(b.iter());
                    self.stack.push_value(result);
                }
                Ne => {
                    let size: Size = frame.read();
                    let b = self.stack.pop(size);
                    let a = self.stack.pop(size);
                    let result = a.iter().ne(b.iter());
                    self.stack.push_value(result);
                }
                Str_Eq => todo!(),
                Str_Ne => todo!(),
                Int_Lt => todo!(),
                Int_Le => todo!(),
                Int_Gt => todo!(),
                Int_Ge => todo!(),
                Float_Lt => todo!(),
                Float_Le => todo!(),
                Float_Gt => todo!(),
                Float_Ge => todo!(),

                // Stack Operations
                Move => todo!(),
                Dup => {
                    let size: Size = frame.read();
                    let addr: Addr = frame.read();
                    let data = self.stack.get(size, addr + frame.stack_bottom);
                    self.stack.push(data);
                }
                DupGlobal => {
                    let size: Size = frame.read();
                    let addr: Addr = frame.read();
                    let data = self.stack.get(size, addr);
                    self.stack.push(data);
                }
                PushPtr => todo!(),
                PushPtrGlobal => todo!(),
                Pop => {
                    let size: Size = frame.read();
                    self.stack.pop(size);
                }

                // Branching
                Jump => {
                    let jump: Addr = frame.read();
                    frame.ip += jump as usize;
                }
                Loop => {
                    let jump: Addr = frame.read();
                    frame.ip -= jump as usize;
                }
                JumpTrue => todo!(),
                JumpFalse => todo!(),
                JumpTrueNoPop => todo!(),
                JumpFalseNoPop => todo!(),

                // Invocation
                Call => todo!(),
                CallBuiltin => todo!(),

                Ret => todo!(),
            }
        }

        Ok(())
    }

    fn call(&mut self, func: &FunctionInfo, arg_size: Size) {
        let stack_top: Addr = self.stack.top.try_into().expect("[INTERNAL ERR] top of stack cannot fit in a `Size`.");
        let stack_bottom = stack_top - arg_size;

        let frame = CallFrame {
            ip: 0,
            code: unsafe { std::mem::transmute(func.code.as_ref().unwrap().as_ref()) },
            stack_bottom,
        };

        self.frames.push(frame);
    }
}
