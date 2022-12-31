use crate::{typing::value_type::runtime_type, codegen::exe::Executable};

const NUM_REGISTERS: usize = 32 * std::mem::size_of::<runtime_type::Int>();
const STACK_SIZE: usize = std::u16::MAX as usize;

pub type Size = u16;
pub type Addr = u16;

struct Stack {
    buffer: [u8; STACK_SIZE],
    top: usize,
}

impl Stack {
    fn new() -> Self {
        Stack { buffer: [0; STACK_SIZE], top: 0 }
    }

    fn push(&mut self, data: &[u8]) {
        if self.top + data.len() >= STACK_SIZE {
            panic!("Stack overflow.");
        }

        let range = self.top..(self.top + data.len());
        self.buffer[range].copy_from_slice(data);
        self.top += data.len();
    }

    fn pop(&mut self, size: usize) -> &[u8] {
        if self.top < size {
            panic!("Stack underflow.");
        }

        self.top -= size;
        let data = &self.buffer[self.top..(self.top + size)];
        data
    }
}

pub struct VM<'exe> {
    registers: [u8; NUM_REGISTERS],
    stack: Stack,
    exe: &'exe Executable,
}

impl<'exe> VM<'exe> {
    pub fn new(exe: &'exe Executable) -> Self {
        Self {
            registers: [0; NUM_REGISTERS],
            stack: Stack::new(),
            exe,
        }
    }
}

impl<'exe> VM<'exe> {
    fn push<T>(&mut self, value: T)
    where
        T: Copy + Sized,
    {
        let size = std::mem::size_of_val(&value);

        let ptr = &value as *const T as *const u8;
        let data: &[u8] = unsafe { std::slice::from_raw_parts(ptr, size) };
        self.stack.push(data);
    }

    fn pop<T>(&mut self) -> T
    where
        T: Copy + Sized,
    {
        let size = std::mem::size_of::<T>();
        let data = self.stack.pop(size);
        let ptr = data.as_ptr() as *const T;
        unsafe { *ptr }
    }
}
