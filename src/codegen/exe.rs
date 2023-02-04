use crate::{
    interp::Interpreter,
    runtime::{
        builtins::Builtin,
        vm::{Addr, Size},
    },
    typing::value_type::runtime_type::{Char, Float, Int, Pointer},
    util::{
        byte_reader::ByteReader,
        errors::{error, Result},
    },
};

use super::inst::Instruction;

pub struct Executable {
    pub constants: Box<[u8]>,
    pub str_constants: Box<[u8]>,
    // pub funcs: Box<[FunctionInfo]>,
    pub entry_point: usize,
}

pub struct ExecutableBuilder {
    constants: Vec<u8>,
    str_constants: Vec<u8>,
    // funcs: Vec<FunctionInfo>,
    entry_point: Option<usize>,
}

impl ExecutableBuilder {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            str_constants: Vec::new(),
            // funcs: Vec::new(),
            entry_point: None,
        }
    }

    pub fn add_constant(&mut self, bytes: &[u8]) -> usize {
        const CONST_ALIGN: usize = 8;

        let size = bytes.len();
        let aligned_size = ((size + CONST_ALIGN - 1) / CONST_ALIGN) * CONST_ALIGN;
        let mut i = 0;
        while i < self.constants.len() {
            if i + aligned_size > self.constants.len() {
                // this constant can't fit so it can't already be in the constants block.
                break;
            }

            if bytes.iter().eq(self
                .constants
                .get(i..(i + size))
                .expect("[INTERNAL ERR] Failed to compare constants.")
                .iter())
            {
                return i;
            }

            i += CONST_ALIGN;
        }

        let idx = self.constants.len();

        let mut i = 0;
        while i < size {
            let byte = bytes[i];
            self.constants.push(byte);
            i += 1;
        }

        while i < aligned_size {
            self.constants.push(0);
            i += 1;
        }

        idx
    }

    pub fn add_str_constant(&mut self, slice: &[u8]) -> usize {
        if let Some(existing) = self.find_existing_slice(slice) {
            return existing;
        }

        let idx = self.str_constants.len();

        unsafe {
            let mut p = &slice.len() as *const usize as *const u8;
            for _ in 0..std::mem::size_of_val(&slice.len()) {
                let byte = *p;
                self.str_constants.push(byte);

                p = p.add(1);
            }
        }

        for byte in slice {
            self.str_constants.push(*byte);
        }

        idx
    }

    pub fn set_entry_point(&mut self, entry_point: usize) -> bool {
        if self.entry_point.is_some() {
            return false;
        }

        self.entry_point = Some(entry_point);
        true
    }

    pub fn build(self) -> Result<Executable> {
        let constants = self.constants.into_boxed_slice();
        let str_constants = self.str_constants.into_boxed_slice();
        // let funcs = self.funcs.into_boxed_slice();

        Ok(Executable {
            constants,
            str_constants,
            // funcs,
            entry_point: self
                .entry_point
                .ok_or_else(|| error!("No entry point function designated."))?,
        })
    }
}

impl ExecutableBuilder {
    fn find_existing_slice(&self, slice: &[u8]) -> Option<usize> {
        let mut i = 0;
        while i < self.str_constants.len() {
            let Some(len_bytes) = self.str_constants.get(i..(i + std::mem::size_of::<usize>())) else {
                break;
            };

            let idx = i;
            let len = unsafe { *(len_bytes.as_ptr() as *const usize) };
            i += len_bytes.len();

            let bytes = self
                .str_constants
                .get(i..(i + len))
                .expect("[INTERNAL ERR] Bad length.");
            if slice.iter().eq(bytes.iter()) {
                return Some(idx);
            }

            i += len;
        }

        None
    }
}

impl Executable {
    pub fn print_instructions(&self) {
        let interp = Interpreter::get();
        let funcs = interp.funcs.iter();
        for func in funcs {
            println!("{}#{:04}: {}", func.name, func.id.0, func.typ);
            if let Some(instructions) = func.code.as_ref() {
                print_instructions(&self.constants, &self.str_constants, instructions);
            } else {
                println!("Not compiled.");
            }
            println!();
        }
    }
}

fn print_instructions(constants: &[u8], str_constants: &[u8], instructions: &[u8]) {
    let mut reader = ByteReader::new(instructions);

    while reader.offset() < reader.bytes().len() {
        let inst_idx = reader.offset();
        let inst: Instruction = reader.read();

        use Instruction::*;
        match inst {
            NoOp => println!("{:04X}: NoOp", inst_idx),

            // Literals
            Lit_True | Lit_False | Lit_0 | Lit_1 => println!("{:04X}: {:?}", inst_idx, inst),
            Lit_Char => {
                let c: Char = reader.read();
                println!("{:04X}: Lit_Char '{}'", inst_idx, c);
            }
            Lit_Int => {
                let k: Int = reader.read();
                println!("{:04X}: Lit_Int {}", inst_idx, k);
            }
            Lit_Float => {
                let f: Float = reader.read();
                println!("{:04X}: Lit_Float {}", inst_idx, f);
            }
            Lit_Pointer => {
                let p: Pointer = reader.read();
                println!("{:04X}: Lit_Pointer {:?}", inst_idx, p);
            }

            // Constants
            PushConst => {
                let size: Size = reader.read();
                let idx: usize = reader.read();
                let constant = constants
                    .get(idx..(idx + size as usize * 8))
                    .expect("[INTERNAL ERR] Bad constant!!!");
                println!(
                    "{:04X}: PushConst [{}] {}b {:?}",
                    inst_idx,
                    idx,
                    size * 8,
                    constant
                );
            }
            PushConst_Str => {
                let idx: usize = reader.read();

                let len = unsafe { *std::mem::transmute::<&u8, &usize>(&str_constants[idx]) };
                let chars = (&str_constants[idx + std::mem::size_of::<i64>()]) as *const u8;

                let slice = unsafe { std::slice::from_raw_parts(chars, len) };
                let constant = unsafe { std::str::from_utf8_unchecked(slice) };

                println!("{:04X}: PushConst_Str [{}] {:?}", inst_idx, idx, constant);
            }

            // Arithmetic
            Int_Add | Int_Sub | Int_Mul | Int_Div | Int_Neg | Int_Mod | Int_Inc | Int_Dec => {
                println!("{:04X}: {:?}", inst_idx, inst)
            }

            Float_Add | Float_Sub | Float_Mul | Float_Div | Float_Neg => {
                println!("{:04X}: {:?}", inst_idx, inst)
            }

            // Bitwise
            Bit_Not | Bit_Shl | Bit_Shr | Bit_And | Bit_Or | Bit_Xor => {
                println!("{:04X}: {:?}", inst_idx, inst)
            }

            // Logic
            And | Or | Not => println!("{:04X}: {:?}", inst_idx, inst),

            // Comparison
            Eq => {
                let size: Size = reader.read();
                println!("{:04X}: Eq {}b", inst_idx, size * 8);
            }
            Ne => {
                let size: Size = reader.read();
                println!("{:04X}: Ne {}b", inst_idx, size * 8);
            }
            Str_Eq | Str_Ne | Int_Lt | Int_Le | Int_Gt | Int_Ge | Float_Lt | Float_Le
            | Float_Gt | Float_Ge => println!("{:04X}: {:?}", inst_idx, inst),

            // Stack Operations
            Move => {
                let size: Size = reader.read();
                println!("{:04X}: Move {}b", inst_idx, size * 8);
            }
            MoveImm | MoveImmGlobal => {
                let size: Size = reader.read();
                let addr: Addr = reader.read();
                println!("{:04X}: {:?} [{}] {}b", inst_idx, inst, addr, size * 8);
            }
            Dup => {
                let size: Size = reader.read();
                let addr: Addr = reader.read();
                println!("{:04X}: Dup [{}] {}b", inst_idx, addr, size * 8);
            }
            DupGlobal => {
                let size: Size = reader.read();
                let addr: Addr = reader.read();
                println!("{:04X}: DupGlobal [{}] {}b", inst_idx, addr, size * 8);
            }
            PushPtr => {
                let size: Size = reader.read();
                let ptr: Pointer = reader.read();
                println!("{:04X}: PushPtr {}b {:?}", inst_idx, size * 8, ptr);
            }
            PushPtrGlobal => {
                let size: Size = reader.read();
                let ptr: Pointer = reader.read();
                println!("{:04X}: PushPtrGlobal {}b {:?}", inst_idx, size * 8, ptr);
            }
            Pop => {
                let size: Size = reader.read();
                println!("{:04X}: Pop {}b", inst_idx, size * 8);
            }
            Flush => {
                let addr: Addr = reader.read();
                println!("{:04X}: Flush => [{}]", inst_idx, addr);
            }
            Alloc | AllocZ => {
                let size: Size = reader.read();
                println!("{:04X}: {:?} {}b", inst_idx, inst, size * 8);
            }

            // Branching
            Jump => {
                let jump: Addr = reader.read();
                println!(
                    "{:04X}: Jump => {:04X}",
                    inst_idx,
                    reader.offset() + jump as usize
                );
            }
            JumpBack => {
                let jump: Addr = reader.read();
                println!(
                    "{:04X}: Loop => {:04X}",
                    inst_idx,
                    reader.offset() - jump as usize
                );
            }
            JumpTrue => {
                let jump: Addr = reader.read();
                println!(
                    "{:04X}: JumpTrue => {:04X}",
                    inst_idx,
                    reader.offset() + jump as usize
                );
            }
            JumpFalse => {
                let jump: Addr = reader.read();
                println!(
                    "{:04X}: JumpFalse => {:04X}",
                    inst_idx,
                    reader.offset() + jump as usize
                );
            }
            JumpTrueNoPop => {
                let jump: Addr = reader.read();
                println!(
                    "{:04X}: JumpTrueNoPop => {:04X}",
                    inst_idx,
                    reader.offset() + jump as usize
                );
            }
            JumpFalseNoPop => {
                let jump: Addr = reader.read();
                println!(
                    "{:04X}: JumpFalseNoPop => {:04X}",
                    inst_idx,
                    reader.offset() + jump as usize
                );
            }

            // Invocation
            Call => {
                let size: Size = reader.read();
                println!("{:04X}: Call {}b", inst_idx, size * 8);
            }
            Call_0 => {
                println!("{:04X}: {:?}", inst_idx, inst);
            }
            CallBuiltin => {
                let size: Size = reader.read();
                let builtin: Builtin = reader.read();
                println!(
                    "{:04X}: CallBuiltin {}b {:?}",
                    inst_idx,
                    size * 8,
                    builtin as *const ()
                );
            }

            Ret => {
                let size: Size = reader.read();
                println!("{:04X}: Ret {}b", inst_idx, size * 8);
            }
            Ret_0 => println!("{:04X}: {:?}", inst_idx, inst),
        }
    }

    let len = reader.offset();
    println!("{:04X}: END", len);
}
