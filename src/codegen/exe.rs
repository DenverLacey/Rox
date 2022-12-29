use crate::interp::FunctionInfo;

pub struct Executable {
    pub constants: Box<[u8]>,
    pub str_constants: Box<[u8]>,
    pub funcs: Box<[FunctionInfo]>,
}

pub struct ExecutableBuilder {
    constants: Vec<u8>,
    slice_constants: Vec<u8>,
    funcs: Vec<FunctionInfo>,
}

impl ExecutableBuilder {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            slice_constants: Vec::new(),
            funcs: Vec::new(),
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

    pub fn add_slice_constant(&mut self, slice: &[u8]) -> usize {
        if let Some(existing) = self.find_existing_slice(slice) {
            return existing;
        }

        let idx = self.slice_constants.len();

        for shift in 0..std::mem::size_of_val(&slice.len()) {
            let len = slice.len();
            let byte = (0xFF & (len >> shift)) as u8;
            self.slice_constants.push(byte);
        }

        for byte in slice {
            self.slice_constants.push(*byte);
        }

        idx
    }

    pub fn add_func(&mut self, f: FunctionInfo) {
        self.funcs.push(f);
    }

    pub fn build(self) -> Executable {
        let constants = self.constants.into_boxed_slice();
        let str_constants = self.slice_constants.into_boxed_slice();
        let funcs = self.funcs.into_boxed_slice();

        Executable {
            constants,
            str_constants,
            funcs,
        }
    }
}

impl ExecutableBuilder {
    fn find_existing_slice(&self, slice: &[u8]) -> Option<usize> {
        let mut i = 0;
        while i < self.slice_constants.len() {
            let Some(len_bytes) = self.slice_constants.get(i..(i + std::mem::size_of::<usize>())) else {
                break;
            };

            let idx = i;
            let len = Self::extract_usize(len_bytes);
            i += len_bytes.len();

            let bytes = self
                .slice_constants
                .get(i..(i + len))
                .expect("[INTERNAL ERR] Bad length.");
            if slice.iter().eq(bytes.iter()) {
                return Some(idx);
            }

            i += len;
        }

        None
    }

    fn extract_usize(bytes: &[u8]) -> usize {
        assert!(bytes.len() == 8);

        let mut result = 0;

        for i in 0..std::mem::size_of::<usize>() {
            let byte = bytes[i];
            let shifted = (byte as usize) << i;
            result &= shifted;
        }

        result
    }
}
