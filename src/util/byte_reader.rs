pub struct ByteReader<'a> {
    i: usize,
    bytes: &'a [u8],
}

impl<'a> ByteReader<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Self { i: 0, bytes }
    }

    pub fn read<T: Copy + Sized>(&mut self) -> T {
        let size = std::mem::size_of::<T>();
        let data = self.bytes.get(self.i..(self.i + size)).expect("[INTERNAL ERR] Attempt to read value failed due to not enough bytes in bytecode remaining.");
        let ptr = data.as_ptr() as *const T;
        let value = unsafe { *ptr };

        self.i += size;
        value
    }

    pub fn offset(&self) -> usize {
        self.i
    }

    pub fn bytes(&self) -> &[u8] {
        self.bytes
    }

    pub fn jump(&mut self, jump: usize) {
        self.i += jump;
    }

    pub fn jump_back(&mut self, jump: usize) {
        self.i -= jump;
    }
}
