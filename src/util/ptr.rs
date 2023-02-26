pub trait AdvancePtr {
    fn advance_by(&mut self, nbytes: usize);
}

impl<T> AdvancePtr for *const T {
    fn advance_by(&mut self, nbytes: usize) {
        unsafe {
            let byte_ptr = (*self) as *const u8;
            let advanced_ptr = byte_ptr.add(nbytes);
            *self = advanced_ptr as *const T;
        }
    }
}
