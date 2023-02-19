pub mod byte_reader;
pub mod errors;
pub mod iter;

pub fn advance_ptr<T>(p: &mut *const T, n: usize) {
    unsafe {
        let byte_ptr = (*p) as *const u8;
        let advanced_ptr = byte_ptr.add(n);
        *p = advanced_ptr as *const T;
    }
}
