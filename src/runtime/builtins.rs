use std::io::Write;

use crate::typing::value_type::runtime_type::Int;

use super::vm::{Size, Stack};

pub type Builtin = fn(&mut Stack, arg_size: Size);

#[allow(non_snake_case)]
pub fn XXXprint(stack: &mut Stack, arg_size: Size) {
    let data = stack.pop(arg_size);
    let value = unsafe { *(data.as_ptr() as *const Int) };

    let mut stdout = std::io::stdout();
    writeln!(stdout, "{}", value);
}
