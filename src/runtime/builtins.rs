use std::io::Write;

use crate::typing::value_type::runtime_type::{Int, self, Bool, Char, Float};

use super::vm::{Size, Stack};

pub type Builtin = fn(&mut Stack, arg_size: Size);

#[allow(non_snake_case)]
pub fn XXXprint_Bool(stack: &mut Stack, arg_size: Size) {
    let data = stack.pop(arg_size);
    let value = unsafe { *(data.as_ptr() as *const Bool) };

    let mut stdout = std::io::stdout();
    writeln!(stdout, "{}", value).expect("Failed to write to stdout.");
}

#[allow(non_snake_case)]
pub fn XXXprint_Char(stack: &mut Stack, arg_size: Size) {
    let data = stack.pop(arg_size);
    let value = unsafe { *(data.as_ptr() as *const Char) };

    let mut stdout = std::io::stdout();
    writeln!(stdout, "{}", value).expect("Failed to write to stdout.");
}

#[allow(non_snake_case)]
pub fn XXXprint_Int(stack: &mut Stack, arg_size: Size) {
    let data = stack.pop(arg_size);
    let value = unsafe { *(data.as_ptr() as *const Int) };

    let mut stdout = std::io::stdout();
    writeln!(stdout, "{}", value).expect("Failed to write to stdout.");
}

#[allow(non_snake_case)]
pub fn XXXprint_Float(stack: &mut Stack, arg_size: Size) {
    let data = stack.pop(arg_size);
    let value = unsafe { *(data.as_ptr() as *const Float) };

    let mut stdout = std::io::stdout();
    writeln!(stdout, "{}", value).expect("Failed to write to stdout.");
}

#[allow(non_snake_case)]
pub fn XXXprint_String(stack: &mut Stack, arg_size: Size) {
    let data = stack.pop(arg_size);
    let value = unsafe { *(data.as_ptr() as *const runtime_type::String) };
    let value = unsafe { std::slice::from_raw_parts(value.chars, value.len as usize) };
    let value = unsafe { std::str::from_utf8_unchecked(value) };

    let mut stdout = std::io::stdout();
    writeln!(stdout, "{}", value).expect("Failed to write to stdout.");
}

