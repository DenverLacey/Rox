#![allow(non_snake_case)]

use std::io::{Stdout, Write};

use crate::{
    typing::value_type::{
        runtime_type::{self, Bool, Char, Float, Int, Pointer},
        Type, TypeInfoEnum, TypeInfoStruct, TypeKind,
    },
    util::advance_ptr,
};

use super::vm::{Size, Stack};

pub type Builtin = fn(&mut Stack, arg_size: Size);

pub fn XXXprint_Bool(stack: &mut Stack, _: Size) {
    let value: Bool = stack.pop_value();
    let mut stdout = std::io::stdout();
    print_bool(&mut stdout, value);
    writeln!(stdout).expect("Failed to write to stdout.");
}

pub fn XXXprint_Char(stack: &mut Stack, _: Size) {
    let value: Char = stack.pop_value();
    let mut stdout = std::io::stdout();
    print_char(&mut stdout, value);
    writeln!(stdout).expect("Failed to write to stdout.");
}

pub fn XXXprint_Int(stack: &mut Stack, _: Size) {
    let value: Int = stack.pop_value();
    let mut stdout = std::io::stdout();
    print_int(&mut stdout, value);
    writeln!(stdout).expect("Failed to write to stdout.");
}

pub fn XXXprint_Float(stack: &mut Stack, _: Size) {
    let value: Float = stack.pop_value();
    let mut stdout = std::io::stdout();
    print_float(&mut stdout, value);
    writeln!(stdout).expect("Failed to write to stdout.");
}

pub fn XXXprint_String(stack: &mut Stack, arg_size: Size) {
    let data = stack.pop(arg_size);
    let value = unsafe { *(data.as_ptr() as *const runtime_type::String) };
    let mut stdout = std::io::stdout();
    print_string(&mut stdout, value);
    writeln!(stdout).expect("Failed to write to stdout.");
}

pub fn XXXprint_Pointer(stack: &mut Stack, _: Size) {
    let value: Pointer = stack.pop_value();
    let mut stdout = std::io::stdout();
    print_pointer(&mut stdout, value);
    writeln!(stdout).expect("Failed to write to stdout.");
}

pub fn XXXprint_enum(stack: &mut Stack, _: Size) {
    let type_info: &TypeInfoEnum = stack.pop_value();
    let value: Int = stack.pop_value();

    let value_name = type_info
        .variants
        .iter()
        .find(|var| var.value == value)
        .map(|var| &var.name);

    let mut stdout = std::io::stdout();
    if let Some(value_name) = value_name {
        writeln!(stdout, "{}", value_name).expect("Failed to write to stdout.");
    } else {
        writeln!(stdout, "<{}: {}>", type_info.name, value).expect("Failed to write to stdout.");
    }
}

pub fn XXXprint_struct(stack: &mut Stack, size: Size) {
    let type_info: &TypeInfoStruct = stack.pop_value();
    let bytes = stack.pop(size - TypeKind::Int.size());
    let mut data = &bytes[0] as *const u8 as *const ();

    let mut stdout = std::io::stdout();
    write!(stdout, "{}(", type_info.name).expect("Failed to write to stdout.");

    if let Some(first) = type_info.fields.first() {
        write!(stdout, "{} = ", first.name).expect("Failed to write to stdout.");

        let field_size = first.typ.size() as usize;
        let first_data = data;
        advance_ptr(&mut data, field_size);

        print_value(&mut stdout, first_data, first.typ);
    }

    let remaining = type_info.fields.iter().skip(1);
    for field in remaining {
        write!(stdout, ", {} = ", field.name).expect("Failed to write to stdout.");

        let field_size = field.typ.size() as usize;
        let field_data = data;
        advance_ptr(&mut data, field_size);

        print_value(&mut stdout, field_data, field.typ);
    }

    writeln!(stdout, ")").expect("Failed to write to stdout.");
}

fn print_value(stdout: &mut Stdout, ptr: *const (), typ: Type) {
    match typ.kind {
        TypeKind::Bool => {
            let value = unsafe { *(ptr as *const Bool) };
            print_bool(stdout, value);
        }
        TypeKind::Char => {
            let value = unsafe { *(ptr as *const Char) };
            print_char(stdout, value);
        }
        TypeKind::Int => {
            let value = unsafe { *(ptr as *const Int) };
            print_int(stdout, value);
        }
        TypeKind::Float => {
            let value = unsafe { *(ptr as *const Float) };
            print_float(stdout, value);
        }
        TypeKind::String => {
            let value = unsafe { *(ptr as *const runtime_type::String) };
            print_string(stdout, value);
        }
        TypeKind::Type => todo!(),
        TypeKind::Composite(idx) => todo!(),
    }
}

fn print_bool(stdout: &mut Stdout, value: Bool) {
    write!(stdout, "{}", value).expect("Failed to write to stdout.");
}

fn print_char(stdout: &mut Stdout, value: Char) {
    write!(stdout, "{}", value).expect("Failed to write to stdout.");
}

fn print_int(stdout: &mut Stdout, value: Int) {
    write!(stdout, "{}", value).expect("Failed to write to stdout.");
}

fn print_float(stdout: &mut Stdout, value: Float) {
    write!(stdout, "{}", value).expect("Failed to write to stdout.");
}

fn print_string(stdout: &mut Stdout, value: runtime_type::String) {
    let value = unsafe { std::slice::from_raw_parts(value.chars, value.len as usize) };
    let value = unsafe { std::str::from_utf8_unchecked(value) };
    write!(stdout, "{}", value).expect("Failed to write to stdout.");
}

fn print_pointer(stdout: &mut Stdout, value: Pointer) {
    write!(stdout, "{:?}", value).expect("Failed to write to stdout.");
}
