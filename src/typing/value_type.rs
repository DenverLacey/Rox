use std::fmt::Display;

use crate::{interp::Interpreter, runtime::vm::Size};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Char,
    Int,
    Float,
    String,
    Type,
    Composite(usize),
}

impl Type {
    pub fn size(&self) -> Size {
        let size: usize = match self {
            Self::Bool => std::mem::size_of::<runtime_type::Bool>(),
            Self::Char => std::mem::size_of::<runtime_type::Char>(),
            Self::Int => std::mem::size_of::<runtime_type::Int>(),
            Self::Float => std::mem::size_of::<runtime_type::Float>(),
            Self::String => std::mem::size_of::<runtime_type::String>(),
            Self::Type => std::mem::size_of::<*const ()>(), // @TEMP
            Self::Composite(idx) => {
                let interp = Interpreter::get();
                let typ = &interp.types[*idx];

                match typ {
                    TypeInfo::Pointer(_) => std::mem::size_of::<*const ()>(),
                    TypeInfo::Array(info) => {
                        let count = info.size;
                        let element_size = info.element_type.size() as usize;
                        count * element_size
                    }
                    TypeInfo::Struct(info) => info
                        .fields
                        .iter()
                        .map(|field| field.typ.size() as usize)
                        .sum(),
                    TypeInfo::Function(_) => std::mem::size_of::<*const ()>(),
                }
            }
        };

        size.try_into()
            .expect("[INTERNAL ERR] type size is too big to fit in a `Size`.")
    }

    pub fn is_pointer(&self) -> bool {
        if let Self::Composite(idx) = *self {
            let interp = Interpreter::get();
            let typ = &interp.types[idx];

            if matches!(typ, TypeInfo::Pointer(_)) {
                return true;
            }
        }

        false
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Bool | Self::Char | Self::Int | Self::Float | Self::String | Self::Type => {
                write!(f, "{:?}", self)
            }
            Self::Composite(idx) => {
                let interp = Interpreter::get();
                let typ = &interp.types[idx];
                match typ {
                    TypeInfo::Pointer(info) => {
                        let mut_str = if info.mutable_pointee { "mut " } else { "" };
                        write!(f, "&{}{}", mut_str, info.pointee_type)
                    }
                    TypeInfo::Array(info) => {
                        write!(f, "[{}]{}", info.size, info.element_type)
                    }
                    TypeInfo::Struct(info) => {
                        write!(f, "{}", info.name)
                    }
                    TypeInfo::Function(info) => {
                        write!(f, "fn(")?;

                        for (i, param) in info.params.iter().enumerate() {
                            write!(f, "{}", param)?;
                            if i + 1 < info.params.len() {
                                write!(f, ", ")?;
                            }
                        }

                        if let Some(return_type) = info.returns {
                            write!(f, "): {}", return_type)
                        } else {
                            write!(f, ")")
                        }
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeInfo {
    Pointer(TypeInfoPointer),
    Array(TypeInfoArray),
    Struct(TypeInfoStruct),
    Function(TypeInfoFunction),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeInfoPointer {
    pub mutable_pointee: bool,
    pub pointee_type: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeInfoArray {
    pub size: usize,
    pub element_type: Type,
}

#[derive(Clone, Debug)]
pub struct TypeInfoStruct {
    pub name: String,
    pub fields: Box<[StructField]>,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub name: String,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct TypeInfoFunction {
    pub params: Box<[Type]>,
    pub returns: Option<Type>, // @TODO: Multiple returns
}

pub mod runtime_type {
    pub type Bool = bool;
    pub type Char = char;
    pub type Int = i64;
    pub type Float = f64;
    pub type Pointer = *const ();

    #[derive(Clone, Copy)]
    pub struct String {
        pub len: Int,
        pub chars: *const u8,
    }
}
