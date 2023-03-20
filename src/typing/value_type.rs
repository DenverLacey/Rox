use std::{fmt::Display, ops::Deref};

use crate::{interp::Interpreter, runtime::vm::Size};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Type {
    pub mutable: bool,
    pub kind: TypeKind,
}

impl Type {
    pub fn of(kind: TypeKind) -> Self {
        Self {
            mutable: false,
            kind,
        }
    }

    pub fn assignable_to(&self, target: Type) -> bool {
        match self.kind {
            TypeKind::Bool => target.kind == TypeKind::Bool,
            TypeKind::Char => target.kind == TypeKind::Char,
            TypeKind::Int => target.kind == TypeKind::Int,
            TypeKind::Float => target.kind == TypeKind::Float,
            TypeKind::String => target.kind == TypeKind::String,
            TypeKind::Range => target.kind == TypeKind::Range,
            TypeKind::Type => target.kind == TypeKind::Type,
            TypeKind::Composite(self_idx) => {
                let interp = Interpreter::get();
                let TypeKind::Composite(target_idx) = target.kind else {
                    return false;
                };

                let self_type = &interp.types[self_idx];
                let target_type = &interp.types[target_idx];

                match self_type {
                    TypeInfo::Pointer(self_info) => {
                        let TypeInfo::Pointer(target_info) = target_type else {
                            return false;
                        };

                        if target_info.pointee_type.mutable && !self_info.pointee_type.mutable {
                            return false;
                        }

                        self_info
                            .pointee_type
                            .assignable_to(target_info.pointee_type)
                    }
                    TypeInfo::Array(self_info) => {
                        // TODO: Handle array to slice conversion or something
                        let TypeInfo::Array(target_info) = target_type else {
                            return false;
                        };

                        if self_info.size != target_info.size {
                            return false;
                        }

                        // NOTE: Since this is just handling array types, and that arrays are value
                        // types means we don't need to check mutability compatibility
                        self_info
                            .element_type
                            .assignable_to(target_info.element_type)
                    }
                    TypeInfo::Slice(self_info) => {
                        let TypeInfo::Slice(target_info) = target_type else {
                            return false;
                        };

                        if (target.mutable && !self.mutable) || (!target.mutable && self.mutable) {
                            return false;
                        }

                        !target_info
                            .element_type
                            .assignable_to(self_info.element_type)
                    }
                    TypeInfo::Function(self_info) => {
                        let TypeInfo::Function(target_info) = target_type else {
                            return false;
                        };

                        todo!()
                    }
                    TypeInfo::Struct(_) => self_idx == target_idx,
                    TypeInfo::Enum(_) => self_idx == target_idx,
                }
            }
        }
    }
}

impl Deref for Type {
    type Target = TypeKind;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.mutable {
            write!(f, "mut ")?;
        }

        Display::fmt(&self.kind, f)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Bool,
    Char,
    Int,
    Float,
    String,
    Range,
    Type,
    Composite(usize),
}

macro_rules! type_is_composite_type {
    ($ty:tt, $($kind:ident),*) => {{
        let mut result = false;
        if let Self::Composite(idx) = *$ty {
            let interp = Interpreter::get();
            let typ = &interp.types[idx];
            result = matches!(typ, $(TypeInfo::$kind(_))|*);
        }

        result
    }};
}

impl TypeKind {
    pub fn size(&self) -> Size {
        let size: usize = match self {
            Self::Bool => std::mem::size_of::<runtime_type::Bool>(),
            Self::Char => std::mem::size_of::<runtime_type::Char>(),
            Self::Int => std::mem::size_of::<runtime_type::Int>(),
            Self::Float => std::mem::size_of::<runtime_type::Float>(),
            Self::String => std::mem::size_of::<runtime_type::String>(),
            Self::Range => std::mem::size_of::<runtime_type::Int>() * 2,
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
                    TypeInfo::Slice(_) => std::mem::size_of::<runtime_type::String>(),
                    TypeInfo::Struct(info) => info
                        .fields
                        .iter()
                        .map(|field| field.typ.size() as usize)
                        .sum(),
                    TypeInfo::Enum(_) => std::mem::size_of::<runtime_type::Int>(),
                    TypeInfo::Function(_) => std::mem::size_of::<*const ()>(),
                }
            }
        };

        size.try_into()
            .expect("[INTERNAL ERR] type size is too big to fit in a `Size`.")
    }

    pub fn is_pointer(&self) -> bool {
        type_is_composite_type!(self, Pointer)
    }

    pub fn is_array(&self) -> bool {
        type_is_composite_type!(self, Array)
    }

    pub fn is_slice(&self) -> bool {
        type_is_composite_type!(self, Slice)
    }

    pub fn is_array_like(&self) -> bool {
        type_is_composite_type!(self, Array, Slice)
    }

    pub fn is_struct(&self) -> bool {
        type_is_composite_type!(self, Struct)
    }

    pub fn is_enum(&self) -> bool {
        type_is_composite_type!(self, Enum)
    }

    pub fn is_function(&self) -> bool {
        type_is_composite_type!(self, Function)
    }
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Bool
            | Self::Char
            | Self::Int
            | Self::Float
            | Self::String
            | Self::Range
            | Self::Type => {
                write!(f, "{:?}", self)
            }
            Self::Composite(idx) => {
                let interp = Interpreter::get();
                let typ = &interp.types[idx];

                match typ {
                    TypeInfo::Pointer(info) => {
                        write!(f, "&{}", info.pointee_type)
                    }
                    TypeInfo::Array(info) => {
                        write!(f, "[{}]{}", info.size, info.element_type)
                    }
                    TypeInfo::Slice(info) => {
                        write!(f, "[]{}", info.element_type)
                    }
                    TypeInfo::Struct(info) => {
                        write!(f, "{}", info.name)
                    }
                    TypeInfo::Enum(info) => {
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
    Slice(TypeInfoSlice),
    Struct(TypeInfoStruct),
    Enum(TypeInfoEnum),
    Function(TypeInfoFunction),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeInfoPointer {
    pub pointee_type: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeInfoArray {
    pub size: usize,
    pub element_type: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeInfoSlice {
    pub element_type: Type,
}

#[derive(Clone, Debug)]
pub struct TypeInfoStruct {
    pub name: String,
    pub fields: Box<[StructField]>,
}

impl TypeInfoStruct {
    pub fn offset_of(&self, field: &str) -> Size {
        self.fields
            .iter()
            .take_while(|f| f.name != field)
            .fold(0, |acc, f| acc + f.typ.size())
    }
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub name: String,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct TypeInfoEnum {
    pub name: String,
    pub variants: Box<[EnumVariant]>,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub name: String,
    pub value: runtime_type::Int,
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

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct String {
        pub len: Int,
        pub chars: *const u8,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct Range {
        pub start: Int,
        pub end: Int,
    }
}
