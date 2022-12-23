use crate::interp::Interpreter;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Char,
    Int,
    Float,
    String,
    Composite(usize),
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Self::Bool => std::mem::size_of::<runtime_type::Bool>(),
            Self::Char => std::mem::size_of::<runtime_type::Char>(),
            Self::Int => std::mem::size_of::<runtime_type::Int>(),
            Self::Float => std::mem::size_of::<runtime_type::Float>(),
            Self::String => std::mem::size_of::<runtime_type::String>(),
            Self::Composite(idx) => {
                let interp = Interpreter::get();
                let typ = &interp.types[*idx];

                match typ {
                    TypeInfo::Array(info) => {
                        let count = info.size;
                        let element_size = info.element_type.size();
                        count * element_size
                    }
                    TypeInfo::Record(info) => {
                        info.fields.iter().map(|field| field.typ.size()).sum()
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeInfo {
    Array(TypeInfoArray),
    Record(TypeInfoRecord),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeInfoArray {
    pub size: usize,
    pub element_type: Type,
}

#[derive(Clone, Debug)]
pub struct TypeInfoRecord {
    pub name: String,
    pub fields: Box<[RecordField]>,
}

#[derive(Clone, Debug)]
pub struct RecordField {
    pub name: String,
    pub typ: Type,
}

pub mod runtime_type {
    pub type Bool = bool;
    pub type Char = char;
    pub type Int = i64;
    pub type Float = f64;

    pub struct String {
        pub len: Int,
        pub chars: *mut u8,
    }
}
