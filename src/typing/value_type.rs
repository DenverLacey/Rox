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
            Self::Bool => 1,
            Self::Char => 4,
            Self::Int => 8,
            Self::Float => 8,
            Self::String => 16,
            Self::Composite(_idx) => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompositeType {
    Array(CompositeTypeInfoArray),
    Record(CompositeTypeInfoRecord),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CompositeTypeInfoArray {
    pub size: usize,
    pub element_type: Type,
}

#[derive(Clone, Debug)]
pub struct CompositeTypeInfoRecord {
    pub name: String,
    pub fields: Box<[RecordField]>,
}

#[derive(Clone, Debug)]
pub struct RecordField {
    pub name: String,
    pub typ: Type,
}
