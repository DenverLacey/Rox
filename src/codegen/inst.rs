
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum Instruction {
    NoOp = 0,
    Lit_True,
    Lit_False,
    Lit_0,
    Lit_1,
    Lit_0b,
    Lit_1b,
    Lit_Char,
    Lit_Int,
    Lit_Byte,
    Lit_Float,
    Lit_Pointer,
}
