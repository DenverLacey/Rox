#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
    NoOp = 0,

    // Literals
    // These operations push a literal value onto the stack.
    Lit_True,    // () [] -> [true]
    Lit_False,   // () [] -> [false]
    Lit_0,       // () [] -> [0]
    Lit_1,       // () [] -> [1]
    Lit_Char,    // (c:char) [] -> [c]
    Lit_Int,     // (k:int) [] -> [k]
    Lit_Float,   // (f:float) [] -> [f]
    Lit_Pointer, // (p:ptr) [] -> [p]

    // Constants
    // These operations push a constant from the constants table or str_constants table
    // respectively.
    PushConst,     // (size:u16, idx:const<C>) [] -> [C]
    PushConst_Str, // (idx:const<S>) [] -> [S]

    // Arithmetic
    Int_Add, // () [m, n] -> [m+n]
    Int_Sub, // () [m, n] -> [m-n]
    Int_Mul, // () [m, n] -> [m*n]
    Int_Div, // () [m, n] -> [m/n]
    Int_Neg, // () [k] -> [-k]
    Int_Mod, // () [m, n] -> [m%n]
    Int_Inc, // () [k] -> [k+1]
    Int_Dec, // () [k] -> [k-1]

    Float_Add, // () [f, g] -> [f+g]
    Float_Sub, // () [f, g] -> [f-g]
    Float_Mul, // () [f, g] -> [f*g]
    Float_Div, // () [f, g] -> [f/g]
    Float_Neg, // () [f] -> [-f]

    // Bitwise
    Bit_Not, // () [k] -> [~k]
    Bit_Shl, // () [m, n] -> [m<<n]
    Bit_Shr, // () [m, n] -> [m>>n]
    Bit_And, // () [m, n] -> [m&n]
    Bit_Or,  // () [m, n] -> [m|n]
    Bit_Xor, // () [m, n] -> [m^n]

    // Logic
    And, // () [m, n] -> [m&&n]
    Or,  // () [m, n] -> [m||n]
    Not, // () [b] -> [!b]

    // Comparison
    Eq,       // (size: u16) [a, b] -> [a==b]
    Ne,       // (size: u16) [a, b] -> [a!=b]
    Str_Eq,   // () [s, r] -> [s==r]
    Str_Ne,   // () [s, r] -> [s!=r]
    Int_Lt,   // () [m, n] -> [m<n]
    Int_Le,   // () [m, n] -> [m<=n]
    Int_Gt,   // () [m, n] -> [m>n]
    Int_Ge,   // () [m, n] -> [m>=n]
    Float_Lt, // () [f, g] -> [f<g]
    Float_Le, // () [f, g] -> [f<=g]
    Float_Gt, // () [f, g] -> [f>g]
    Float_Ge, // () [f, g] -> [f>=g]

    // Stack Operations
    Move,
    // Desc:   Copies the bytes of p to q
    // Schema: (size:u16) [p[size], q[size]] -> []
    Dup,
    // Desc:   Pushes a duplicate of `x` to the top of the stack. `x` is located at `addr` and is
    //         `size` bytes large.
    // Schema: (size:u16, addr:u16) [x, ...] -> [x, ..., x]
    DupGlobal,
    // Desc:   Same as `Dup` but the addr is not offset by the current `CallFrame`.
    // Schema: (size:u16, addr:u16) [x, ...] -> [x, ..., x]
    PushPtr,
    // Desc:   Pushes the value pointed to by `p` onto the stack.
    // Schema: (size:u16, p:u16) [] -> [*p]
    PushPtrGlobal,
    // Desc:   Same as `PushPtr` but `p` is not offset by the current `CallFrame`.
    // Schema: (size:u16, p:u16) [] -> [*p]
    Pop,
    // Desc:   Pops `size` bytes off the stack.
    // Schema: (size:u16) [x] -> []
    Alloc,
    // Desc:   Allocates `size` bytes on the stack and leaves it unitialized (if this area of the
    //         stack has never been used it'll be zero.)
    // Schema: (size:u16) [] -> [B1, B2, ..., Bsize]
    AllocZ,
    // Desc:   Allocates `size` bytes on the stack and zero initializes.
    // Schema: (size:u16) [] -> [0, 0, ..., 0]
    Ret,
    // Desc:   Moves `size` bytes to the beginning of the current `CallFrame` and returns to the
    //         previous `CallFrame` effectively popping off any arguments left on the stack.
    // Schema: (size:u16) [..., x] -> [x]
    Ret_0,
    // Desc:   Returns from a procedure with no return value.
    // Schema: () [...] -> []

    // Branching
    Jump,
    // Desc:   Jumps the instruction pointer `k` bytes forwards.
    // Schema: (k:u16) [] -> []
    JumpBack,
    // Desc:   Jumps the instruction pointer `k` bytes backwards.
    // Scehma: (k:u16) [] -> []
    JumpTrue,
    // Desc:   Same as `Jump` but only if condition `b` is true. `b` is popped off the stack.
    // Schema: (k:u16) [b] -> []
    JumpFalse,
    // Desc:   Same as `JumpTrue` but `b` must be false.
    // Schema: (k:u16) [b] -> []
    JumpTrueNoPop,
    // Desc:   Same as `JumpTrue` but `b` isn't popped off the stack.
    // Schema: (k:u16) [b] -> [b]
    JumpFalseNoPop,
    // Desc:   Same as `JumpFalse` but `b` isn't popped off the stack.
    // Schema: (k:u16) [b] -> [b]

    // Invocation
    Call,
    // Desc:   Calls function pointed at by `f` and sets up a new `CallFrame` with `size` bytes as
    //         the parameters to the function.
    // Schema: (size:u16) [args..., f] -> [args...]
    CallBuiltin,
    // Desc:   Calls builtin function `f`.
    // Schema: (size:u16, f:Builtin) [args...] -> [return_value?]
}
