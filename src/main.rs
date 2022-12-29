mod codegen;
mod canon;
mod interp;
mod ir;
mod parsing;
mod typing;
mod util;

use crate::interp::Interpreter;

fn main() -> Result<(), &'static str> {
    let mut args = std::env::args().skip(1);
    let project_path = args.next().ok_or("Please provide a path to a project.")?;

    let interpreter = Interpreter::get_mut();
    interpreter.generate_program(project_path)?;
    interpreter.execute_program()?;

    Ok(())
}
