mod canon;
mod codegen;
mod interp;
mod ir;
mod parsing;
mod runtime;
mod typing;
mod util;

use crate::interp::Interpreter;
use crate::util::errors::{error, Result};

fn main() {
    let result: Result<()> = (|| {
        let mut args = std::env::args().skip(1);
        let project_path = args
            .next()
            .ok_or_else(|| error!("Please provide a path to a project."))?;

        let interpreter = Interpreter::get_mut();
        let exe = interpreter.generate_executable(project_path)?;
        interpreter.execute_executable(&exe)?;

        Ok(())
    })();

    if let Err(err) = result {
        eprintln!("{:?}", err);
    }
}
