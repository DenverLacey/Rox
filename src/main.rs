use debug_print::debug_println as dprintln;
use std::path::Path;

fn main() -> Result<(), &'static str> {
    let mut args = std::env::args().skip(1);
    let project_path = args
        .next()
        .ok_or("Error: Please provide a path to a project.")?;
    dprintln!("project_path = '{}'", project_path);

    let project_path = Path::new(&project_path);
    if !project_path.exists() {
        return Err("Given path does not exist.");
    } else if project_path.is_dir() {
        dprintln!("Path is a directory.");
    } else if project_path.is_file() {
        dprintln!("Path is a file.");
        let source = std::fs::read_to_string(project_path).map_err(|_| "Failed to read file.")?;
        let chars = source.chars();
        for c in chars {
            dprintln!("{}", c);
        }
    } else {
        return Err("Given path is neither a file or a directory.");
    }

    Ok(())
}
