use debug_print::debug_println as dprintln;
use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

mod parsing;
use parsing::tokenization;

use crate::parsing::tokenization::tokenize_file;

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

        let mut loaded_files = vec![];

        let dir = std::fs::read_dir(project_path).map_err(|_| "Failed to read directory.")?;
        for dir_entry in dir {
            let path = dir_entry
                .map_err(|_| "Failed to walk project directory.")?
                .path();
            if path.extension() != Some(OsStr::new("rox")) {
                continue;
            }

            dprintln!("Loading `{:?}`", path);
            let loaded_file = load_file(path)?;
            loaded_files.push(loaded_file);
        }

        dprintln!("{:#?}", loaded_files);
    } else if project_path.is_file() {
        dprintln!("Path is a file.");
        let loaded_file = load_file(project_path)?;
        dprintln!("{:#?}", loaded_file);

        let tokens = tokenize_file(&loaded_file);
        dprintln!("{:#?}", tokens);
    } else {
        return Err("Given path is neither a file or a directory.");
    }

    Ok(())
}

fn load_file<P: AsRef<Path>>(path: P) -> Result<LoadedFile, &'static str> {
    let path = path.as_ref();

    let source = std::fs::read_to_string(path).map_err(|_| "Failed to read file.")?;
    Ok(LoadedFile {
        filepath: path
            .canonicalize()
            .map_err(|_| "Failed to canonicalize project path.")?,
        source,
    })
}

#[derive(Debug)]
struct LoadedFile {
    filepath: PathBuf,
    source: String,
}

#[derive(Debug)]
struct ParsedFile {
    filepath: PathBuf,
    ast: (),
}
