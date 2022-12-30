use std::fmt::Debug;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{interp::Interpreter, parsing::tokenization::CodeLocation};

pub type Result<T> = miette::Result<T>;

#[derive(Error, Debug, Diagnostic)]
#[error("{}", err)]
pub struct SourceError {
    #[source_code]
    src: NamedSource,
    err: String,
    #[label("{}", label)]
    span: SourceSpan,
    label: String,
}

#[derive(Error, Debug, Diagnostic)]
#[error("{}", err)]
pub struct SourceError2 {
    #[source_code]
    src: NamedSource,
    err: String,
    #[label("{}", label1)]
    span1: SourceSpan,
    label1: String,
    #[label("{}", label2)]
    span2: SourceSpan,
    label2: String,
}

macro_rules! error {
    ($($ts:tt)*) => {{
        miette::miette!($($ts)*)
    }};
}

pub(crate) use error;

impl SourceError {
    pub fn new(err: impl Into<String>, loc: CodeLocation, label: impl Into<String>) -> Self {
        let interp = Interpreter::get();
        let file = &interp.loaded_files[loc.loaded_file_idx];
        let src = NamedSource::new(file.filepath.to_str().expect("[INTERNAL ERR] Loaded file's filepath not valid UTF-8"), file.source.clone());

        Self {
            src,
            err: err.into(),
            span: loc.span,
            label: label.into(),
        }
    }
}

impl SourceError2 {
    pub fn new(err: impl Into<String>, loc1: CodeLocation, label1: impl Into<String>, loc2: CodeLocation, label2: impl Into<String>) -> Self {
        let interp = Interpreter::get();
        let file = &interp.loaded_files[loc1.loaded_file_idx];
        let src = NamedSource::new(file.filepath.to_str().expect("[INTERNAL ERR] Loaded file's filepath not valid UTF-8"), file.source.clone());

        Self {
            src,
            err: err.into(),
            span1: loc1.span,
            label1: label1.into(),
            span2: loc2.span,
            label2: label2.into(),
        }
    }
}

