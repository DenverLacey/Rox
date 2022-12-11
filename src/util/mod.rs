pub mod iter;
pub mod structures;

macro_rules! lformat {
    ($fmt:literal, $($args:expr),*) => {{
        let s = format!($fmt, $($args),*);
        Box::leak(s.into_boxed_str())
    }};
}

pub(crate) use lformat;
