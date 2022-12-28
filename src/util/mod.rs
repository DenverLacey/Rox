pub mod iter;

macro_rules! lformat {
    ($fmt:literal, $($args:expr),*) => {{
        let s = format!($fmt, $($args),*);
        Box::leak(s.into_boxed_str()) as &'static str
    }};
}

pub(crate) use lformat;
