use std::hash::Hasher;

use bitflags::bitflags;
use phf::phf_map;

bitflags! {
    #[derive(Default)]
    pub struct Annotations: u32 {
        const FN_ENTRY  = 0x1;
        const FN_INLINE = 0x2;
    }
}

pub static TABLE: phf::Map<&'static str, Annotations> = phf_map! {
    "entry" => Annotations::FN_ENTRY,
    "inline" => Annotations::FN_INLINE,
};

pub static FN_ANNOTATIONS: Annotations = fn_annotations();

const fn fn_annotations() -> Annotations {
    let entry: u32 = unsafe { std::mem::transmute(Annotations::FN_ENTRY) };
    let inline: u32 = unsafe { std::mem::transmute(Annotations::FN_INLINE) };
    let annons = entry | inline;
    unsafe { std::mem::transmute(annons) }
}
