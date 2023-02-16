use bitflags::bitflags;
use phf::phf_map;

bitflags! {
    #[derive(Default)]
    pub struct Annotations: u32 {
        const FN_ENTRY  = 0x1;
        const FN_INLINE = 0x2;
        const STRUCT_UNION = 0x4;
    }
}

pub static TABLE: phf::Map<&'static str, Annotations> = phf_map! {
    "entry" => Annotations::FN_ENTRY,
    "inline" => Annotations::FN_INLINE,
    "union" => Annotations::STRUCT_UNION,
};

pub static FN_ANNOTATIONS: Annotations = fn_annotations();
pub static STRUCT_ANNOTATIONS: Annotations = struct_annotations();
pub static ENUM_ANNOTATIONS: Annotations = enum_annotations();

const fn fn_annotations() -> Annotations {
    let entry: u32 = unsafe { std::mem::transmute(Annotations::FN_ENTRY) };
    let inline: u32 = unsafe { std::mem::transmute(Annotations::FN_INLINE) };
    let annons = 0x0 | entry | inline;
    unsafe { std::mem::transmute(annons) }
}

const fn struct_annotations() -> Annotations {
    let union: u32 = unsafe { std::mem::transmute(Annotations::STRUCT_UNION) };
    let annons = 0x0 | union;
    unsafe { std::mem::transmute(annons) }
}

const fn enum_annotations() -> Annotations {
    let annons = 0x0;
    unsafe { std::mem::transmute(annons) }
}
