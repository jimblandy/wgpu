//! `requires …;` extensions in WGSL.
//!
//! The focal point of this module is the [`LanguageExtension`] API.

define_extensions! {
    /// A language extension recognized by Naga, but not guaranteed to be present in all environments.
    ///
    /// WGSL spec.: <https://www.w3.org/TR/WGSL/#language-extensions-sec>
    #[derive(Default)]
    pub struct LanguageExtensions: u32 {
        const READONLY_AND_READWRITE_STORAGE_TEXTURES, "readonly_and_readwrite_storage_textures" = 0x1;
        const PACKED4X8_INTEGER_DOT_PRODUCT, "packed_4x8_integer_dot_product" = 0x2;
        const UNRESTRICTED_POINTER_PARAMETERS, "unrestricted_pointer_parameters" = 0x4;
        const POINTER_COMPOSITE_ACCESS, "pointer_composite_access" = 0x8;
    }
}

impl LanguageExtensions {
    pub const IMPLEMENTED: Self = Self::empty()
        .union(Self::READONLY_AND_READWRITE_STORAGE_TEXTURES)
        .union(Self::PACKED4X8_INTEGER_DOT_PRODUCT)
        .union(Self::POINTER_COMPOSITE_ACCESS);

    pub const UNIMPLEMENTED: Self = Self::IMPLEMENTED.complement();

    pub(crate) const fn tracking_issue_num(self) -> Option<u16> {
        match self {
            Self::UNRESTRICTED_POINTER_PARAMETERS => Some(5158),
            other => {
                assert!(Self::IMPLEMENTED.contains(other));
                None
            }
        }
    }
}
