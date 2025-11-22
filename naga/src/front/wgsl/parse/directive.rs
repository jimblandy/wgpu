//! WGSL directives. The focal point of this API is [`DirectiveKind`].
//!
//! See also <https://www.w3.org/TR/WGSL/#directives>.

use alloc::boxed::Box;

/// A parsed sentinel word indicating the type of directive to be parsed next.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(test, derive(strum::EnumIter))]
pub(crate) enum DirectiveKind {
    /// A [`crate::diagnostic_filter`].
    Diagnostic,
    /// An [`enable_extension`].
    Enable,
    /// A [`language_extension`].
    Requires,
}

impl DirectiveKind {
    const DIAGNOSTIC: &'static str = "diagnostic";
    const ENABLE: &'static str = "enable";
    const REQUIRES: &'static str = "requires";

    /// Convert from a sentinel word in WGSL into its associated [`DirectiveKind`], if possible.
    pub fn from_ident(s: &str) -> Option<Self> {
        Some(match s {
            Self::DIAGNOSTIC => Self::Diagnostic,
            Self::ENABLE => Self::Enable,
            Self::REQUIRES => Self::Requires,
            _ => return None,
        })
    }
}

impl crate::diagnostic_filter::Severity {
    #[cfg(feature = "wgsl-in")]
    pub(crate) fn report_wgsl_parse_diag<'a>(
        self,
        err: Box<crate::front::wgsl::error::Error<'a>>,
        source: &str,
    ) -> crate::front::wgsl::Result<'a, ()> {
        self.report_diag(err, |e, level| {
            let e = e.as_parse_error(source);
            log::log!(level, "{}", e.emit_to_string(source));
        })
    }
}

/// Define a bitflags type representing a set of extensions, with their source names.
///
/// This is used to define bitflags types for language and enable
/// extensions.
///
/// An invocation of this macro defines a `bitflags` type that
/// implements `Copy` and `Eq`, with methods `from_ident` and
/// `to_ident` that convert to and from the WGSL source name for the
/// extension.
macro_rules! define_extensions {
    {
        $( #[ $( $meta:meta )* ] )*
        pub struct $typename:ident: $type:ty
        {
            $(
                $( #[ $inner:ident $( $args:tt )* ] )*
                const $name:ident, $wgsl:literal = $value:expr ;
            )*
        }
    } => {
        bitflags::bitflags! {
            $( #[ $( $meta )* ] )*
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub struct $typename: $type {
                $(
                    $( #[ $inner $( $args )* ] )*
                    const $name = $value ;
                )*
            }
        }

        impl $typename {
            pub fn from_ident(wgsl: &str) -> Option<Self> {
                match wgsl {
                    $(
                        $wgsl => Some($typename :: $name),
                    )*
                    _ => None,
                }
            }

            pub fn to_ident(self) -> &'static str {
                match self {
                    $(
                        $typename :: $name => $wgsl,
                    )*
                    _ => unreachable!("should have exactly one extension bit set"),
                }
            }
        }
    }
}

define_extensions! {
    /// All enable extensions known to Naga.
    ///
    /// This includes extensions that Naga does not implement; the [`IMPLEMENTED`]
    /// associated constant indicates which ones we do support.
    ///
    /// [`IMPLEMENTED`]: EnableExtensions::IMPLEMENTED
    #[derive(Default)]
    pub struct EnableExtensions: u32 {
        /// Enables `f16`/`half` primitive support in all shader languages.
        ///
        /// In the WGSL standard, this corresponds to [`enable f16;`].
        ///
        /// [`enable f16;`]: https://www.w3.org/TR/WGSL/#extension-f16
        const F16, "f16" = 0x1;

        /// Enables the `clip_distances` variable in WGSL.
        ///
        /// In the WGSL standard, this corresponds to [`enable clip_distances;`].
        ///
        /// [`enable clip_distances;`]: https://www.w3.org/TR/WGSL/#extension-clip_distances
        const CLIP_DISTANCES, "clip_distances" = 0x2;

        /// Enables the `blend_src` attribute in WGSL.
        ///
        /// In the WGSL standard, this corresponds to [`enable dual_source_blending;`].
        ///
        /// [`enable dual_source_blending;`]: https://www.w3.org/TR/WGSL/#extension-dual_source_blending
        const DUAL_SOURCE_BLENDING, "dual_source_blending" = 0x4;

        /// Enables subgroup built-ins in all languages.
        ///
        /// In the WGSL standard, this corresponds to [`enable subgroups;`].
        ///
        /// [`enable subgroups;`]: https://www.w3.org/TR/WGSL/#extension-subgroups
        const SUBGROUPS, "subgroups" = 0x8;

        /// Enables the `@builtin(primitive_index)` attribute in WGSL.
        ///
        /// In the WGSL standard, this corresponds to [`enable primitive-index;`].
        ///
        /// [`enable primitive-index;`]: https://www.w3.org/TR/WGSL/#extension-primitive_index
        const PRIMITIVE_INDEX, "primitive_index" = 0x10;

        // wgpu extensions

        /// Enables the `wgpu_mesh_shader` extension, native only
        const WGPU_MESH_SHADER, "wgpu_mesh_shader" = 0x100;

        /// Enables the `wgpu_ray_query` extension, native only.
        const WGPU_RAY_QUERY, "wgpu_ray_query" = 0x200;

        /// Enables the `wgpu_ray_query_vertex_return` extension, native only.
        const WGPU_RAY_QUERY_VERTEX_RETURN, "wgpu_ray_query_vertex_return" = 0x400;
    }
}

impl EnableExtensions {
    pub const IMPLEMENTED: Self = Self::empty()
        .union(Self::WGPU_MESH_SHADER)
        .union(Self::WGPU_RAY_QUERY)
        .union(Self::WGPU_RAY_QUERY_VERTEX_RETURN)
        .union(Self::DUAL_SOURCE_BLENDING)
        .union(Self::F16)
        .union(Self::CLIP_DISTANCES);

    pub const UNIMPLEMENTED: Self = Self::IMPLEMENTED.complement();

    pub fn tracking_issue_num(self) -> Option<u16> {
        match self {
            Self::SUBGROUPS => Some(5555),
            Self::PRIMITIVE_INDEX => Some(8236),
            other => {
                assert!(Self::IMPLEMENTED.contains(other));
                None
            }
        }
    }
}

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

#[cfg(test)]
mod test {
    use alloc::format;

    use strum::IntoEnumIterator;

    use super::DirectiveKind;
    use crate::front::wgsl::assert_parse_err;

    #[test]
    fn directive_after_global_decl() {
        for unsupported_shader in DirectiveKind::iter() {
            let directive;
            let expected_msg;
            match unsupported_shader {
                DirectiveKind::Diagnostic => {
                    directive = "diagnostic(off,derivative_uniformity)";
                    expected_msg = "\
error: expected global declaration, but found a global directive
  ┌─ wgsl:2:1
  │
2 │ diagnostic(off,derivative_uniformity);
  │ ^^^^^^^^^^ written after first global declaration
  │
  = note: global directives are only allowed before global declarations; maybe hoist this closer to the top of the shader module?

";
                }
                DirectiveKind::Enable => {
                    directive = "enable f16";
                    expected_msg = "\
error: expected global declaration, but found a global directive
  ┌─ wgsl:2:1
  │
2 │ enable f16;
  │ ^^^^^^ written after first global declaration
  │
  = note: global directives are only allowed before global declarations; maybe hoist this closer to the top of the shader module?

";
                }
                DirectiveKind::Requires => {
                    directive = "requires readonly_and_readwrite_storage_textures";
                    expected_msg = "\
error: expected global declaration, but found a global directive
  ┌─ wgsl:2:1
  │
2 │ requires readonly_and_readwrite_storage_textures;
  │ ^^^^^^^^ written after first global declaration
  │
  = note: global directives are only allowed before global declarations; maybe hoist this closer to the top of the shader module?

";
                }
            }

            let shader = format!(
                "\
@group(0) @binding(0) var<storage> thing: i32;
{directive};
"
            );
            assert_parse_err(&shader, expected_msg);
        }
    }
}
