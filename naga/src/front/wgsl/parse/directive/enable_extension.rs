//! `enable …;` extensions in WGSL.
//!
//! The focal point of this module is the [`EnableExtensions`] bitflags type.

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
