/*! Generating HLSL from Naga IR, based on backend IR lowering.

This module defines [`write`], a function which generates HLSL from a
[`naga::ir::Module`] and its [`ModuleInfo`], by first lowering it to a
[`naga::back::ir::Module`], and then generating HLSL from that.

[`naga::ir::Module`]: crate::ir::Module
[`ModuleInfo`]: crate::valid::ModuleInfo
*/

#![allow(unused)]

pub mod shader_model;

use crate::FastHashMap;
use alloc::string::String;
use alloc::vec::Vec;

/// Options for generating HLSL from Naga IR.
#[derive(Debug)]
pub struct Options {
    /// Generate output acceptable to this shader model.
    ///
    /// If this shader model is not adequate to translate the module,
    /// [`write`] will panic:
    ///
    /// - `wgpu_hal::dx12` should advertise only `Features` that its
    ///   shader model can support.
    ///
    /// - `wgpu-core` should select only Naga `Capabilities` that its
    ///   `Features` can support.
    ///
    /// - Naga validation should reject modules that use unselected
    ///   `Capabilities`.
    ///
    /// So if the Naga module we are generating HLSL for contains
    /// anything that cannot be expressed in this shader model, that
    /// is not this backend's problem.
    pub shader_model: shader_model::ShaderModel,

    /// User-defined output restrictions for selected entry points.
    ///
    /// If this map has an entry for `i`, then the entry point at index `i` in
    /// the Naga IR `Module` should have its user-defined outputs limited to
    /// those whose locations are included in hte given `BitSet`.
    pub user_output_masks: FastHashMap<usize, bit_set::BitSet>,
}

/// The result of generating HLSL from Naga IR.
#[derive(Debug)]
pub struct HLSLOutput {
    /// HLSL source code.
    pub source: String,

    /// The name assigned to each entry point.
    ///
    /// The `i`'th element of this vector is the name we assigned to
    /// the `i`'th entry point in the Naga IR `Module`.
    pub entry_point_names: Vec<String>,
}

/// Generate HLSL for `module` and `info`, according to `options`.
///
/// # Panics
///
/// This function will panic if `module` is not valid.
///
/// This function will panic if `module` cannot be expressed in the
/// HLSL shader model given in [`options.shader_model`][sm]. See the
/// documentation for that field for details.
///
/// [sm]: Options::shader_model
pub fn write(module: &crate::Module,
             info: &crate::valid::ModuleInfo,
             options: &Options,
) -> HLSLOutput {
    todo!()
}
