/*! Generating SPIR-V from Naga IR, based on backend IR lowering.

This module defines [`write_vec`], a function which generates SPIR-V
from a [`naga::ir::Module`] and its [`ModuleInfo`], by first lowering
it to a [`naga::back::ir::Module`], and then generating SPIR-V from
that.

[`naga::ir::Module`]: crate::ir::Module
[`ModuleInfo`]: crate::valid::ModuleInfo
*/

#![allow(unused)]

mod builder;
mod instruction;
mod r#type;
mod function;

use builder::Builder;
use crate::back;
use crate::arena::{Handle, HandleVec};
use spirv::Word;

use thiserror::Error;

use alloc::string::String;
use alloc::vec::Vec;

struct Context<'m> {
    /// The backend module we're generating SPIR-V for.
    module: &'m back::ir::Module,

    /// SPIR-V generation options.
    options: &'m Options,

    /// Mapping from backend IR types to SPIR-V `OpType` instructions.
    ir_types: HandleVec<back::ir::TypeInner, Word>,

    /// Mapping from backend global variables to SPIR-V `OpVariable` instructions.
    ir_globals: HandleVec<back::ir::GlobalVariable, Word>,

    /// Mapping from backend functions to SPIR-V `OpFunction` instructions.
    ir_functions: HandleVec<back::ir::Function, Word>,

    /// Id of the "GLSL.std.450" extended set of instructions.
    ///
    /// This is so commonly used that we just always request it.
    ext_gl450: Word,

    /// Id of the "NonSemantic.DebugPrintf" extended instructions, if used.
    ext_debug_printf: Option<Word>,
}

#[derive(Debug, Clone)]
pub struct Options {
    /// (Major, Minor) target version of the SPIR-V.
    pub lang_version: (u8, u8),
}

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error("module uses {0}, which requires {1}, but this device only supports {2}")]
    VersionTooLow(&'static str, &'static str, String),
}

impl<'m> Context<'m> {
    fn new<'b>(
        module: &'m back::ir::Module,
        options: &'m Options,
        builder: &'b mut Builder
    ) -> Self {
        // Request the GL functions; we'll always use them.
        let ext_gl450 = builder.ext_inst_import("GLSL.std.450");

        Context {
            module,
            options,
            ir_types: HandleVec::with_capacity(module.types.len()),
            ir_globals: HandleVec::with_capacity(module.globals.len()),
            ir_functions: HandleVec::with_capacity(module.functions.len()),
            ext_gl450,
            ext_debug_printf: None,
        }
    }

    fn type_id(&self, ty: Handle<back::ir::Type>) -> Word {
        self.ir_types[self.module.types[ty].inner]
    }
}

pub fn supported_capabilities() -> crate::valid::Capabilities {
    use crate::valid::Capabilities as Caps;
    Caps::empty()
}

pub fn write_vec(module: &crate::Module,
                 info: &crate::valid::ModuleInfo,
                 options: &Options,
) -> Vec<u32> {
    // Lower the module to backend IR as necessary for SPIR-V.
    let lowered = back::ir::lower(
        module,
        info,
        back::ir::option::Options {
            types: back::ir::option::TypeOptions {
                replace_cx2_matrix_with_struct: true,
                spirv_unique_types: true,
                no_atomic_types: true,
                matrix_orientation: back::ir::MatrixOrientation::ColumnMajor,
            },
            entry_points: back::ir::option::EntryPointOptions {
                io: back::ir::option::ShaderStageIoStyle::Globals,
                user_output_masks: Default::default(),
            },
            naming_rules: None,
        }
    );

    // Generate SPIR-V from the lowered IR.
    let mut builder = Builder::new(options.lang_version);
    let mut context = Context::new(&lowered, options, &mut builder);
    context.generate_types(&mut builder);

    // context.generate_globals(&mut builder);

    context.generate_functions(&mut builder);

    builder.build()
}
