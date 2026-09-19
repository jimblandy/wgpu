/*! Options that control how Naga IR is lowered to backend IR. */

use crate::proc::{CaseInsensitiveKeywordSet, KeywordSet};

use crate::back;
use crate::FastHashMap;

use alloc::vec::Vec;
use alloc::vec;

#[derive(Debug)]
pub struct Options {
    pub types: TypeOptions,
    pub entry_points: EntryPointOptions,

    /// The set of naming rules the output module must respect.
    ///
    /// If present, names in the final lowered module will respect these rules.
    /// See the module documentation for [`back::ir::pass::adjust_names`] for
    /// details.
    ///
    /// For target languages like SPIR-V, in which references to definitions do
    /// not use names to identify their referents, this can be `None`. In this
    /// case, names in the module are simply provided on a best-effort basis, for
    /// diagnostic and debugging purposes. Names may conflict, be invalid
    /// identifiers, or be absent altogether.
    pub naming_rules: Option<NamingRules>,
}

/// Options for lowering types and operations on them.
#[derive(Debug)]
pub struct TypeOptions {
    /// Replace matrices that have two rows of four-byte elements with structs
    /// with a separate member for each vector.
    ///
    /// See the module documentation for [`back::ir::pass::struct_for_matrix`]
    /// for details.
    pub replace_cx2_matrix_with_struct: bool,

    /// Use row-indexed matrix types in the output to represent Naga IR matrix
    /// values.
    ///
    /// See the module documentation for [`back::ir::pass::transpose_matrices`]
    /// for details.
    pub transpose_matrices: bool,

    /// Ensure the module's [`TypeInner`]s are unique as required for SPIR-V.
    ///
    /// SPIR-V §2.8 requires some classes of `OpType...` instructions to be unique;
    /// for example, you can't have two `OpTypeInt 32 1` instructions in the same
    /// module. All 32-bit signed integers must use the same type id.
    ///
    /// When this flag is set, lowering ensures that all main IR types that must
    /// be the same SPIR-V type are lowered to the same backend IR type. For
    /// example, since SPIR-V doesn't distinguish between comparison and
    /// non-comparison samplers, all samplers are lowered to ordinary,
    /// non-comparison samplers.
    pub spirv_unique_types: bool,

    /// Generate atomic operations on ordinary scalar types, not atomic types.
    pub no_atomic_types: bool,
}

/// Options for generating entry points
#[derive(Debug)]
pub struct EntryPointOptions {
    /// How shader stage inputs and outputs (builtin and user-defined)
    /// should be passed to and returned from an entry point.
    pub io: ShaderStageIoStyle,

    /// User-defined output restrictions for selected entry points.
    ///
    /// If this map has an entry for `i`, then the entry point at index `i` in
    /// the Naga IR `Module` should have its user-defined outputs limited to
    /// those whose locations are included in hte given `BitSet`.
    // Should this be a struct of entry-point-specific options? Then shouldn't
    // *that* type be more appropriately named `EntryPointOptions`? Options like
    // `io` only make sense to apply globally. Ugh.
    pub user_output_masks: FastHashMap<usize, bit_set::BitSet>,
}

#[derive(Debug)]
pub enum ShaderStageIoStyle {
    /// Values are passed as arguments to the entry point.
    ///
    /// Metal and WGSL work this way.
    Arguments,

    /// Values are passed in global variables.
    ///
    /// SPIR-V and GLSL work this way.
    Globals,
}

#[derive(Debug)]
pub struct NamingRules {
    /// Reserved words in the language.
    pub keywords: &'static KeywordSet,

    /// Identifiers which can be shadowed, but which we should avoid defining
    /// anyway because synthesized code might want to use these bindings.
    pub builtin_identifiers: &'static KeywordSet,

    /// Words that are reserved regardless of case.
    pub keywords_case_insensitive: &'static CaseInsensitiveKeywordSet,

    /// Identifier prefixes that our definitions must not use.
    pub reserved_prefixes: Vec<&'static str>,
}
