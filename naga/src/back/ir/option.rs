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
    pub naming_rules: Option<NamingRules>,
}

/// Options for lowering types and operations on them.
#[derive(Debug)]
pub struct TypeOptions {
    /// Replace matrices that have two rows of four-byte elements with structs
    /// with a separate member for each vector.
    ///
    /// Naga matrices with two rows of four-byte elements have a stride of eight
    /// bytes per column, but some backend languages require their matrix types
    /// to allocate sixteen bytes per column, so we can't render Naga matrix
    /// types as the obvious corresponding backend matrix types. Setting this
    /// flag directs lowering to store such Naga types as struct types
    /// containing one member per matrix column, and adjust accesses
    /// accordingly.
    pub replace_cx2_matrix_with_struct: bool,

    /// Use only the given orientation for matrix types in the output.
    ///
    /// HLSL matrices are row-major: `matrix<T, N, M>` is a matrix of `N` rows
    /// and `M columns, and `m[i]` retrieves the `i`'th *row* of a matrix `m`.
    /// However, unless the `row_major` type qualifier is present, HLSL matrices
    /// are *stored* in column-major order. Naga backend IR has no representation
    /// for matrices that are indexed one way but stored in the other, so all
    /// matrix types in generated HLSL must have the `row_major` qualifier --- at
    /// least if they are stored anywhere. The qualifier has no effect on
    /// indexing or multiplication operations, and HLSL treats qualified and
    /// unqualified matrix types as interconvertible.
    ///
    /// Even though a Naga IR expression `Access { base, index }`, where `base`
    /// is a matrix, retrieves the `index`'th column of `base`, it is still
    /// possible to translate such expressions to `m[i]` in a row-major language
    /// like HLSL, with some trickery:
    ///
    /// - As explained above, assume that both indexing and storage are
    ///   row-major: no mixed-orientation madness.
    ///
    /// - Render a Naga IR column-major CxR matrix as a backend IR row-major
    ///   matrix with C rows and R columns: that is, transpose the row and column
    ///   dimensions.
    ///
    /// - Perform loads and stores *directly*. If the contents of memory are laid
    ///   out as column-major CxR matrix, loading that as a row-major matrix of C
    ///   rows and R columns gives you the transpose of the intended value.
    ///
    /// - Since the matrices are transposed, an indexing expression on a
    ///   row-major matrix retrieves the "columns" of the intended Naga IR value,
    ///   so `Access { base, index }` can be rendered as `base[index]`.
    /// 
    /// - For vector-matrix multiplication, since `transpose(m) * v` is
    ///   equivalent to `v * m` (note the reversal of the operands), and
    ///   `v * transpose(m)` is equivalent to `m * v`, we can render Naga IR
    ///   `m * v` and `v * m` simply by reversing the operands.
    pub matrix_orientation: back::ir::MatrixOrientation,

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
    pub keywords: &'static KeywordSet,
    pub builtin_identifiers: &'static KeywordSet,
    pub keywords_case_insensitive: &'static CaseInsensitiveKeywordSet,
    pub reserved_prefixes: Vec<&'static str>,
}
