/*! Replace matrices with struct types, to match WGSL layout.

Naga IR matrices with two rows of four-byte elements have a stride of eight
bytes per column, but some backend languages require their matrix types to
allocate sixteen bytes per column, so we can't render Naga matrix types as the
obvious corresponding backend matrix types.

The [`ModuleBuilder::struct_for_cx2_matrices`] function adjusts the backend
module to store such types as struct types containing one member per matrix
column, and adjusts accesses accordingly.

Specifics:

- In Vulkan, SPIR-V blocks in uniform buffers must be aligned according to its
  "extended alignment", in which:

    > - An array or structure type has an extended alignment equal to the
    >   largest extended alignment of any of its members, rounded up to a
    >   multiple of 16.
    >
    > - A matrix type inherits extended alignment from the equivalent
    >   array declaration.

- For HLSL, according to the "High-Level Shader Language Specification" working
  draft as of 2026-9-10, DirectX constant buffers are "arranged like an array of
  16-byte rows, or 4-component vectors of 32-bit elements", so that:

  > Matrix types with more than one row in column major storage layout and
  > matrix types in row major storage layout in are aligned to the 16-byte row.
  > Each row in storage layout (each column for column major matrix) is aligned
  > to a 16-byte row.

*/

use crate::back;
use back::ir::builder::ModuleBuilder;

use alloc::vec::Vec;

impl ModuleBuilder {
    /// Replace matrix types with structs to satisfy [`Uniform`] address space
    /// alignment requirements.
    ///
    /// Search all variables in the [`Uniform`] address space for matrix types
    /// whose rows Naga IR lays out with less than a sixteen-byte stride, and
    /// replace those matrices with synthesized struct types with a member for
    /// each column, to evade the platform's stricter alignment requirements
    /// as explained in [the module documentation][self].
    /// 
    /// Adjust all accesses to the replaced matrices accordingly:
    ///
    /// - For `ExtractElement` expressions that retrieve a particular column from
    ///   a matrix:
    ///
    ///     - If the index is constant, change the expression into an
    ///       `ExtractMember` expression.
    ///
    ///     - If the index is not constant, change the expression into a call to
    ///       a synthesized helper function that uses a `switch` statement to
    ///       return the right member.
    ///
    /// - For `ExtractElement` 
    ///
    /// - There should be no `PointerElement` expressions to values in the
    ///   `Uniform` address space, since it is read-only.
    ///
    /// Does HLSL permit dynamic indices on by-value matrices and arrays? If so,
    /// perhaps we can always just load 
    ///
    /// [`Uniform`]: back::ir::AddressSpace::Uniform
    pub fn struct_for_cx2_matrices(&self) {
        todo!(); // and see also  the comments above.
    }
}
