/*! Use row-indexed matrix types in the output

This module implements [`ModuleBuilder::transpose_matrices`], which
edits a backend module to use exclusively row-indexed matrix types.

This helps us target HLSL, in which matrices are indexed by row:
`matrix<T, N, M>` is a matrix of `N` rows and `M` columns, and `m[i]`
retrieves the `i`-th *row* of a matrix `m`. This is independent of their
layout in memory, which can be either row-by-row or column-by-column, as
selected by type qualifiers. Matrices with different layout qualifiers
are freely interconvertable.

Even though a Naga IR expression `Access { base, index }`, where `base`
is a matrix, retrieves the `index`-th column of `base`, it is still
possible to translate such expressions to `m[i]` in a row-indexed
language like HLSL. Just treat everything as transposed:

- Render a Naga IR column-indexed CxR matrix as a backend IR row-indexed
  matrix with C rows and R columns: that is, transpose the row and
  column dimensions.

- Since the matrices are transposed, an indexing expression on a
  row-major matrix retrieves the "columns" of the intended Naga IR value,
  so `Access { base, index }` can be rendered simply as `base[index]`.

- Use row-by-row layout for stored matrices, and perform loads and
  stores *directly*. Since each row in the lowered matrix represents a
  column in the Naga IR matrix, this effectively stores the original
  values in column-by-column layout.

- For vector-matrix multiplication, since `transpose(m) * v` is
  equivalent to `v * m` (note the reversal of the operands), and `v *
  transpose(m)` is equivalent to `m * v` (same), we can render Naga IR
  `m * v` and `v * m` simply by reversing the operands. For
  matrix-matrix multiplication, `transpose(m) * transpose(n)` is `n * m`.

*/

use crate::back;
use back::ir::builder::ModuleBuilder;

use alloc::vec::Vec;

impl ModuleBuilder {
    /// Edit the module to use only row-indexed matrix types.
    ///
    /// See the [module documentation](self) for the rationale.
    /// 
    /// Replace all column-indexed, column-by-column layout matrix types in
    /// `self.module` with row-indexed, row-by-row layout matrix types, such
    /// that all matrix values are transposed.
    ///
    /// Swap the operands of all matrix-by-vector and vector-by-matrix
    /// multiplies, so that they produce the same results as before.
    ///
    /// Swap the operands of all matrix-by-matrix multiplies, so that they
    /// produce the transpose of the results they would have before.
    pub fn transpose_matrices(&mut self) {
        todo!()
    }
}
