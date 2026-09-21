/*! Use custom representations for certain types in memory.

This module defines [`ModuleBuilder::adjust_store_types`], a pass that allows a
given type to be stored as a different one, given lossless conversions between
the two. This can be useful when Naga IR layout does not satisfy the platform
language's layout requirements.

For example, Naga IR `mat2x2<f32>` values have a different layout than HLSL's
`matrix<f32, 2, 2>` in a constant buffer, so `mat2x2<f32>` must be stored as
some other HLSL type that has the correct layout, like:

    struct NagaMat2x2f32 { float2 c0; float2 c1; };

Here we'd call `mat2x2<f32>` "the replaced type" and `NagaMat2x2f32` "the store
type". We'd say "`mat2x2<f32>` is stored as `NagaMat2x2f32`".

Whenever some type `R` is stored as some other type `RS`, that induces any type
containing an `R` to have its own store type in turn:

- If a struct has a member of type `R`, then it is stored as another struct in
  which that member has type `RS`. This rule may apply to multiple members of
  the struct.

- An array of `R` has a store type: an array of `RS`.

If `R` is stored as `RS` in address space `AS`, then obviously `ptr<AS, R, AM>`
must be replaced with `ptr<AS, RS, AM>`. Pointer types are never stored, so it
would be confusing to call the latter "the store type" of the former, so we call
it "the replacement type" instead.




if a pointer's target type is
stored as some other type,


- A pointer to `R` has a replacement type: a pointer to `RS`.

If a replaced type `R` with a store type `RS` appears in some compound
type `C` (as struct member or array element, for example), then `C` is
itself a replaced type. Its store type is the analogous type with each
`R` replaced by `RS`. This rule applies iteratively to larger and
larger compound types.


  



The `adjust_store_types` function can actually replace several types in a single
pass, since doing so doesn't add much complexity.

See the [Motivation][#motivation] section below for details about the specific
Naga IR / platform mismatches we address with this pass.

# The transformation

The caller provides a mapping from certain types appearing in the module
now to the types that should be used to represent them in memory. 

TODO: The interface to the pass should be co-designed with its users:

- Scanning the module for types that need to be updated is likely to produce
  information (sets of affected types and variables, say) useful to the pass.

- The pass itself can accumulate the set of helper functions it actually needed,
  for subsequent code to synthesize.

If some type `T` must be replaced when it appears in memory:

- Structs with members of type `T` and arrays of type `T` must also be replaced
  when they appear in memory. 

- Pointers to `T` must be replaced everywhere they appear.

These rule must be applied iteratively to identify all affected types. TODO:
Perhaps make a pass over the type arena from leaf types upwards, classifying
types as unaffected; affected if stored; or always affected. Store types could
be constructed in tandem with this classification.

Expressions that form pointers to an element or member of a replaced type must
be rewritten to operate on the store type instead. Dynamic indexing is
implemented by calls to synthesized helper functions.

Loads and stores of replaced non-array types are implemented by synthesized
helper functions, to convert between the original and replaced types.

Loads and stores of replaced array types do not convert (as such a conversion
step would introduce a loop). Instead, the store type is used as a value until the value is
consumed.

When 


if those types are . This must be
iterated until all affected types are identified.


- 

Because we want to avoid introducing loops, all array types whose elements are
or contain replaced types are replaced themselves.

For pointer formation, `ElementPointer` and `MemberPointer` expressions on
replaced types are rewritten to operate on the store types.



# Motivation

The platform APIs that use Naga's target shader languages impose stricter
alignment requirements in a few circumstances.

## Vulkan

In Vulkan, SPIR-V blocks in uniform buffers must be aligned according to its
["extended alignment"][ea], in which:

> - An array or structure type has an extended alignment equal to the
>   largest extended alignment of any of its members, rounded up to a
>   multiple of 16.
>
> - A matrix type inherits extended alignment from the equivalent
>   array declaration.

[ea]: https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-alignment-requirements

## Direct3D

Direct3D uses HLSL constant buffers for both root constants (the analogue of
WebGPU immediate data) and constant buffer views (WebGPU uniform buffers). Data
stored in constant buffers must meet stricter alignment requirements.

In the HLSL spec, [§9.5.2 Constant Buffer Layout][hlsl Resources.cnbuf.lay]
says:

> A constant buffer is arranged like an array of 16-byte rows, or 4-component
> vectors of 32-bit elements. Shader constants are arranged into the buffer in
> the order they were declared based on following rules.
>
> ...
>
> Matrix types ... in column major storage layout and matrix types in row major
> storage layout in are aligned to the 16-byte row. Each row in storage layout
> (each column for column major matrix) is aligned to a 16-byte row.

(This is quoted from the "High-Level Shader Language Specification" working
draft as of 2026-9-10.)

One surprising aspect of HLSL is that the layout of an HLSL type depends on
where it's stored. For example, a `float3x3` (without type qualifiers) is laid
out column by column; in a constant buffer, each column occupies sixteen bytes,
whereas in a `RWStructuredBuffer`, each column occupies twelve bytes. This
implies that types have no single size: [Chapter 3 footnote five][ch3 fn5] in
the HLSL specification says:

> sizeof(T) returns the size of the object as if it’s stored in device memory,
> and determining the size if it’s stored in another memory space is not
> possible.

where "device memory" refers to `RWByteAddressBuffer`s and `RWStructuredBuffer`s.

[hlsl Resources.cnbuf.lay]: https://microsoft.github.io/hlsl-specs/specs/9-Resources.html#Resources.cnbuf.lay
[ch3 fn5]: https://microsoft.github.io/hlsl-specs/specs/3-Basic.html#fn5

*/

use crate::back;
use back::ir::builder::ModuleBuilder;

use alloc::vec::Vec;

impl ModuleBuilder {
    /// Replace matrix types with structs to satisfy address space alignment
    /// requirements.
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
    /// - There should be no `ElementPointer` expressions to values in the
    ///   `Uniform` address space, since it is read-only.
    ///
    /// HLSL permits dynamic indices on by-value matrices and arrays.
    ///
    /// Q: Does HLSL accept tight matrices in storage buffers? 
    ///
    /// [`Uniform`]: back::ir::AddressSpace::Uniform
    pub fn struct_for_cx2_matrices(&self) {
        todo!(); // and see also  the comments above.
    }
}
