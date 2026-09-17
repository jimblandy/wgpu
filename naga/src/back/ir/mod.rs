/*! Backend IR: a backend-oriented IR for shader modules.

This module defines [`naga::back::ir::Module`], Naga's backend intermediate
representation (IR). We lower ordinary, validated [`naga::ir::Module`]s to this
IR, and then backends generate SPIR-V, HLSL, Metal Shading Language, directly
from the backend IR.

The backend IR closely mirrors the structure of the shader languages we want to
generate, to allow the language-specific code to remain simple. Generating
SPIR-V, HLSL, or Metal Shading Language from this IR should require minimal
last-minute transformations.

Of course, Naga's backend languages differ from each other in significant ways.
The backend IR incorporates all the features we need to use from any of those
languages. Lowering a [`naga::ir::Module`] to the backend IR consults an
[`Options`] value to see which features to use and which lowerings to apply,
such that the language-specific backends can simply panic if they see anything
they don't recognize (perhaps a feature specific to a different language).

Even though each backend expects a different dialect of the backend IR, many
important transformations are the same for all backends, or very similar. Having
a common IR allows all languages to share code for these transformations. For
example, WebGPU language extensions like `unrestricted_pointer_parameters` and
`uniform_buffer_standard_layout` are similar for all backend languages.

# Lowering from Naga IR to backend IR

Given a [`naga::ir::Module`] and a [`naga::valid::ModuleInfo`] derived from it,
the [`naga::back::ir::lower`] lower function produces a [`back::ir::Module`]
suitable for generating target code. The [`lower`] function takes an [`Options`]
value that selects the transformations necessary to produce the dialect that
backend accepts.

[`back::ir::Module`]: Module

*/

#![allow(unused)]

mod adjust_names;
mod builder;
mod function;
pub mod option;
mod r#type;
mod utils;

use option::Options;

// The following backend IR types are identical to their frontend counterparts.
// Don't `use` frontend IR types otherwise; refer to them with an `ir::` prefix.
pub use crate::ir::{BuiltIn, ImageClass, ImageDimension, Interpolation, Sampling, Scalar, ScalarKind, StorageAccess, StorageFormat, VectorSize};
use crate::ir;

use crate::FastHashMap;
use crate::arena::{Arena, Handle, UniqueArena};

use alloc::string::String;
use alloc::vec::Vec;

pub use utils::compute_user_input_mask;

/// A backend IR module, suitable for generating platform shader code.
///
/// This type represents the same code as a [`naga::ir::Module`], but lowered to
/// something closer to the actual backend languages' features, so that
/// generating the backend code should be straightforward: backends should be
/// able to focus on generating syntactically correct, legible output, without
/// the complexity of applying last-minute transformations.
///
/// See the [module documentation][self] for details.
///
/// [`naga::ir::Module`]: crate::ir::Module
#[derive(Debug, Default)]
pub struct Module {
    /// Types used by this module.
    ///
    /// Many lowering transformations introduce new types; for example; see
    /// [`TypeOptions::replace_cx2_matrix_with_struct`]. So in general this
    /// arena is not a direct translation of the Naga module's type arena.
    pub types: UniqueArena<Type>,

    /// Inner types used by this module.
    pub inner_types: UniqueArena<TypeInner>,

    pub globals: Arena<GlobalVariable>,

    /// The module's functions, including entry points.
    ///
    /// This arena is ordered so that callees appear before callers. 
    pub functions: Arena<Function>,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Type {
    /// Name of the type. If `None`, the type is written using the language's
    /// usual syntax.
    ///
    /// If given, the name is always unique within the module, and never
    /// conflicts with the backend language's reserved words.
    pub name: Option<String>,

    /// Specifics of this type.
    pub inner: Handle<TypeInner>,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum TypeInner {
    /// A single scalar value, either integer or floating-point.
    Scalar(Scalar),

    Vector {
        size: VectorSize,

        /// The vector's element type. This must be a [`TypeInner::Scalar`].
        scalar: Handle<Type>
    },

    Matrix {
        /// Whether this matrix is row-major or column-major.
        ///
        /// If this is `ColMajor`, then the "outer elements" of the
        /// matrix are columns: `size` is the number of columns, and
        /// `element` is the column type. If this is `RowMajor`, then
        /// those refer to rows, instead.
        ///
        /// This also describes how the matrix is stored in memory.
        ///
        /// See [`option::TypeOptions::matrix_orientation`] for
        /// relevant details.
        orientation: MatrixOrientation,

        /// The number of outer elements in the matrix.
        size: VectorSize,

        /// The type of one matrix element: a column if `row_major` is
        /// `false`, or a row if `row_major` is true. This handle must
        /// refer to a [`TypeInner::Vector`].
        element: Handle<Type>,
    },

    Atomic(Scalar),

    Pointer {
        base: Handle<Type>,
        space: AddressSpace,
    },

    Array {
        base: Handle<Type>,
        size: ArraySize,
    },

    Struct {
        members: Vec<StructMember>,
    },

    Image(ImageType),

    Sampler { comparison: bool },

    AccelerationStructure { vertex_return: bool },
    RayQuery { vertex_return: bool },
    BindingArray { base: Handle<Type>, size: Option<usize> },
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MatrixOrientation {
    RowMajor,
    ColumnMajor,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AddressSpace {
    Function,
    Private,
    WorkGroup,
    Uniform,
    Storage { access: StorageAccess },
    Handle,
    Immediate,
    TaskPayload,
    RayPayload,
    IncomingRayPayload,
    Input,
    Output,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum ArraySize {
    Constant { size: u32, size_type: Handle<Type> },
    Dynamic,
}

/// A backend image type.
#[derive(Debug, PartialEq, Hash, Eq, Copy, Clone)]
pub struct ImageType {
    pub sampled_type: Scalar,
    pub dim: ImageDimension,
    pub flags: ImageTypeFlags,
    pub image_format: StorageFormat,
}

bitflags::bitflags! {
    /// Flags corresponding to the boolean(-ish) image parameters
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
    pub struct ImageTypeFlags: u8 {
        const DEPTH = 0x1;
        const ARRAYED = 0x2;
        const MULTISAMPLED = 0x4;
        const SAMPLED = 0x8;
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct StructMember {
    pub name: Option<String>,
    pub ty: Handle<Type>,
    pub offset: u32,
}

#[derive(Debug)]
pub struct GlobalVariable {
    pub name: Option<String>,
    pub address_space: AddressSpace,
    pub ty: Handle<Type>,
    pub attributes: Vec<Attribute>,
}

/// Attributes to be placed on globals, parameters, etc.
#[derive(Debug)]
pub enum Attribute {
    /// A builtin value.
    BuiltIn(BuiltIn),

    /// A user-defined I/O value.
    ///
    /// The meanings of the fields are the same as for [`ir::Binding::Location`].
    UserDefined {
        location: u32,
        interpolation: Option<Interpolation>,
        sampling: Option<Sampling>,
        blend_src: Option<u32>,
        per_primitive: bool,
    },

    /// Buffer bound at `index` in a flat buffer name space (Metal)
    BufferIndex(usize),

    /// Texture bound at `index` in a flat buffer name space (Metal)
    TextureIndex(usize),
}

#[derive(Debug)]
pub struct Function {
    pub name: Option<String>,
    pub arguments: Vec<Argument>,
    pub entry_point_info: Option<EntryPointInfo>
}

#[derive(Debug)]
pub struct Argument {
    pub name: Option<String>,
    pub ty: Handle<Type>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug)]
pub struct EntryPointInfo {
    /// The entry point name. This is guaranteed to be a valid 
    pub name: String,
    pub stage: EntryPointStageInfo,
}

#[derive(Debug)]
pub enum EntryPointStageInfo {
    Compute {
        workgroup_size: [u32; 3],
    }
}

pub fn lower(module: &ir::Module,
             info: &crate::valid::ModuleInfo,
             options: Options) -> Module
{
    let mut builder = builder::ModuleBuilder::new(module, info, options);

    let mut out = Module::default();

    for (handle, function) in module.functions.iter() {
        builder.lower_function(handle, function, &mut out);
    }
    
    for entry_point in &module.entry_points {
        builder.lower_entry_point(entry_point, &mut out);
    }
    
    builder.adjust_names(&mut out);

    out
}
