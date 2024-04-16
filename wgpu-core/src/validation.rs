use crate::{
    device::bgl,
    id::{markers::Buffer, Id},
    FastHashMap, FastHashSet,
};
use arrayvec::ArrayVec;
use std::{collections::hash_map::Entry, fmt};
use thiserror::Error;
use wgt::{BindGroupLayoutEntry, BindingType};

#[derive(Debug)]
enum ResourceType {
    Buffer {
        size: wgt::BufferSize,
    },
    Texture {
        dim: naga::ImageDimension,
        arrayed: bool,
        class: naga::ImageClass,
    },
    Sampler {
        comparison: bool,
    },
}

#[derive(Debug)]
struct Resource {
    #[allow(unused)]
    name: Option<String>,
    bind: naga::ResourceBinding,
    ty: ResourceType,
    class: naga::AddressSpace,
}

#[derive(Clone, Copy, Debug)]
enum NumericDimension {
    Scalar,
    Vector(naga::VectorSize),
    Matrix(naga::VectorSize, naga::VectorSize),
}

impl fmt::Display for NumericDimension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { todo!() }
}

impl NumericDimension {
    fn num_components(&self) -> u32 { todo!() }
}

#[derive(Clone, Copy, Debug)]
pub struct NumericType {
    dim: NumericDimension,
    scalar: naga::Scalar,
}

impl fmt::Display for NumericType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { todo!() }
}

#[derive(Clone, Debug)]
pub struct InterfaceVar {
    pub ty: NumericType,
    interpolation: Option<naga::Interpolation>,
    sampling: Option<naga::Sampling>,
}

impl InterfaceVar {
    pub fn vertex_attribute(format: wgt::VertexFormat) -> Self { todo!() }
}

impl fmt::Display for InterfaceVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { todo!() }
}

#[derive(Debug)]
enum Varying {
    Local { location: u32, iv: InterfaceVar },
    BuiltIn(naga::BuiltIn),
}

#[allow(unused)]
#[derive(Debug)]
struct SpecializationConstant {
    id: u32,
    ty: NumericType,
}

#[derive(Debug, Default)]
struct EntryPoint {
    inputs: Vec<Varying>,
    outputs: Vec<Varying>,
    resources: Vec<naga::Handle<Resource>>,
    #[allow(unused)]
    spec_constants: Vec<SpecializationConstant>,
    sampling_pairs: FastHashSet<(naga::Handle<Resource>, naga::Handle<Resource>)>,
    workgroup_size: [u32; 3],
    dual_source_blending: bool,
}

#[derive(Debug)]
pub struct Interface {
    limits: wgt::Limits,
    features: wgt::Features,
    resources: naga::Arena<Resource>,
    entry_points: FastHashMap<(naga::ShaderStage, String), EntryPoint>,
}

#[derive(Clone, Debug, Error)]
#[error(
    "Usage flags {actual:?} for buffer {id:?} do not contain required usage flags {expected:?}"
)]
pub struct MissingBufferUsageError {
    pub(crate) id: Id<Buffer>,
    pub(crate) actual: wgt::BufferUsages,
    pub(crate) expected: wgt::BufferUsages,
}

/// Checks that the given buffer usage contains the required buffer usage,
/// returns an error otherwise.
pub fn check_buffer_usage(
    id: Id<Buffer>,
    actual: wgt::BufferUsages,
    expected: wgt::BufferUsages,
) -> Result<(), MissingBufferUsageError> { todo!() }

#[derive(Clone, Debug, Error)]
#[error("Texture usage is {actual:?} which does not contain required usage {expected:?}")]
pub struct MissingTextureUsageError {
    pub(crate) actual: wgt::TextureUsages,
    pub(crate) expected: wgt::TextureUsages,
}

/// Checks that the given texture usage contains the required texture usage,
/// returns an error otherwise.
pub fn check_texture_usage(
    actual: wgt::TextureUsages,
    expected: wgt::TextureUsages,
) -> Result<(), MissingTextureUsageError> { todo!() }

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum BindingError {
    #[error("Binding is missing from the pipeline layout")]
    Missing,
    #[error("Visibility flags don't include the shader stage")]
    Invisible,
    #[error("Type on the shader side does not match the pipeline binding")]
    WrongType,
    #[error("Storage class {binding:?} doesn't match the shader {shader:?}")]
    WrongAddressSpace {
        binding: naga::AddressSpace,
        shader: naga::AddressSpace,
    },
    #[error("Buffer structure size {0}, added to one element of an unbound array, if it's the last field, ended up greater than the given `min_binding_size`")]
    WrongBufferSize(wgt::BufferSize),
    #[error("View dimension {dim:?} (is array: {is_array}) doesn't match the binding {binding:?}")]
    WrongTextureViewDimension {
        dim: naga::ImageDimension,
        is_array: bool,
        binding: BindingType,
    },
    #[error("Texture class {binding:?} doesn't match the shader {shader:?}")]
    WrongTextureClass {
        binding: naga::ImageClass,
        shader: naga::ImageClass,
    },
    #[error("Comparison flag doesn't match the shader")]
    WrongSamplerComparison,
    #[error("Derived bind group layout type is not consistent between stages")]
    InconsistentlyDerivedType,
    #[error("Texture format {0:?} is not supported for storage use")]
    BadStorageFormat(wgt::TextureFormat),
    #[error(
        "Storage texture with access {0:?} doesn't have a matching supported `StorageTextureAccess`"
    )]
    UnsupportedTextureStorageAccess(naga::StorageAccess),
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum FilteringError {
    #[error("Integer textures can't be sampled with a filtering sampler")]
    Integer,
    #[error("Non-filterable float textures can't be sampled with a filtering sampler")]
    Float,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum InputError {
    #[error("Input is not provided by the earlier stage in the pipeline")]
    Missing,
    #[error("Input type is not compatible with the provided {0}")]
    WrongType(NumericType),
    #[error("Input interpolation doesn't match provided {0:?}")]
    InterpolationMismatch(Option<naga::Interpolation>),
    #[error("Input sampling doesn't match provided {0:?}")]
    SamplingMismatch(Option<naga::Sampling>),
}

/// Errors produced when validating a programmable stage of a pipeline.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum StageError {
    #[error("Shader module is invalid")]
    InvalidModule,
    #[error(
        "Shader entry point's workgroup size {current:?} ({current_total} total invocations) must be less or equal to the per-dimension limit {limit:?} and the total invocation limit {total}"
    )]
    InvalidWorkgroupSize {
        current: [u32; 3],
        current_total: u32,
        limit: [u32; 3],
        total: u32,
    },
    #[error("Shader uses {used} inter-stage components above the limit of {limit}")]
    TooManyVaryings { used: u32, limit: u32 },
    #[error("Unable to find entry point '{0}'")]
    MissingEntryPoint(String),
    #[error("Shader global {0:?} is not available in the pipeline layout")]
    Binding(naga::ResourceBinding, #[source] BindingError),
    #[error("Unable to filter the texture ({texture:?}) by the sampler ({sampler:?})")]
    Filtering {
        texture: naga::ResourceBinding,
        sampler: naga::ResourceBinding,
        #[source]
        error: FilteringError,
    },
    #[error("Location[{location}] {var} is not provided by the previous stage outputs")]
    Input {
        location: wgt::ShaderLocation,
        var: InterfaceVar,
        #[source]
        error: InputError,
    },
    #[error("Location[{location}] is provided by the previous stage output but is not consumed as input by this stage.")]
    InputNotConsumed { location: wgt::ShaderLocation },
    #[error(
        "Unable to select an entry point: no entry point was found in the provided shader module"
    )]
    NoEntryPointFound,
    #[error(
        "Unable to select an entry point: \
        multiple entry points were found in the provided shader module, \
        but no entry point was specified"
    )]
    MultipleEntryPointsFound,
}

fn map_storage_format_to_naga(format: wgt::TextureFormat) -> Option<naga::StorageFormat> { todo!() }

fn map_storage_format_from_naga(format: naga::StorageFormat) -> wgt::TextureFormat { todo!() }

impl Resource {
    fn check_binding_use(&self, entry: &BindGroupLayoutEntry) -> Result<(), BindingError> { todo!() }

    fn derive_binding_type(&self) -> Result<BindingType, BindingError> { todo!() }
}

impl NumericType {
    fn from_vertex_format(format: wgt::VertexFormat) -> Self { todo!() }

    fn from_texture_format(format: wgt::TextureFormat) -> Self { todo!() }

    fn is_subtype_of(&self, other: &NumericType) -> bool { todo!() }

    fn is_compatible_with(&self, other: &NumericType) -> bool { todo!() }
}

/// Return true if the fragment `format` is covered by the provided `output`.
pub fn check_texture_format(
    format: wgt::TextureFormat,
    output: &NumericType,
) -> Result<(), NumericType> { todo!() }

pub enum BindingLayoutSource<'a> {
    /// The binding layout is derived from the pipeline layout.
    ///
    /// This will be filled in by the shader binding validation, as it iterates the shader's interfaces.
    Derived(ArrayVec<bgl::EntryMap, { hal::MAX_BIND_GROUPS }>),
    /// The binding layout is provided by the user in BGLs.
    ///
    /// This will be validated against the shader's interfaces.
    Provided(ArrayVec<&'a bgl::EntryMap, { hal::MAX_BIND_GROUPS }>),
}

impl<'a> BindingLayoutSource<'a> {
    pub fn new_derived(limits: &wgt::Limits) -> Self { todo!() }
}

pub type StageIo = FastHashMap<wgt::ShaderLocation, InterfaceVar>;

impl Interface {
    fn populate(
        list: &mut Vec<Varying>,
        binding: Option<&naga::Binding>,
        ty: naga::Handle<naga::Type>,
        arena: &naga::UniqueArena<naga::Type>,
    ) { todo!() }

    pub fn new(
        module: &naga::Module,
        info: &naga::valid::ModuleInfo,
        limits: wgt::Limits,
        features: wgt::Features,
    ) -> Self { todo!() }

    pub fn finalize_entry_point_name(
        &self,
        stage_bit: wgt::ShaderStages,
        entry_point_name: Option<&str>,
    ) -> Result<String, StageError> { todo!() }

    pub(crate) fn shader_stage_from_stage_bit(stage_bit: wgt::ShaderStages) -> naga::ShaderStage { todo!() }

    pub fn check_stage(
        &self,
        layouts: &mut BindingLayoutSource<'_>,
        shader_binding_sizes: &mut FastHashMap<naga::ResourceBinding, wgt::BufferSize>,
        entry_point_name: &str,
        stage_bit: wgt::ShaderStages,
        inputs: StageIo,
        compare_function: Option<wgt::CompareFunction>,
    ) -> Result<StageIo, StageError> { todo!() }

    pub fn fragment_uses_dual_source_blending(
        &self,
        entry_point_name: &str,
    ) -> Result<bool, StageError> { todo!() }
}

// https://gpuweb.github.io/gpuweb/#abstract-opdef-calculating-color-attachment-bytes-per-sample
pub fn validate_color_attachment_bytes_per_sample(
    attachment_formats: impl Iterator<Item = Option<wgt::TextureFormat>>,
    limit: u32,
) -> Result<(), u32> { todo!() }
