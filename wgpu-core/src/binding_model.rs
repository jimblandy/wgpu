#[cfg(feature = "trace")]
use crate::device::trace;
use crate::{
    device::{
        bgl, Device, DeviceError, MissingDownlevelFlags, MissingFeatures, SHADER_STAGE_COUNT,
    },
    error::{ErrorFormatter, PrettyError},
    hal_api::HalApi,
    id::{BindGroupLayoutId, BufferId, SamplerId, TextureId, TextureViewId},
    init_tracker::{BufferInitTrackerAction, TextureInitTrackerAction},
    resource::{Resource, ResourceInfo, ResourceType},
    resource_log,
    snatch::{SnatchGuard, Snatchable},
    track::{BindGroupStates, UsageConflict},
    validation::{MissingBufferUsageError, MissingTextureUsageError},
    Label,
};

use arrayvec::ArrayVec;

#[cfg(feature = "serde")]
use serde::Deserialize;
#[cfg(feature = "serde")]
use serde::Serialize;

use std::{borrow::Cow, ops::Range, sync::Arc};

use thiserror::Error;

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum BindGroupLayoutEntryError {
    #[error(transparent)]
    MissingFeatures(#[from] MissingFeatures),
    #[error(transparent)]
    MissingDownlevelFlags(#[from] MissingDownlevelFlags),
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CreateBindGroupLayoutError {
    #[error(transparent)]
    Device(#[from] DeviceError),
}

//TODO: refactor this to move out `enum BindingError`.

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CreateBindGroupError {
    #[error(transparent)]
    Device(#[from] DeviceError),
    #[error(transparent)]
    MissingBufferUsage(#[from] MissingBufferUsageError),
    #[error(transparent)]
    MissingTextureUsage(#[from] MissingTextureUsageError),
    #[error(transparent)]
    ResourceUsageConflict(#[from] UsageConflict),
}

impl PrettyError for CreateBindGroupError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

#[derive(Clone, Debug, Error)]
pub enum BindingZone {
}

#[derive(Clone, Debug, Error)]
#[error("Too many bindings of type {kind:?} in {zone}, limit is {limit}, count was {count}. Check the limit `{}` passed to `Adapter::request_device`", .kind.to_config_str())]
pub struct BindingTypeMaxCountError {
    pub kind: BindingTypeMaxCountErrorKind,
    pub zone: BindingZone,
    pub limit: u32,
    pub count: u32,
}

#[derive(Clone, Debug)]
pub enum BindingTypeMaxCountErrorKind {
}

impl BindingTypeMaxCountErrorKind {
    fn to_config_str(&self) -> &'static str { todo!() }
}

#[derive(Debug, Default)]
pub(crate) struct PerStageBindingTypeCounter {
}

impl PerStageBindingTypeCounter {
}

#[derive(Debug, Default)]
pub(crate) struct BindingTypeMaxCountValidator {
}

impl BindingTypeMaxCountValidator {
}

/// Bindable resource and the slot to bind it to.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BindGroupEntry<'a> {
    /// Slot for which binding provides resource. Corresponds to an entry of the same
    /// binding index in the [`BindGroupLayoutDescriptor`].
    pub binding: u32,
    /// Resource to attach to the binding
    pub resource: BindingResource<'a>,
}

/// Describes a group of bindings and the resources to be bound.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BindGroupDescriptor<'a> {
    /// Debug label of the bind group.
    ///
    /// This will show up in graphics debuggers for easy identification.
    pub label: Label<'a>,
    /// The [`BindGroupLayout`] that corresponds to this bind group.
    pub layout: BindGroupLayoutId,
    /// The resources to bind to this bind group.
    pub entries: Cow<'a, [BindGroupEntry<'a>]>,
}

/// Describes a [`BindGroupLayout`].
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BindGroupLayoutDescriptor<'a> {
    /// Debug label of the bind group layout.
    ///
    /// This will show up in graphics debuggers for easy identification.
    pub label: Label<'a>,
    /// Array of entries in this BindGroupLayout
    pub entries: Cow<'a, [wgt::BindGroupLayoutEntry]>,
}

/// Bind group layout.
#[derive(Debug)]
pub struct BindGroupLayout<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}

impl<A: HalApi> Drop for BindGroupLayout<A> {
    fn drop(&mut self) { todo!() }
}

impl<A: HalApi> Resource for BindGroupLayout<A> {
    const TYPE: ResourceType = "BindGroupLayout";

    type Marker = crate::id::markers::BindGroupLayout;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }

    fn label(&self) -> String { todo!() }
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CreatePipelineLayoutError {
    #[error(transparent)]
    Device(#[from] DeviceError),
}

impl PrettyError for CreatePipelineLayoutError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum PushConstantUploadError {
}

/// Describes a pipeline layout.
///
/// A `PipelineLayoutDescriptor` can be used to create a pipeline layout.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PipelineLayoutDescriptor<'a> {
    /// Debug label of the pipeline layout.
    ///
    /// This will show up in graphics debuggers for easy identification.
    pub label: Label<'a>,
    /// Bind groups that this pipeline uses. The first entry will provide all the bindings for
    /// "set = 0", second entry will provide all the bindings for "set = 1" etc.
    pub bind_group_layouts: Cow<'a, [BindGroupLayoutId]>,
    /// Set of push constant ranges this pipeline uses. Each shader stage that
    /// uses push constants must define the range in push constant memory that
    /// corresponds to its single `layout(push_constant)` uniform block.
    ///
    /// If this array is non-empty, the
    /// [`Features::PUSH_CONSTANTS`](wgt::Features::PUSH_CONSTANTS) feature must
    /// be enabled.
    pub push_constant_ranges: Cow<'a, [wgt::PushConstantRange]>,
}

#[derive(Debug)]
pub struct PipelineLayout<A: HalApi> {
    marker: std::marker::PhantomData<A>,    
}

impl<A: HalApi> Drop for PipelineLayout<A> {
    fn drop(&mut self) { todo!() }
}

impl<A: HalApi> PipelineLayout<A> {
}

impl<A: HalApi> Resource for PipelineLayout<A> {
    const TYPE: ResourceType = "PipelineLayout";

    type Marker = crate::id::markers::PipelineLayout;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BufferBinding {
    pub buffer_id: BufferId,
    pub offset: wgt::BufferAddress,
    pub size: Option<wgt::BufferSize>,
}

// Note: Duplicated in `wgpu-rs` as `BindingResource`
// They're different enough that it doesn't make sense to share a common type
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BindingResource<'a> {
    Buffer(BufferBinding),
    BufferArray(Cow<'a, [BufferBinding]>),
    Sampler(SamplerId),
    SamplerArray(Cow<'a, [SamplerId]>),
    TextureView(TextureViewId),
    TextureViewArray(Cow<'a, [TextureViewId]>),
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum BindError {
}

#[derive(Debug)]
pub struct BindGroupDynamicBindingData {
}

#[derive(Debug)]
pub struct BindGroup<A: HalApi> {
    pub(crate) raw: Snatchable<A::BindGroup>,
    pub(crate) device: Arc<Device<A>>,
    pub(crate) layout: Arc<BindGroupLayout<A>>,
    pub(crate) info: ResourceInfo<BindGroup<A>>,
    pub(crate) used: BindGroupStates<A>,
    pub(crate) used_buffer_ranges: Vec<BufferInitTrackerAction<A>>,
    pub(crate) used_texture_ranges: Vec<TextureInitTrackerAction<A>>,
    pub(crate) dynamic_binding_info: Vec<BindGroupDynamicBindingData>,
    /// Actual binding sizes for buffers that don't have `min_binding_size`
    /// specified in BGL. Listed in the order of iteration of `BGL.entries`.
    pub(crate) late_buffer_binding_sizes: Vec<wgt::BufferSize>,
}

impl<A: HalApi> Drop for BindGroup<A> {
    fn drop(&mut self) { todo!() }
}

impl<A: HalApi> BindGroup<A> {
    pub(crate) fn raw(&self, guard: &SnatchGuard) -> Option<&A::BindGroup> { todo!() }
    pub(crate) fn validate_dynamic_bindings(
        &self,
        bind_group_index: u32,
        offsets: &[wgt::DynamicOffset],
        limits: &wgt::Limits,
    ) -> Result<(), BindError> { todo!() }
}

impl<A: HalApi> Resource for BindGroup<A> {
    const TYPE: ResourceType = "BindGroup";

    type Marker = crate::id::markers::BindGroup;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum GetBindGroupLayoutError {
    #[error("Pipeline is invalid")]
    InvalidPipeline,
    #[error("Invalid group index {0}")]
    InvalidGroupIndex(u32),
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
#[error("Buffer is bound with size {bound_size} where the shader expects {shader_size} in group[{group_index}] compact index {compact_index}")]
pub struct LateMinBufferBindingSizeMismatch {
    pub group_index: u32,
    pub compact_index: usize,
    pub shader_size: wgt::BufferAddress,
    pub bound_size: wgt::BufferAddress,
}
