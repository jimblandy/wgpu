use crate::{Epoch, Index};
use std::{
    cmp::Ordering,
    fmt::{self, Debug},
    hash::Hash,
    marker::PhantomData,
};
use wgt::{Backend, WasmNotSendSync};

type NonZeroId = std::num::NonZeroU64;

/// The raw underlying representation of an identifier.
#[repr(transparent)]
#[cfg_attr(
    any(feature = "serde", feature = "trace"),
    derive(serde::Serialize),
    serde(into = "SerialId")
)]
#[cfg_attr(
    any(feature = "serde", feature = "replay"),
    derive(serde::Deserialize),
    serde(from = "SerialId")
)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RawId(NonZeroId);

/// An identifier for a wgpu object.
///
/// An `Id<T>` value identifies a value stored in a [`Global`]'s [`Hub`].
///
/// ## Note on `Id` typing
///
/// You might assume that an `Id<T>` can only be used to retrieve a resource of
/// type `T`, but that is not quite the case. The id types in `wgpu-core`'s
/// public API ([`TextureId`], for example) can refer to resources belonging to
/// any backend, but the corresponding resource types ([`Texture<A>`], for
/// example) are always parameterized by a specific backend `A`.
///
/// So the `T` in `Id<T>` is usually a resource type like `Texture<Empty>`,
/// where [`Empty`] is the `wgpu_hal` dummy back end. These empty types are
/// never actually used, beyond just making sure you access each `Storage` with
/// the right kind of identifier. The members of [`Hub<A>`] pair up each
/// `X<Empty>` type with the resource type `X<A>`, for some specific backend
/// `A`.
///
/// [`Global`]: crate::global::Global
/// [`Hub`]: crate::hub::Hub
/// [`Hub<A>`]: crate::hub::Hub
/// [`Texture<A>`]: crate::resource::Texture
/// [`Registry`]: crate::hub::Registry
/// [`Empty`]: hal::api::Empty
#[repr(transparent)]
#[cfg_attr(any(feature = "serde", feature = "trace"), derive(serde::Serialize))]
#[cfg_attr(any(feature = "serde", feature = "replay"), derive(serde::Deserialize))]
#[cfg_attr(
    any(feature = "serde", feature = "trace", feature = "replay"),
    serde(transparent)
)]
pub struct Id<T: Marker>(RawId, PhantomData<T>);

// This type represents Id in a more readable (and editable) way.
#[allow(dead_code)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum SerialId {
    // The only variant forces RON to not ignore "Id"
    Id(Index, Epoch, Backend),
}

impl From<RawId> for SerialId {
    fn from(id: RawId) -> Self { todo!() }
}

impl From<SerialId> for RawId {
    fn from(id: SerialId) -> Self { todo!() }
}

impl<T> Copy for Id<T> where T: Marker {}

impl<T> Clone for Id<T>
where
    T: Marker,
{
    #[inline]
    fn clone(&self) -> Self { todo!() }
}

impl<T> Debug for Id<T>
where
    T: Marker,
{
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result { todo!() }
}

impl<T> Hash for Id<T>
where
    T: Marker,
{
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { todo!() }
}

impl<T> PartialEq for Id<T>
where
    T: Marker,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool { todo!() }
}

impl<T> Eq for Id<T> where T: Marker {}

impl<T> PartialOrd for Id<T>
where
    T: Marker,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { todo!() }
}

impl<T> Ord for Id<T>
where
    T: Marker,
{
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering { todo!() }
}

/// Marker trait used to determine which types uniquely identify a resource.
///
/// For example, `Device<A>` will have the same type of identifier as
/// `Device<B>` because `Device<T>` for any `T` defines the same maker type.
pub trait Marker: 'static + WasmNotSendSync {}

// This allows `()` to be used as a marker type for tests.
//
// We don't want these in production code, since they essentially remove type
// safety, like how identifiers across different types can be compared.
#[cfg(test)]
impl Marker for () {}

/// Define identifiers for each resource.
macro_rules! ids {
    ($(
        $(#[$($meta:meta)*])*
        pub type $name:ident $marker:ident;
    )*) => {
        /// Marker types for each resource.
        pub mod markers {
            $(
                #[derive(Debug)]
                pub enum $marker {}
                impl super::Marker for $marker {}
            )*
        }
    }
}

ids! {
    pub type AdapterId Adapter;
    pub type SurfaceId Surface;
    pub type DeviceId Device;
    pub type QueueId Queue;
    pub type BufferId Buffer;
    pub type StagingBufferId StagingBuffer;
    pub type TextureViewId TextureView;
    pub type TextureId Texture;
    pub type SamplerId Sampler;
    pub type BindGroupLayoutId BindGroupLayout;
    pub type PipelineLayoutId PipelineLayout;
    pub type BindGroupId BindGroup;
    pub type ShaderModuleId ShaderModule;
    pub type RenderPipelineId RenderPipeline;
    pub type ComputePipelineId ComputePipeline;
    pub type CommandEncoderId CommandEncoder;
    pub type CommandBufferId CommandBuffer;
    pub type RenderPassEncoderId RenderPassEncoder;
    pub type ComputePassEncoderId ComputePassEncoder;
    pub type RenderBundleEncoderId RenderBundleEncoder;
    pub type RenderBundleId RenderBundle;
    pub type QuerySetId QuerySet;
}

#[test]
fn test_id_backend() { todo!() }

#[test]
fn test_id() { todo!() }
