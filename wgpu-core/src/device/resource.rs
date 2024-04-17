#[cfg(feature = "trace")]
use crate::device::trace;
use crate::{
    binding_model::{self, BindGroup, BindGroupLayout, BindGroupLayoutEntryError},
    command, conv,
    device::{
        bgl,
        life::{LifetimeTracker, WaitIdleError},
        queue::PendingWrites,
        AttachmentData, DeviceLostInvocation, MissingDownlevelFlags, MissingFeatures,
        RenderPassContext, 
    },
    hal_api::HalApi,
    hub::Hub,
    init_tracker::{
        BufferInitTracker, BufferInitTrackerAction, MemoryInitKind, TextureInitRange,
        TextureInitTracker, TextureInitTrackerAction,
    },
    instance::Adapter,
    pipeline,
    pool::ResourcePool,
    registry::Registry,
    resource::{
        self, Buffer, QuerySet, Resource, ResourceInfo, ResourceType, Sampler, Texture,
        TextureView, TextureViewNotRenderableReason,
    },
    resource_log,
    snatch::{SnatchGuard, SnatchLock, Snatchable},
    storage::Storage,
    track::{
        BindGroupStates, TextureSelector, Tracker, TrackerIndexAllocators, UsageScope,
        UsageScopePool,
    },
    validation::{
        self, check_buffer_usage, check_texture_usage, validate_color_attachment_bytes_per_sample,
    },
    FastHashMap, LabelHelpers as _, SubmissionIndex,
};

use arrayvec::ArrayVec;
use hal::{CommandEncoder as _, Device as _};
use once_cell::sync::OnceCell;
use parking_lot::{Mutex, MutexGuard, RwLock};

use smallvec::SmallVec;
use thiserror::Error;
use wgt::{DeviceLostReason, TextureFormat, TextureSampleType, TextureViewDimension};

use std::{
    borrow::Cow,
    iter,
    num::NonZeroU32,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc, Weak,
    },
};

use super::{
    life::{self, ResourceMaps},
    queue::{self, Queue},
    DeviceDescriptor, DeviceError, ImplicitPipelineContext, UserClosures,
};

/// Structure describing a logical device. Some members are internally mutable,
/// stored behind mutexes.
///
/// TODO: establish clear order of locking for these:
/// `life_tracker`, `trackers`, `render_passes`, `pending_writes`, `trace`.
///
/// Currently, the rules are:
/// 1. `life_tracker` is locked after `hub.devices`, enforced by the type system
/// 1. `self.trackers` is locked last (unenforced)
/// 1. `self.trace` is locked last (unenforced)
///
/// Right now avoid locking twice same resource or registry in a call execution
/// and minimize the locking to the minimum scope possible
/// Unless otherwise specified, no lock may be acquired while holding another lock.
/// This means that you must inspect function calls made while a lock is held
/// to see what locks the callee may try to acquire.
///
/// As far as this point:
/// device_maintain_ids locks Device::lifetime_tracker, and calls...
/// triage_suspected locks Device::trackers, and calls...
/// Registry::unregister locks Registry::storage
///
/// Important:
/// When locking pending_writes please check that trackers is not locked
/// trackers should be locked only when needed for the shortest time possible
#[allow(dead_code)] // JIMB 0.76s
pub struct Device<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}

#[allow(dead_code)] // JIMB
pub(crate) enum DeferredDestroy<A: HalApi> {
    TextureView(Weak<TextureView<A>>),
    BindGroup(Weak<BindGroup<A>>),
}

impl<A: HalApi> std::fmt::Debug for Device<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { todo!() }
}

impl<A: HalApi> Drop for Device<A> {
    fn drop(&mut self) { todo!() }
}

#[derive(Clone, Debug, Error)]
pub enum CreateDeviceError {
}


impl<A: HalApi> Resource for Device<A> {
    const TYPE: ResourceType = "Device";

    type Marker = crate::id::markers::Device;

    fn as_info(&self) -> &ResourceInfo<Self> { todo!() }

    fn as_info_mut(&mut self) -> &mut ResourceInfo<Self> { todo!() }
}
