/*! Presentation.

## Lifecycle

Whenever a submission detects the use of any surface texture, it adds it to the device
tracker for the duration of the submission (temporarily, while recording).
It's added with `UNINITIALIZED` state and transitioned into `empty()` state.
When this texture is presented, we remove it from the device tracker as well as
extract it from the hub.
!*/

use std::{borrow::Borrow, sync::Arc};

#[cfg(feature = "trace")]
use crate::device::trace::Action;
use crate::{
    conv,
    device::any_device::AnyDevice,
    device::{DeviceError, MissingDownlevelFlags, WaitIdleError},
    global::Global,
    hal_api::HalApi,
    id,
    init_tracker::TextureInitTracker,
    resource::{self, ResourceInfo},
    snatch::Snatchable,
    track,
};

use hal::{Queue as _, Surface as _};
use parking_lot::{Mutex, RwLock};
use thiserror::Error;
use wgt::SurfaceStatus as Status;

const FRAME_TIMEOUT_MS: u32 = 1000;

#[derive(Debug)]
pub(crate) struct Presentation {
    pub(crate) device: AnyDevice,
    pub(crate) config: wgt::SurfaceConfiguration<Vec<wgt::TextureFormat>>,
    pub(crate) acquired_texture: Option<id::TextureId>,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum SurfaceError {
    #[error("Surface is invalid")]
    Invalid,
    #[error("Surface is not configured for presentation")]
    NotConfigured,
    #[error(transparent)]
    Device(#[from] DeviceError),
    #[error("Surface image is already acquired")]
    AlreadyAcquired,
    #[error("Acquired frame is still referenced")]
    StillReferenced,
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ConfigureSurfaceError {
    #[error(transparent)]
    Device(#[from] DeviceError),
    #[error("Invalid surface")]
    InvalidSurface,
    #[error("The view format {0:?} is not compatible with texture format {1:?}, only changing srgb-ness is allowed.")]
    InvalidViewFormat(wgt::TextureFormat, wgt::TextureFormat),
    #[error(transparent)]
    MissingDownlevelFlags(#[from] MissingDownlevelFlags),
    #[error("`SurfaceOutput` must be dropped before a new `Surface` is made")]
    PreviousOutputExists,
    #[error("Both `Surface` width and height must be non-zero. Wait to recreate the `Surface` until the window has non-zero area.")]
    ZeroArea,
    #[error("`Surface` width and height must be within the maximum supported texture size. Requested was ({width}, {height}), maximum extent for either dimension is {max_texture_dimension_2d}.")]
    TooLarge {
        width: u32,
        height: u32,
        max_texture_dimension_2d: u32,
    },
    #[error("Surface does not support the adapter's queue family")]
    UnsupportedQueueFamily,
    #[error("Requested format {requested:?} is not in list of supported formats: {available:?}")]
    UnsupportedFormat {
        requested: wgt::TextureFormat,
        available: Vec<wgt::TextureFormat>,
    },
    #[error("Requested present mode {requested:?} is not in the list of supported present modes: {available:?}")]
    UnsupportedPresentMode {
        requested: wgt::PresentMode,
        available: Vec<wgt::PresentMode>,
    },
    #[error("Requested alpha mode {requested:?} is not in the list of supported alpha modes: {available:?}")]
    UnsupportedAlphaMode {
        requested: wgt::CompositeAlphaMode,
        available: Vec<wgt::CompositeAlphaMode>,
    },
    #[error("Requested usage is not supported")]
    UnsupportedUsage,
    #[error("Gpu got stuck :(")]
    StuckGpu,
}

impl From<WaitIdleError> for ConfigureSurfaceError {
    fn from(e: WaitIdleError) -> Self { todo!() }
}

#[repr(C)]
#[derive(Debug)]
pub struct SurfaceOutput {
    pub status: Status,
    pub texture_id: Option<id::TextureId>,
}

impl Global {
    pub fn surface_get_current_texture<A: HalApi>(
        &self,
        surface_id: id::SurfaceId,
        texture_id_in: Option<id::TextureId>,
    ) -> Result<SurfaceOutput, SurfaceError> { todo!() }

    pub fn surface_present<A: HalApi>(
        &self,
        surface_id: id::SurfaceId,
    ) -> Result<Status, SurfaceError> { todo!() }

    pub fn surface_texture_discard<A: HalApi>(
        &self,
        surface_id: id::SurfaceId,
    ) -> Result<(), SurfaceError> { todo!() }
}
