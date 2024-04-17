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

#[derive(Debug)]
pub(crate) struct Presentation {
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum SurfaceError {
}

#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ConfigureSurfaceError {
}

impl From<WaitIdleError> for ConfigureSurfaceError {
    fn from(e: WaitIdleError) -> Self { todo!() }
}

#[repr(C)]
#[derive(Debug)]
pub struct SurfaceOutput {
    pub status: Status,
    pub texture_id: Option<()>,
}
