use std::{ops::Range, sync::Arc};

#[cfg(feature = "trace")]
use crate::device::trace::Command as TraceCommand;
use crate::{
    api_log,
    command::CommandBuffer,
    device::DeviceError,
    global::Global,
    hal_api::HalApi,
    init_tracker::{MemoryInitKind, TextureInitRange},
    resource::{Resource, Texture, TextureClearMode},
    snatch::SnatchGuard,
    track::{TextureSelector, TextureTracker},
};

use hal::CommandEncoder as _;
use thiserror::Error;
use wgt::{math::align_to, BufferAddress, BufferUsages, ImageSubresourceRange, TextureAspect};

/// Error encountered while attempting a clear.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ClearError {
}
