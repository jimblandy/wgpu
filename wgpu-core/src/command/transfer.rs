#[cfg(feature = "trace")]
use crate::device::trace::Command as TraceCommand;
use crate::{
    api_log,
    command::{CommandBuffer, CommandEncoderError},
    conv,
    device::{Device, DeviceError, MissingDownlevelFlags},
    error::{ErrorFormatter, PrettyError},
    global::Global,
    hal_api::HalApi,
    init_tracker::{
        MemoryInitKind, TextureInitRange,
        TextureInitTrackerAction,
    },
    resource::{Resource, Texture, TextureErrorDimension},
    snatch::SnatchGuard,
    track::{TextureSelector, Tracker},
};

use arrayvec::ArrayVec;
use hal::CommandEncoder as _;
use thiserror::Error;
use wgt::{BufferAddress, BufferUsages, Extent3d, TextureUsages};

use std::{iter, sync::Arc};

use super::{memory_init::CommandBufferTextureMemoryActions, ClearError};

pub type ImageCopyBuffer = wgt::ImageCopyBuffer<()>;
pub type ImageCopyTexture = wgt::ImageCopyTexture<()>;

#[derive(Clone, Copy, Debug)]
pub enum CopySide {
}

/// Error encountered while attempting a data transfer.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum TransferError {
}

impl PrettyError for TransferError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}
/// Error encountered while attempting to do a copy on a command encoder.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum CopyError {
    #[error(transparent)]
    Encoder(#[from] CommandEncoderError),
    #[error("Copy error")]
    Transfer(#[from] TransferError),
}

impl From<DeviceError> for CopyError {
    fn from(err: DeviceError) -> Self { todo!() }
}

