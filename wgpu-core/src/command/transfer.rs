#[cfg(feature = "trace")]
use crate::device::trace::Command as TraceCommand;
use crate::{
    api_log,
    command::{clear_texture, CommandBuffer, CommandEncoderError},
    conv,
    device::{Device, DeviceError, MissingDownlevelFlags},
    error::{ErrorFormatter, PrettyError},
    global::Global,
    hal_api::HalApi,
    id::{BufferId, CommandEncoderId, DeviceId, TextureId},
    init_tracker::{
        has_copy_partial_init_tracker_coverage, MemoryInitKind, TextureInitRange,
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

use super::{memory_init::CommandBufferTextureMemoryActions, ClearError, CommandEncoder};

pub type ImageCopyBuffer = wgt::ImageCopyBuffer<BufferId>;
pub type ImageCopyTexture = wgt::ImageCopyTexture<TextureId>;
pub type ImageCopyTextureTagged = wgt::ImageCopyTextureTagged<TextureId>;

#[derive(Clone, Copy, Debug)]
pub enum CopySide {
    Source,
    Destination,
}

/// Error encountered while attempting a data transfer.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum TransferError {
    #[error("Device {0:?} is invalid")]
    InvalidDevice(DeviceId),
    #[error("Buffer {0:?} is invalid or destroyed")]
    InvalidBuffer(BufferId),
    #[error("Texture {0:?} is invalid or destroyed")]
    InvalidTexture(TextureId),
    #[error("Source and destination cannot be the same buffer")]
    SameSourceDestinationBuffer,
    #[error("Source buffer/texture is missing the `COPY_SRC` usage flag")]
    MissingCopySrcUsageFlag,
    #[error("Destination buffer/texture is missing the `COPY_DST` usage flag")]
    MissingCopyDstUsageFlag(Option<BufferId>, Option<TextureId>),
    #[error("Destination texture is missing the `RENDER_ATTACHMENT` usage flag")]
    MissingRenderAttachmentUsageFlag(TextureId),
    #[error("Copy of {start_offset}..{end_offset} would end up overrunning the bounds of the {side:?} buffer of size {buffer_size}")]
    BufferOverrun {
        start_offset: BufferAddress,
        end_offset: BufferAddress,
        buffer_size: BufferAddress,
        side: CopySide,
    },
    #[error("Copy of {dimension:?} {start_offset}..{end_offset} would end up overrunning the bounds of the {side:?} texture of {dimension:?} size {texture_size}")]
    TextureOverrun {
        start_offset: u32,
        end_offset: u32,
        texture_size: u32,
        dimension: TextureErrorDimension,
        side: CopySide,
    },
    #[error("Unable to select texture aspect {aspect:?} from format {format:?}")]
    InvalidTextureAspect {
        format: wgt::TextureFormat,
        aspect: wgt::TextureAspect,
    },
    #[error("Unable to select texture mip level {level} out of {total}")]
    InvalidTextureMipLevel { level: u32, total: u32 },
    #[error("Texture dimension must be 2D when copying from an external texture")]
    InvalidDimensionExternal(TextureId),
    #[error("Buffer offset {0} is not aligned to block size or `COPY_BUFFER_ALIGNMENT`")]
    UnalignedBufferOffset(BufferAddress),
    #[error("Copy size {0} does not respect `COPY_BUFFER_ALIGNMENT`")]
    UnalignedCopySize(BufferAddress),
    #[error("Copy width is not a multiple of block width")]
    UnalignedCopyWidth,
    #[error("Copy height is not a multiple of block height")]
    UnalignedCopyHeight,
    #[error("Copy origin's x component is not a multiple of block width")]
    UnalignedCopyOriginX,
    #[error("Copy origin's y component is not a multiple of block height")]
    UnalignedCopyOriginY,
    #[error("Bytes per row does not respect `COPY_BYTES_PER_ROW_ALIGNMENT`")]
    UnalignedBytesPerRow,
    #[error("Number of bytes per row needs to be specified since more than one row is copied")]
    UnspecifiedBytesPerRow,
    #[error("Number of rows per image needs to be specified since more than one image is copied")]
    UnspecifiedRowsPerImage,
    #[error("Number of bytes per row is less than the number of bytes in a complete row")]
    InvalidBytesPerRow,
    #[error("Image is 1D and the copy height and depth are not both set to 1")]
    InvalidCopySize,
    #[error("Number of rows per image is invalid")]
    InvalidRowsPerImage,
    #[error("Copy source aspects must refer to all aspects of the source texture format")]
    CopySrcMissingAspects,
    #[error(
        "Copy destination aspects must refer to all aspects of the destination texture format"
    )]
    CopyDstMissingAspects,
    #[error("Copy aspect must refer to a single aspect of texture format")]
    CopyAspectNotOne,
    #[error("Copying from textures with format {format:?} and aspect {aspect:?} is forbidden")]
    CopyFromForbiddenTextureFormat {
        format: wgt::TextureFormat,
        aspect: wgt::TextureAspect,
    },
    #[error("Copying to textures with format {format:?} and aspect {aspect:?} is forbidden")]
    CopyToForbiddenTextureFormat {
        format: wgt::TextureFormat,
        aspect: wgt::TextureAspect,
    },
    #[error(
        "Copying to textures with format {0:?} is forbidden when copying from external texture"
    )]
    ExternalCopyToForbiddenTextureFormat(wgt::TextureFormat),
    #[error("The entire texture must be copied when copying from depth texture")]
    InvalidDepthTextureExtent,
    #[error(
        "Source format ({src_format:?}) and destination format ({dst_format:?}) are not copy-compatible (they may only differ in srgb-ness)"
    )]
    TextureFormatsNotCopyCompatible {
        src_format: wgt::TextureFormat,
        dst_format: wgt::TextureFormat,
    },
    #[error(transparent)]
    MemoryInitFailure(#[from] ClearError),
    #[error("Cannot encode this copy because of a missing downelevel flag")]
    MissingDownlevelFlags(#[from] MissingDownlevelFlags),
    #[error("Source texture sample count must be 1, got {sample_count}")]
    InvalidSampleCount { sample_count: u32 },
    #[error("Requested mip level {requested} does no exist (count: {count})")]
    InvalidMipLevel { requested: u32, count: u32 },
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

pub(crate) fn extract_texture_selector<A: HalApi>(
    copy_texture: &ImageCopyTexture,
    copy_size: &Extent3d,
    texture: &Texture<A>,
) -> Result<(TextureSelector, hal::TextureCopyBase), TransferError> { todo!() }

/// WebGPU's [validating linear texture data][vltd] algorithm.
///
/// Copied with some modifications from WebGPU standard.
///
/// If successful, returns a pair `(bytes, stride)`, where:
/// - `bytes` is the number of buffer bytes required for this copy, and
/// - `stride` number of bytes between array layers.
///
/// [vltd]: https://gpuweb.github.io/gpuweb/#abstract-opdef-validating-linear-texture-data
pub(crate) fn validate_linear_texture_data(
    layout: &wgt::ImageDataLayout,
    format: wgt::TextureFormat,
    aspect: wgt::TextureAspect,
    buffer_size: BufferAddress,
    buffer_side: CopySide,
    copy_size: &Extent3d,
    need_copy_aligned_rows: bool,
) -> Result<(BufferAddress, BufferAddress), TransferError> { todo!() }

/// WebGPU's [validating texture copy range][vtcr] algorithm.
///
/// Copied with minor modifications from WebGPU standard.
///
/// Returns the HAL copy extent and the layer count.
///
/// [vtcr]: https://gpuweb.github.io/gpuweb/#validating-texture-copy-range
pub(crate) fn validate_texture_copy_range(
    texture_copy_view: &ImageCopyTexture,
    desc: &wgt::TextureDescriptor<(), Vec<wgt::TextureFormat>>,
    texture_side: CopySide,
    copy_size: &Extent3d,
) -> Result<(hal::CopyExtent, u32), TransferError> { todo!() }

fn handle_texture_init<A: HalApi>(
    init_kind: MemoryInitKind,
    encoder: &mut CommandEncoder<A>,
    trackers: &mut Tracker<A>,
    texture_memory_actions: &mut CommandBufferTextureMemoryActions<A>,
    device: &Device<A>,
    copy_texture: &ImageCopyTexture,
    copy_size: &Extent3d,
    texture: &Arc<Texture<A>>,
    snatch_guard: &SnatchGuard<'_>,
) -> Result<(), ClearError> { todo!() }

/// Prepare a transfer's source texture.
///
/// Ensure the source texture of a transfer is in the right initialization
/// state, and record the state for after the transfer operation.
fn handle_src_texture_init<A: HalApi>(
    encoder: &mut CommandEncoder<A>,
    trackers: &mut Tracker<A>,
    texture_memory_actions: &mut CommandBufferTextureMemoryActions<A>,
    device: &Device<A>,
    source: &ImageCopyTexture,
    copy_size: &Extent3d,
    texture: &Arc<Texture<A>>,
    snatch_guard: &SnatchGuard<'_>,
) -> Result<(), TransferError> { todo!() }

/// Prepare a transfer's destination texture.
///
/// Ensure the destination texture of a transfer is in the right initialization
/// state, and record the state for after the transfer operation.
fn handle_dst_texture_init<A: HalApi>(
    encoder: &mut CommandEncoder<A>,
    trackers: &mut Tracker<A>,
    texture_memory_actions: &mut CommandBufferTextureMemoryActions<A>,
    device: &Device<A>,
    destination: &ImageCopyTexture,
    copy_size: &Extent3d,
    texture: &Arc<Texture<A>>,
    snatch_guard: &SnatchGuard<'_>,
) -> Result<(), TransferError> { todo!() }

impl Global {
    pub fn command_encoder_copy_buffer_to_buffer<A: HalApi>(
        &self,
        command_encoder_id: CommandEncoderId,
        source: BufferId,
        source_offset: BufferAddress,
        destination: BufferId,
        destination_offset: BufferAddress,
        size: BufferAddress,
    ) -> Result<(), CopyError> { todo!() }

    pub fn command_encoder_copy_buffer_to_texture<A: HalApi>(
        &self,
        command_encoder_id: CommandEncoderId,
        source: &ImageCopyBuffer,
        destination: &ImageCopyTexture,
        copy_size: &Extent3d,
    ) -> Result<(), CopyError> { todo!() }

    pub fn command_encoder_copy_texture_to_buffer<A: HalApi>(
        &self,
        command_encoder_id: CommandEncoderId,
        source: &ImageCopyTexture,
        destination: &ImageCopyBuffer,
        copy_size: &Extent3d,
    ) -> Result<(), CopyError> { todo!() }

    pub fn command_encoder_copy_texture_to_texture<A: HalApi>(
        &self,
        command_encoder_id: CommandEncoderId,
        source: &ImageCopyTexture,
        destination: &ImageCopyTexture,
        copy_size: &Extent3d,
    ) -> Result<(), CopyError> { todo!() }
}
