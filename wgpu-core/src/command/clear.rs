use std::{ops::Range, sync::Arc};

#[cfg(feature = "trace")]
use crate::device::trace::Command as TraceCommand;
use crate::{
    api_log,
    command::CommandBuffer,
    device::DeviceError,
    global::Global,
    hal_api::HalApi,
    id::{BufferId, CommandEncoderId, DeviceId, TextureId},
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
    #[error("To use clear_texture the CLEAR_TEXTURE feature needs to be enabled")]
    MissingClearTextureFeature,
    #[error("Command encoder {0:?} is invalid")]
    InvalidCommandEncoder(CommandEncoderId),
    #[error("Device {0:?} is invalid")]
    InvalidDevice(DeviceId),
    #[error("Buffer {0:?} is invalid or destroyed")]
    InvalidBuffer(BufferId),
    #[error("Texture {0:?} is invalid or destroyed")]
    InvalidTexture(TextureId),
    #[error("Texture {0:?} can not be cleared")]
    NoValidTextureClearMode(TextureId),
    #[error("Buffer clear size {0:?} is not a multiple of `COPY_BUFFER_ALIGNMENT`")]
    UnalignedFillSize(BufferAddress),
    #[error("Buffer offset {0:?} is not a multiple of `COPY_BUFFER_ALIGNMENT`")]
    UnalignedBufferOffset(BufferAddress),
    #[error("Clear starts at offset {start_offset} with size of {requested_size}, but these added together exceed `u64::MAX`")]
    OffsetPlusSizeExceeds64BitBounds {
        start_offset: BufferAddress,
        requested_size: BufferAddress,
    },
    #[error("Clear of {start_offset}..{end_offset} would end up overrunning the bounds of the buffer of size {buffer_size}")]
    BufferOverrun {
        start_offset: BufferAddress,
        end_offset: BufferAddress,
        buffer_size: BufferAddress,
    },
    #[error("Destination buffer is missing the `COPY_DST` usage flag")]
    MissingCopyDstUsageFlag(Option<BufferId>, Option<TextureId>),
    #[error("Texture lacks the aspects that were specified in the image subresource range. Texture with format {texture_format:?}, specified was {subresource_range_aspects:?}")]
    MissingTextureAspect {
        texture_format: wgt::TextureFormat,
        subresource_range_aspects: TextureAspect,
    },
    #[error("Image subresource level range is outside of the texture's level range. texture range is {texture_level_range:?},  \
whereas subesource range specified start {subresource_base_mip_level} and count {subresource_mip_level_count:?}")]
    InvalidTextureLevelRange {
        texture_level_range: Range<u32>,
        subresource_base_mip_level: u32,
        subresource_mip_level_count: Option<u32>,
    },
    #[error("Image subresource layer range is outside of the texture's layer range. texture range is {texture_layer_range:?},  \
whereas subesource range specified start {subresource_base_array_layer} and count {subresource_array_layer_count:?}")]
    InvalidTextureLayerRange {
        texture_layer_range: Range<u32>,
        subresource_base_array_layer: u32,
        subresource_array_layer_count: Option<u32>,
    },
    #[error(transparent)]
    Device(#[from] DeviceError),
}

impl Global {
    pub fn command_encoder_clear_buffer<A: HalApi>(
        &self,
        command_encoder_id: CommandEncoderId,
        dst: BufferId,
        offset: BufferAddress,
        size: Option<BufferAddress>,
    ) -> Result<(), ClearError> { todo!() }

    pub fn command_encoder_clear_texture<A: HalApi>(
        &self,
        command_encoder_id: CommandEncoderId,
        dst: TextureId,
        subresource_range: &ImageSubresourceRange,
    ) -> Result<(), ClearError> { todo!() }
}

pub(crate) fn clear_texture<A: HalApi>(
    dst_texture: &Arc<Texture<A>>,
    range: TextureInitRange,
    encoder: &mut A::CommandEncoder,
    texture_tracker: &mut TextureTracker<A>,
    alignments: &hal::Alignments,
    zero_buffer: &A::Buffer,
    snatch_guard: &SnatchGuard<'_>,
) -> Result<(), ClearError> { todo!() }

fn clear_texture_via_buffer_copies<A: HalApi>(
    texture_desc: &wgt::TextureDescriptor<(), Vec<wgt::TextureFormat>>,
    alignments: &hal::Alignments,
    zero_buffer: &A::Buffer, // Buffer of size device::ZERO_BUFFER_SIZE
    range: TextureInitRange,
    encoder: &mut A::CommandEncoder,
    dst_raw: &A::Texture,
) { todo!() }

fn clear_texture_via_render_passes<A: HalApi>(
    dst_texture: &Texture<A>,
    range: TextureInitRange,
    is_color: bool,
    encoder: &mut A::CommandEncoder,
) -> Result<(), ClearError> { todo!() }
