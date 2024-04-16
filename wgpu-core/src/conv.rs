use wgt::TextureFormatFeatures;

use crate::resource::{self, TextureDescriptor};

pub fn is_power_of_two_u16(val: u16) -> bool { todo!() }

pub fn is_power_of_two_u32(val: u32) -> bool { todo!() }

pub fn is_valid_copy_src_texture_format(
    format: wgt::TextureFormat,
    aspect: wgt::TextureAspect,
) -> bool { todo!() }

pub fn is_valid_copy_dst_texture_format(
    format: wgt::TextureFormat,
    aspect: wgt::TextureAspect,
) -> bool { todo!() }

#[cfg_attr(
    any(not(target_arch = "wasm32"), target_os = "emscripten"),
    allow(unused)
)]
pub fn is_valid_external_image_copy_dst_texture_format(format: wgt::TextureFormat) -> bool { todo!() }

pub fn map_buffer_usage(usage: wgt::BufferUsages) -> hal::BufferUses { todo!() }

pub fn map_texture_usage(
    usage: wgt::TextureUsages,
    aspect: hal::FormatAspects,
) -> hal::TextureUses { todo!() }

pub fn map_texture_usage_for_texture(
    desc: &TextureDescriptor,
    format_features: &TextureFormatFeatures,
) -> hal::TextureUses { todo!() }

pub fn map_texture_usage_from_hal(uses: hal::TextureUses) -> wgt::TextureUsages { todo!() }

pub fn check_texture_dimension_size(
    dimension: wgt::TextureDimension,
    wgt::Extent3d {
        width,
        height,
        depth_or_array_layers,
    }: wgt::Extent3d,
    sample_size: u32,
    limits: &wgt::Limits,
) -> Result<(), resource::TextureDimensionError> {
    use resource::{TextureDimensionError as Tde, TextureErrorDimension as Ted};
    use wgt::TextureDimension::*;

    let (extent_limits, sample_limit) = match dimension {
        D1 => ([limits.max_texture_dimension_1d, 1, 1], 1),
        D2 => (
            [
                limits.max_texture_dimension_2d,
                limits.max_texture_dimension_2d,
                limits.max_texture_array_layers,
            ],
            32,
        ),
        D3 => (
            [
                limits.max_texture_dimension_3d,
                limits.max_texture_dimension_3d,
                limits.max_texture_dimension_3d,
            ],
            1,
        ),
    };

    for (&dim, (&given, &limit)) in [Ted::X, Ted::Y, Ted::Z].iter().zip(
        [width, height, depth_or_array_layers]
            .iter()
            .zip(extent_limits.iter()),
    ) {
        if given == 0 {
            return Err(Tde::Zero(dim));
        }
        if given > limit {
            return Err(Tde::LimitExceeded { dim, given, limit });
        }
    }
    if sample_size == 0 || sample_size > sample_limit || !is_power_of_two_u32(sample_size) {
        return Err(Tde::InvalidSampleCount(sample_size));
    }

    Ok(())
}

pub fn bind_group_layout_flags(features: wgt::Features) -> hal::BindGroupLayoutFlags { todo!() }
