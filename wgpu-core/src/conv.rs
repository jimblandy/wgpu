use wgt::TextureFormatFeatures;

use crate::resource::{self, TextureDescriptor};

#[cfg_attr(
    any(not(target_arch = "wasm32"), target_os = "emscripten"),
    allow(unused)
)]
pub fn is_valid_external_image_copy_dst_texture_format(format: wgt::TextureFormat) -> bool { todo!() }
