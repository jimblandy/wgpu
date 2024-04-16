use super::{InitTracker, MemoryInitKind};
use crate::{hal_api::HalApi, resource::Texture, track::TextureSelector};
use arrayvec::ArrayVec;
use std::{ops::Range, sync::Arc};

#[derive(Debug, Clone)]
pub(crate) struct TextureInitRange {
    pub(crate) mip_range: Range<u32>,
    // Strictly array layers. We do *not* track volume slices separately.
    pub(crate) layer_range: Range<u32>,
}

// Returns true if a copy operation doesn't fully cover the texture init
// tracking granularity. I.e. if this function returns true for a pending copy
// operation, the target texture needs to be ensured to be initialized first!
pub(crate) fn has_copy_partial_init_tracker_coverage(
    copy_size: &wgt::Extent3d,
    mip_level: u32,
    desc: &wgt::TextureDescriptor<(), Vec<wgt::TextureFormat>>,
) -> bool { todo!() }

impl From<TextureSelector> for TextureInitRange {
    fn from(selector: TextureSelector) -> Self { todo!() }
}

#[derive(Debug, Clone)]
pub(crate) struct TextureInitTrackerAction<A: HalApi> {
    pub(crate) texture: Arc<Texture<A>>,
    pub(crate) range: TextureInitRange,
    pub(crate) kind: MemoryInitKind,
}

pub(crate) type TextureLayerInitTracker = InitTracker<u32>;

#[derive(Debug)]
pub(crate) struct TextureInitTracker {
    pub mips: ArrayVec<TextureLayerInitTracker, { hal::MAX_MIP_LEVELS as usize }>,
}

impl TextureInitTracker {
    pub(crate) fn new(mip_level_count: u32, depth_or_array_layers: u32) -> Self { todo!() }

    pub(crate) fn check_action<A: HalApi>(
        &self,
        action: &TextureInitTrackerAction<A>,
    ) -> Option<TextureInitTrackerAction<A>> { todo!() }

    pub(crate) fn discard(&mut self, mip_level: u32, layer: u32) { todo!() }
}
