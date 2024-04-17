use super::{InitTracker, MemoryInitKind};
use crate::{hal_api::HalApi, resource::Texture, track::TextureSelector};
use arrayvec::ArrayVec;
use std::{ops::Range, sync::Arc};

#[derive(Debug, Clone)]
pub(crate) struct TextureInitRange {
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // JIMB: 37s
pub(crate) struct TextureInitTrackerAction<A: HalApi> {
    pub(crate) texture: Arc<Texture<A>>,
    pub(crate) range: TextureInitRange,
    pub(crate) kind: MemoryInitKind,
}

#[derive(Debug)]
pub(crate) struct TextureInitTracker {
}
