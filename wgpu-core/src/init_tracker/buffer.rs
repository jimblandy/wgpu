use super::{InitTracker, MemoryInitKind};
use crate::{hal_api::HalApi, resource::Buffer};
use std::{ops::Range, sync::Arc};

#[derive(Debug, Clone)]
#[allow(dead_code)] // JIMB
pub(crate) struct BufferInitTrackerAction<A: HalApi> {
    pub buffer: Arc<Buffer<A>>,
    pub range: Range<wgt::BufferAddress>,
    pub kind: MemoryInitKind,
}

pub(crate) type BufferInitTracker = InitTracker<wgt::BufferAddress>;
