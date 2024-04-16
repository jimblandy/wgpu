use std::{collections::hash_map::Entry, ops::Range, sync::Arc, vec::Drain};

use hal::CommandEncoder;

use crate::{
    device::Device,
    hal_api::HalApi,
    init_tracker::*,
    resource::{Resource, Texture},
    snatch::SnatchGuard,
    track::{TextureTracker, Tracker},
    FastHashMap,
};

use super::{
    ClearError,
};

/// Surface that was discarded by `StoreOp::Discard` of a preceding renderpass.
/// Any read access to this surface needs to be preceded by a texture initialization.
#[derive(Clone)]
pub(crate) struct TextureSurfaceDiscard<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}

pub(crate) struct CommandBufferTextureMemoryActions<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}

impl<A: HalApi> Default for CommandBufferTextureMemoryActions<A> {
    fn default() -> Self { todo!() }
}

impl<A: HalApi> CommandBufferTextureMemoryActions<A> {
}
