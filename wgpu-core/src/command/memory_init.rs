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
    clear::clear_texture, ClearError,
};

/// Surface that was discarded by `StoreOp::Discard` of a preceding renderpass.
/// Any read access to this surface needs to be preceded by a texture initialization.
#[derive(Clone)]
pub(crate) struct TextureSurfaceDiscard<A: HalApi> {
    pub texture: Arc<Texture<A>>,
    pub mip_level: u32,
    pub layer: u32,
}

pub(crate) type SurfacesInDiscardState<A> = Vec<TextureSurfaceDiscard<A>>;

pub(crate) struct CommandBufferTextureMemoryActions<A: HalApi> {
    /// The tracker actions that we need to be executed before the command
    /// buffer is executed.
    init_actions: Vec<TextureInitTrackerAction<A>>,
    /// All the discards that haven't been followed by init again within the
    /// command buffer i.e. everything in this list resets the texture init
    /// state *after* the command buffer execution
    discards: Vec<TextureSurfaceDiscard<A>>,
}

impl<A: HalApi> Default for CommandBufferTextureMemoryActions<A> {
    fn default() -> Self { todo!() }
}

impl<A: HalApi> CommandBufferTextureMemoryActions<A> {
    pub(crate) fn drain_init_actions(&mut self) -> Drain<TextureInitTrackerAction<A>> { todo!() }

    pub(crate) fn discard(&mut self, discard: TextureSurfaceDiscard<A>) { todo!() }

    // Registers a TextureInitTrackerAction.
    // Returns previously discarded surface that need to be initialized *immediately* now.
    // Only returns a non-empty list if action is MemoryInitKind::NeedsInitializedMemory.
    #[must_use]
    pub(crate) fn register_init_action(
        &mut self,
        action: &TextureInitTrackerAction<A>,
    ) -> SurfacesInDiscardState<A> { todo!() }

    // Shortcut for register_init_action when it is known that the action is an
    // implicit init, not requiring any immediate resource init.
    pub(crate) fn register_implicit_init(
        &mut self,
        texture: &Arc<Texture<A>>,
        range: TextureInitRange,
    ) { todo!() }
}

// Utility function that takes discarded surfaces from (several calls to)
// register_init_action and initializes them on the spot.
//
// Takes care of barriers as well!
pub(crate) fn fixup_discarded_surfaces<
    A: HalApi,
    InitIter: Iterator<Item = TextureSurfaceDiscard<A>>,
>(
    inits: InitIter,
    encoder: &mut A::CommandEncoder,
    texture_tracker: &mut TextureTracker<A>,
    device: &Device<A>,
    snatch_guard: &SnatchGuard<'_>,
) { todo!() }
