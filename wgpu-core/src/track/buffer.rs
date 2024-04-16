/*! Buffer Trackers
 *
 * Buffers are represented by a single state for the whole resource,
 * a 16 bit bitflag of buffer usages. Because there is only ever
 * one subresource, they have no selector.
!*/

use std::{borrow::Cow, marker::PhantomData, sync::Arc};

use super::{PendingTransition, ResourceTracker, TrackerIndex};
use crate::{
    hal_api::HalApi,
    id::BufferId,
    resource::{Buffer, Resource},
    snatch::SnatchGuard,
    storage::Storage,
    track::{
        invalid_resource_state, skip_barrier, ResourceMetadata, ResourceMetadataProvider,
        ResourceUses, UsageConflict,
    },
};
use hal::{BufferBarrier, BufferUses};
use parking_lot::Mutex;
use wgt::{strict_assert, strict_assert_eq};

impl ResourceUses for BufferUses {
    const EXCLUSIVE: Self = Self::EXCLUSIVE;

    type Selector = ();

    fn bits(self) -> u16 { todo!() }

    fn all_ordered(self) -> bool { todo!() }

    fn any_exclusive(self) -> bool { todo!() }
}

/// Stores all the buffers that a bind group stores.
#[derive(Debug)]
pub(crate) struct BufferBindGroupState<A: HalApi> {
    buffers: Mutex<Vec<(Arc<Buffer<A>>, BufferUses)>>,

    _phantom: PhantomData<A>,
}
impl<A: HalApi> BufferBindGroupState<A> {
    pub fn new() -> Self { todo!() }

    /// Optimize the buffer bind group state by sorting it by ID.
    ///
    /// When this list of states is merged into a tracker, the memory
    /// accesses will be in a constant ascending order.
    #[allow(clippy::pattern_type_mismatch)]
    pub(crate) fn optimize(&self) { todo!() }

    /// Returns a list of all buffers tracked. May contain duplicates.
    #[allow(clippy::pattern_type_mismatch)]
    pub fn used_tracker_indices(&self) -> impl Iterator<Item = TrackerIndex> + '_ { std::iter::empty() }

    /// Returns a list of all buffers tracked. May contain duplicates.
    pub fn drain_resources(&self) -> impl Iterator<Item = Arc<Buffer<A>>> + '_ { std::iter::empty() }

    /// Adds the given resource with the given state.
    pub fn add_single<'a>(
        &self,
        storage: &'a Storage<Buffer<A>>,
        id: BufferId,
        state: BufferUses,
    ) -> Option<&'a Arc<Buffer<A>>> { todo!() }
}

/// Stores all buffer state within a single usage scope.
#[derive(Debug)]
pub(crate) struct BufferUsageScope<A: HalApi> {
    state: Vec<BufferUses>,
    metadata: ResourceMetadata<Buffer<A>>,
}

impl<A: HalApi> Default for BufferUsageScope<A> {
    fn default() -> Self { todo!() }
}

impl<A: HalApi> BufferUsageScope<A> {
    fn tracker_assert_in_bounds(&self, index: usize) { todo!() }
    pub fn clear(&mut self) { todo!() }

    /// Sets the size of all the vectors inside the tracker.
    ///
    /// Must be called with the highest possible Buffer ID before
    /// all unsafe functions are called.
    pub fn set_size(&mut self, size: usize) { todo!() }

    /// Extend the vectors to let the given index be valid.
    fn allow_index(&mut self, index: usize) { todo!() }

    /// Drains all buffers tracked.
    pub fn drain_resources(&mut self) -> impl Iterator<Item = Arc<Buffer<A>>> + '_ { std::iter::empty() }

    /// Merge the list of buffer states in the given bind group into this usage scope.
    ///
    /// If any of the resulting states is invalid, stops the merge and returns a usage
    /// conflict with the details of the invalid state.
    ///
    /// Because bind groups do not check if the union of all their states is valid,
    /// this method is allowed to return Err on the first bind group bound.
    ///
    /// # Safety
    ///
    /// [`Self::set_size`] must be called with the maximum possible Buffer ID before this
    /// method is called.
    pub unsafe fn merge_bind_group(
        &mut self,
        bind_group: &BufferBindGroupState<A>,
    ) -> Result<(), UsageConflict> { todo!() }

    /// Merge the list of buffer states in the given usage scope into this UsageScope.
    ///
    /// If any of the resulting states is invalid, stops the merge and returns a usage
    /// conflict with the details of the invalid state.
    ///
    /// If the given tracker uses IDs higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn merge_usage_scope(&mut self, scope: &Self) -> Result<(), UsageConflict> { todo!() }

    /// Merge a single state into the UsageScope.
    ///
    /// If the resulting state is invalid, returns a usage
    /// conflict with the details of the invalid state.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn merge_single<'a>(
        &mut self,
        storage: &'a Storage<Buffer<A>>,
        id: BufferId,
        new_state: BufferUses,
    ) -> Result<&'a Arc<Buffer<A>>, UsageConflict> { todo!() }
}

pub(crate) type SetSingleResult<A> =
    Option<(Arc<Buffer<A>>, Option<PendingTransition<BufferUses>>)>;

/// Stores all buffer state within a command buffer or device.
pub(crate) struct BufferTracker<A: HalApi> {
    start: Vec<BufferUses>,
    end: Vec<BufferUses>,

    metadata: ResourceMetadata<Buffer<A>>,

    temp: Vec<PendingTransition<BufferUses>>,
}

impl<A: HalApi> ResourceTracker for BufferTracker<A> {
    /// Try to remove the buffer `id` from this tracker if it is otherwise unused.
    ///
    /// A buffer is 'otherwise unused' when the only references to it are:
    ///
    /// 1) the `Arc` that our caller, `LifetimeTracker::triage_suspected`, has just
    ///    drained from `LifetimeTracker::suspected_resources`,
    ///
    /// 2) its `Arc` in [`self.metadata`] (owned by [`Device::trackers`]), and
    ///
    /// 3) its `Arc` in the [`Hub::buffers`] registry.
    ///
    /// If the buffer is indeed unused, this function removes 2), and
    /// `triage_suspected` will remove 3), leaving 1) as the sole
    /// remaining reference.
    ///
    /// Returns true if the resource was removed or if not existing in metadata.
    ///
    /// [`Device::trackers`]: crate::device::Device
    /// [`self.metadata`]: BufferTracker::metadata
    /// [`Hub::buffers`]: crate::hub::Hub::buffers
    fn remove_abandoned(&mut self, index: TrackerIndex) -> bool { todo!() }
}

impl<A: HalApi> BufferTracker<A> {
    pub fn new() -> Self { todo!() }

    fn tracker_assert_in_bounds(&self, index: usize) { todo!() }

    /// Sets the size of all the vectors inside the tracker.
    ///
    /// Must be called with the highest possible Buffer ID before
    /// all unsafe functions are called.
    pub fn set_size(&mut self, size: usize) { todo!() }

    /// Extend the vectors to let the given index be valid.
    fn allow_index(&mut self, index: usize) { todo!() }

    /// Returns a list of all buffers tracked.
    pub fn used_resources(&self) -> impl Iterator<Item = Arc<Buffer<A>>> + '_ { std::iter::empty() }

    /// Drains all currently pending transitions.
    pub fn drain_transitions<'a, 'b: 'a>(
        &'b mut self,
        snatch_guard: &'a SnatchGuard<'a>,
    ) -> impl Iterator<Item = BufferBarrier<'a, A>> { std::iter::empty() }

    /// Inserts a single buffer and its state into the resource tracker.
    ///
    /// If the resource already exists in the tracker, this will panic.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn insert_single(&mut self, resource: Arc<Buffer<A>>, state: BufferUses) { todo!() }

    /// Sets the state of a single buffer.
    ///
    /// If a transition is needed to get the buffer into the given state, that transition
    /// is returned. No more than one transition is needed.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn set_single(&mut self, buffer: &Arc<Buffer<A>>, state: BufferUses) -> SetSingleResult<A> { todo!() }

    /// Sets the given state for all buffers in the given tracker.
    ///
    /// If a transition is needed to get the buffers into the needed state,
    /// those transitions are stored within the tracker. A subsequent
    /// call to [`Self::drain_transitions`] is needed to get those transitions.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn set_from_tracker(&mut self, tracker: &Self) { todo!() }

    /// Sets the given state for all buffers in the given UsageScope.
    ///
    /// If a transition is needed to get the buffers into the needed state,
    /// those transitions are stored within the tracker. A subsequent
    /// call to [`Self::drain_transitions`] is needed to get those transitions.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn set_from_usage_scope(&mut self, scope: &BufferUsageScope<A>) { todo!() }

    /// Iterates through all buffers in the given bind group and adopts
    /// the state given for those buffers in the UsageScope. It also
    /// removes all touched buffers from the usage scope.
    ///
    /// If a transition is needed to get the buffers into the needed state,
    /// those transitions are stored within the tracker. A subsequent
    /// call to [`Self::drain_transitions`] is needed to get those transitions.
    ///
    /// This is a really funky method used by Compute Passes to generate
    /// barriers after a call to dispatch without needing to iterate
    /// over all elements in the usage scope. We use each the
    /// a given iterator of ids as a source of which IDs to look at.
    /// All the IDs must have first been added to the usage scope.
    ///
    /// # Safety
    ///
    /// [`Self::set_size`] must be called with the maximum possible Buffer ID before this
    /// method is called.
    pub unsafe fn set_and_remove_from_usage_scope_sparse(
        &mut self,
        scope: &mut BufferUsageScope<A>,
        index_source: impl IntoIterator<Item = TrackerIndex>,
    ) { todo!() }

    #[allow(dead_code)]
    pub fn get(&self, index: TrackerIndex) -> Option<&Arc<Buffer<A>>> { todo!() }
}

/// Source of Buffer State.
#[derive(Debug, Clone)]
enum BufferStateProvider<'a> {
    /// Get a state that was provided directly.
    Direct { state: BufferUses },
    /// Get a state from an an array of states.
    Indirect { state: &'a [BufferUses] },
}
impl BufferStateProvider<'_> {
    /// Gets the state from the provider, given a resource ID index.
    ///
    /// # Safety
    ///
    /// Index must be in bounds for the indirect source iff this is in the indirect state.
    #[inline(always)]
    unsafe fn get_state(&self, index: usize) -> BufferUses { todo!() }
}

/// Does an insertion operation if the index isn't tracked
/// in the current metadata, otherwise merges the given state
/// with the current state. If the merging would cause
/// a conflict, returns that usage conflict.
///
/// # Safety
///
/// Indexes must be valid indexes into all arrays passed in
/// to this function, either directly or via metadata or provider structs.
#[inline(always)]
unsafe fn insert_or_merge<A: HalApi>(
    start_states: Option<&mut [BufferUses]>,
    current_states: &mut [BufferUses],
    resource_metadata: &mut ResourceMetadata<Buffer<A>>,
    index32: u32,
    index: usize,
    state_provider: BufferStateProvider<'_>,
    metadata_provider: ResourceMetadataProvider<'_, Buffer<A>>,
) -> Result<(), UsageConflict> { todo!() }

/// If the resource isn't tracked
/// - Inserts the given resource.
/// - Uses the `start_state_provider` to populate `start_states`
/// - Uses either `end_state_provider` or `start_state_provider`
///   to populate `current_states`.
/// If the resource is tracked
/// - Inserts barriers from the state in `current_states`
///   to the state provided by `start_state_provider`.
/// - Updates the `current_states` with either the state from
///   `end_state_provider` or `start_state_provider`.
///
/// Any barriers are added to the barrier vector.
///
/// # Safety
///
/// Indexes must be valid indexes into all arrays passed in
/// to this function, either directly or via metadata or provider structs.
#[inline(always)]
unsafe fn insert_or_barrier_update<A: HalApi>(
    start_states: Option<&mut [BufferUses]>,
    current_states: &mut [BufferUses],
    resource_metadata: &mut ResourceMetadata<Buffer<A>>,
    index: usize,
    start_state_provider: BufferStateProvider<'_>,
    end_state_provider: Option<BufferStateProvider<'_>>,
    metadata_provider: ResourceMetadataProvider<'_, Buffer<A>>,
    barriers: &mut Vec<PendingTransition<BufferUses>>,
) { todo!() }

#[inline(always)]
unsafe fn insert<A: HalApi>(
    start_states: Option<&mut [BufferUses]>,
    current_states: &mut [BufferUses],
    resource_metadata: &mut ResourceMetadata<Buffer<A>>,
    index: usize,
    start_state_provider: BufferStateProvider<'_>,
    end_state_provider: Option<BufferStateProvider<'_>>,
    metadata_provider: ResourceMetadataProvider<'_, Buffer<A>>,
) { todo!() }

#[inline(always)]
unsafe fn merge<A: HalApi>(
    current_states: &mut [BufferUses],
    index32: u32,
    index: usize,
    state_provider: BufferStateProvider<'_>,
    metadata_provider: ResourceMetadataProvider<'_, Buffer<A>>,
) -> Result<(), UsageConflict> { todo!() }

#[inline(always)]
unsafe fn barrier(
    current_states: &mut [BufferUses],
    index: usize,
    state_provider: BufferStateProvider<'_>,
    barriers: &mut Vec<PendingTransition<BufferUses>>,
) { todo!() }

#[inline(always)]
unsafe fn update(
    current_states: &mut [BufferUses],
    index: usize,
    state_provider: BufferStateProvider<'_>,
) { todo!() }
