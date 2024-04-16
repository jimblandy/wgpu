/*! Texture Trackers
 *
 * Texture trackers are significantly more complicated than
 * the buffer trackers because textures can be in a "complex"
 * state where each individual subresource can potentially be
 * in a different state from every other subtresource. These
 * complex states are stored separately from the simple states
 * because they are signifignatly more difficult to track and
 * most resources spend the vast majority of their lives in
 * simple states.
 *
 * There are two special texture usages: `UNKNOWN` and `UNINITIALIZED`.
 * - `UNKNOWN` is only used in complex states and is used to signify
 *   that the complex state does not know anything about those subresources.
 *   It cannot leak into transitions, it is invalid to transition into UNKNOWN
 *   state.
 * - `UNINITIALIZED` is used in both simple and complex states to mean the texture
 *   is known to be in some undefined state. Any transition away from UNINITIALIZED
 *   will treat the contents as junk.
!*/

use super::{
    range::RangedStates, PendingTransition, PendingTransitionList, ResourceTracker, TrackerIndex,
};
use crate::{
    hal_api::HalApi,
    resource::{Resource, Texture, TextureInner},
    snatch::SnatchGuard,
    track::{
        invalid_resource_state, skip_barrier, ResourceMetadata, ResourceMetadataProvider,
        ResourceUses, UsageConflict,
    },
};
use hal::TextureUses;

use arrayvec::ArrayVec;
use naga::FastHashMap;

use parking_lot::Mutex;
use wgt::{strict_assert, strict_assert_eq};

use std::{borrow::Cow, iter, marker::PhantomData, ops::Range, sync::Arc, vec::Drain};

/// Specifies a particular set of subresources in a texture.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextureSelector {
    pub mips: Range<u32>,
    pub layers: Range<u32>,
}

impl ResourceUses for TextureUses {
    const EXCLUSIVE: Self = Self::EXCLUSIVE;

    type Selector = TextureSelector;

    fn bits(self) -> u16 { todo!() }

    fn all_ordered(self) -> bool { todo!() }

    fn any_exclusive(self) -> bool { todo!() }
}

/// Represents the complex state of textures where every subresource is potentially
/// in a different state.
#[derive(Clone, Debug, Default, PartialEq)]
struct ComplexTextureState {
    mips: ArrayVec<RangedStates<u32, TextureUses>, { hal::MAX_MIP_LEVELS as usize }>,
}

impl ComplexTextureState {
    /// Creates complex texture state for the given sizes.
    ///
    /// This state will be initialized with the UNKNOWN state, a special state
    /// which means the trakcer knows nothing about the state.
    fn new(mip_level_count: u32, array_layer_count: u32) -> Self { todo!() }

    /// Initialize a complex state from a selector representing the full size of the texture
    /// and an iterator of a selector and a texture use, specifying a usage for a specific
    /// set of subresources.
    ///
    /// [`Self::to_selector_state_iter`] can be used to create such an iterator.
    ///
    /// # Safety
    ///
    /// All selectors in the iterator must be inside of the full_range selector.
    ///
    /// The full range selector must have mips and layers start at 0.
    unsafe fn from_selector_state_iter(
        full_range: TextureSelector,
        state_iter: impl Iterator<Item = (TextureSelector, TextureUses)>,
    ) -> Self { todo!() }

    /// Convert a complex state into an iterator over all states stored.
    ///
    /// [`Self::from_selector_state_iter`] can be used to consume such an iterator.
    fn to_selector_state_iter(
        &self,
    ) -> impl Iterator<Item = (TextureSelector, TextureUses)> + Clone + '_ { std::iter::empty() }
}

#[derive(Debug)]
struct TextureBindGroupStateData<A: HalApi> {
    selector: Option<TextureSelector>,
    texture: Arc<Texture<A>>,
    usage: TextureUses,
}

/// Stores all the textures that a bind group stores.
#[derive(Debug)]
pub(crate) struct TextureBindGroupState<A: HalApi> {
    textures: Mutex<Vec<TextureBindGroupStateData<A>>>,
}
impl<A: HalApi> TextureBindGroupState<A> {
    pub fn new() -> Self { todo!() }

    /// Optimize the texture bind group state by sorting it by ID.
    ///
    /// When this list of states is merged into a tracker, the memory
    /// accesses will be in a constant ascending order.
    pub(crate) fn optimize(&self) { todo!() }

    /// Returns a list of all textures tracked. May contain duplicates.
    pub fn drain_resources(&self) -> impl Iterator<Item = Arc<Texture<A>>> + '_ { std::iter::empty() }

    /// Adds the given resource with the given state.
    pub fn add_single<'a>(
        &self,
        texture: &'a Arc<Texture<A>>,
        selector: Option<TextureSelector>,
        state: TextureUses,
    ) -> Option<&'a Arc<Texture<A>>> { todo!() }
}

/// Container for corresponding simple and complex texture states.
#[derive(Debug)]
pub(crate) struct TextureStateSet {
    simple: Vec<TextureUses>,
    complex: FastHashMap<usize, ComplexTextureState>,
}

impl TextureStateSet {
    fn new() -> Self { todo!() }

    fn clear(&mut self) { todo!() }

    fn set_size(&mut self, size: usize) { todo!() }
}

/// Stores all texture state within a single usage scope.
#[derive(Debug)]
pub(crate) struct TextureUsageScope<A: HalApi> {
    set: TextureStateSet,
    metadata: ResourceMetadata<Texture<A>>,
}

impl<A: HalApi> Default for TextureUsageScope<A> {
    fn default() -> Self { todo!() }
}

impl<A: HalApi> TextureUsageScope<A> {
    fn tracker_assert_in_bounds(&self, index: usize) { todo!() }

    pub fn clear(&mut self) { todo!() }

    /// Sets the size of all the vectors inside the tracker.
    ///
    /// Must be called with the highest possible Texture ID before
    /// all unsafe functions are called.
    pub fn set_size(&mut self, size: usize) { todo!() }

    /// Drains all textures tracked.
    pub(crate) fn drain_resources(&mut self) -> impl Iterator<Item = Arc<Texture<A>>> + '_ { std::iter::empty() }

    /// Returns true if the tracker owns no resources.
    ///
    /// This is a O(n) operation.
    pub(crate) fn is_empty(&self) -> bool { todo!() }

    /// Merge the list of texture states in the given usage scope into this UsageScope.
    ///
    /// If any of the resulting states is invalid, stops the merge and returns a usage
    /// conflict with the details of the invalid state.
    ///
    /// If the given tracker uses IDs higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn merge_usage_scope(&mut self, scope: &Self) -> Result<(), UsageConflict> { todo!() }

    /// Merge the list of texture states in the given bind group into this usage scope.
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
        bind_group: &TextureBindGroupState<A>,
    ) -> Result<(), UsageConflict> { todo!() }

    /// Merge a single state into the UsageScope.
    ///
    /// If the resulting state is invalid, returns a usage
    /// conflict with the details of the invalid state.
    ///
    /// # Safety
    ///
    /// Unlike other trackers whose merge_single is safe, this method is only
    /// called where there is already other unsafe tracking functions active,
    /// so we can prove this unsafe "for free".
    ///
    /// [`Self::set_size`] must be called with the maximum possible Buffer ID before this
    /// method is called.
    pub unsafe fn merge_single(
        &mut self,
        texture: &Arc<Texture<A>>,
        selector: Option<TextureSelector>,
        new_state: TextureUses,
    ) -> Result<(), UsageConflict> { todo!() }
}

/// Stores all texture state within a command buffer or device.
pub(crate) struct TextureTracker<A: HalApi> {
    start_set: TextureStateSet,
    end_set: TextureStateSet,

    metadata: ResourceMetadata<Texture<A>>,

    temp: Vec<PendingTransition<TextureUses>>,

    _phantom: PhantomData<A>,
}

impl<A: HalApi> ResourceTracker for TextureTracker<A> {
    /// Try to remove the given resource from the tracker iff we have the last reference to the
    /// resource and the epoch matches.
    ///
    /// Returns true if the resource was removed or if not existing in metadata.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// false will be returned.
    fn remove_abandoned(&mut self, index: TrackerIndex) -> bool { todo!() }
}

impl<A: HalApi> TextureTracker<A> {
    pub fn new() -> Self { todo!() }

    fn tracker_assert_in_bounds(&self, index: usize) { todo!() }

    /// Sets the size of all the vectors inside the tracker.
    ///
    /// Must be called with the highest possible Texture ID before
    /// all unsafe functions are called.
    pub fn set_size(&mut self, size: usize) { todo!() }

    /// Extend the vectors to let the given index be valid.
    fn allow_index(&mut self, index: usize) { todo!() }

    /// Returns a list of all textures tracked.
    pub fn used_resources(&self) -> impl Iterator<Item = Arc<Texture<A>>> + '_ { std::iter::empty() }

    /// Drain all currently pending transitions.
    pub fn drain_transitions<'a>(
        &'a mut self,
        snatch_guard: &'a SnatchGuard<'a>,
    ) -> (PendingTransitionList, Vec<Option<&'a TextureInner<A>>>) { todo!() }

    /// Inserts a single texture and a state into the resource tracker.
    ///
    /// If the resource already exists in the tracker, this will panic.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn insert_single(&mut self, resource: Arc<Texture<A>>, usage: TextureUses) { todo!() }

    /// Sets the state of a single texture.
    ///
    /// If a transition is needed to get the texture into the given state, that transition
    /// is returned.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn set_single(
        &mut self,
        texture: &Arc<Texture<A>>,
        selector: TextureSelector,
        new_state: TextureUses,
    ) -> Option<Drain<'_, PendingTransition<TextureUses>>> { todo!() }

    /// Sets the given state for all texture in the given tracker.
    ///
    /// If a transition is needed to get the texture into the needed state,
    /// those transitions are stored within the tracker. A subsequent
    /// call to [`Self::drain_transitions`] is needed to get those transitions.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn set_from_tracker(&mut self, tracker: &Self) { todo!() }

    /// Sets the given state for all textures in the given UsageScope.
    ///
    /// If a transition is needed to get the textures into the needed state,
    /// those transitions are stored within the tracker. A subsequent
    /// call to [`Self::drain_transitions`] is needed to get those transitions.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn set_from_usage_scope(&mut self, scope: &TextureUsageScope<A>) { todo!() }

    /// Iterates through all textures in the given bind group and adopts
    /// the state given for those textures in the UsageScope. It also
    /// removes all touched textures from the usage scope.
    ///
    /// If a transition is needed to get the textures into the needed state,
    /// those transitions are stored within the tracker. A subsequent
    /// call to [`Self::drain_transitions`] is needed to get those transitions.
    ///
    /// This is a really funky method used by Compute Passes to generate
    /// barriers after a call to dispatch without needing to iterate
    /// over all elements in the usage scope. We use each the
    /// bind group as a source of which IDs to look at. The bind groups
    /// must have first been added to the usage scope.
    ///
    /// # Safety
    ///
    /// [`Self::set_size`] must be called with the maximum possible Buffer ID before this
    /// method is called.
    pub unsafe fn set_and_remove_from_usage_scope_sparse(
        &mut self,
        scope: &mut TextureUsageScope<A>,
        bind_group_state: &TextureBindGroupState<A>,
    ) { todo!() }

    /// Unconditionally removes the given resource from the tracker.
    ///
    /// Returns true if the resource was removed.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// false will be returned.
    pub fn remove(&mut self, index: TrackerIndex) -> bool { todo!() }
}

/// An iterator adapter that can store two different iterator types.
#[derive(Clone)]
enum EitherIter<L, R> {
    Left(L),
    Right(R),
}

impl<L, R, D> Iterator for EitherIter<L, R>
where
    L: Iterator<Item = D>,
    R: Iterator<Item = D>,
{
    type Item = D;

    fn next(&mut self) -> Option<Self::Item> { todo!() }
}

/// Container that signifies storing both different things
/// if there is a single state or many different states
/// involved in the operation.
#[derive(Debug, Clone)]
enum SingleOrManyStates<S, M> {
    Single(S),
    Many(M),
}

/// A source of texture state.
#[derive(Clone)]
enum TextureStateProvider<'a> {
    /// Comes directly from a single state.
    KnownSingle { state: TextureUses },
    /// Comes from a selector and a single state.
    Selector {
        selector: TextureSelector,
        state: TextureUses,
    },
    /// Comes from another texture set.
    TextureSet { set: &'a TextureStateSet },
}
impl<'a> TextureStateProvider<'a> {
    /// Convenience function turning `Option<Selector>` into this enum.
    fn from_option(selector: Option<TextureSelector>, state: TextureUses) -> Self { todo!() }

    /// Get the state provided by this.
    ///
    /// # Panics
    ///
    /// Panics if texture_selector is None and this uses a Selector source.
    ///
    /// # Safety
    ///
    /// - The index must be in bounds of the state set if this uses an TextureSet source.
    #[inline(always)]
    unsafe fn get_state(
        self,
        texture_selector: Option<&TextureSelector>,
        index: usize,
    ) -> SingleOrManyStates<
        TextureUses,
        impl Iterator<Item = (TextureSelector, TextureUses)> + Clone + 'a,
    > { SingleOrManyStates::Many(std::iter::empty()) }
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
    texture_selector: &TextureSelector,
    current_state_set: &mut TextureStateSet,
    resource_metadata: &mut ResourceMetadata<Texture<A>>,
    index: usize,
    state_provider: TextureStateProvider<'_>,
    metadata_provider: ResourceMetadataProvider<'_, Texture<A>>,
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
    texture_selector: &TextureSelector,
    start_state: Option<&mut TextureStateSet>,
    current_state_set: &mut TextureStateSet,
    resource_metadata: &mut ResourceMetadata<Texture<A>>,
    index: usize,
    start_state_provider: TextureStateProvider<'_>,
    end_state_provider: Option<TextureStateProvider<'_>>,
    metadata_provider: ResourceMetadataProvider<'_, Texture<A>>,
    barriers: &mut Vec<PendingTransition<TextureUses>>,
) { todo!() }

#[inline(always)]
unsafe fn insert<A: HalApi>(
    texture_selector: Option<&TextureSelector>,
    start_state: Option<&mut TextureStateSet>,
    end_state: &mut TextureStateSet,
    resource_metadata: &mut ResourceMetadata<Texture<A>>,
    index: usize,
    start_state_provider: TextureStateProvider<'_>,
    end_state_provider: Option<TextureStateProvider<'_>>,
    metadata_provider: ResourceMetadataProvider<'_, Texture<A>>,
) { todo!() }

#[inline(always)]
unsafe fn merge<A: HalApi>(
    texture_selector: &TextureSelector,
    current_state_set: &mut TextureStateSet,
    index: usize,
    state_provider: TextureStateProvider<'_>,
    metadata_provider: ResourceMetadataProvider<'_, Texture<A>>,
) -> Result<(), UsageConflict> { todo!() }

#[inline(always)]
unsafe fn barrier(
    texture_selector: &TextureSelector,
    current_state_set: &TextureStateSet,
    index: usize,
    state_provider: TextureStateProvider<'_>,
    barriers: &mut Vec<PendingTransition<TextureUses>>,
) { todo!() }

#[allow(clippy::needless_option_as_deref)] // we use this for reborrowing Option<&mut T>
#[inline(always)]
unsafe fn update(
    texture_selector: &TextureSelector,
    start_state_set: &mut TextureStateSet,
    current_state_set: &mut TextureStateSet,
    index: usize,
    state_provider: TextureStateProvider<'_>,
) { todo!() }
