/*! Stateless Trackers
 *
 * Stateless trackers don't have any state, so make no
 * distinction between a usage scope and a full tracker.
!*/

use std::sync::Arc;

use parking_lot::Mutex;

use crate::{id::Id, resource::Resource, resource_log, storage::Storage, track::ResourceMetadata};

use super::{ResourceTracker, TrackerIndex};

/// Satisfy clippy.
type Pair<T> = (Id<<T as Resource>::Marker>, Arc<T>);

/// Stores all the resources that a bind group stores.
#[derive(Debug)]
pub(crate) struct StatelessBindGroupSate<T: Resource> {
    resources: Mutex<Vec<Pair<T>>>,
}

impl<T: Resource> StatelessBindGroupSate<T> {
    pub fn new() -> Self { todo!() }

    /// Optimize the buffer bind group state by sorting it by ID.
    ///
    /// When this list of states is merged into a tracker, the memory
    /// accesses will be in a constant ascending order.
    pub(crate) fn optimize(&self) { todo!() }

    /// Returns a list of all resources tracked. May contain duplicates.
    pub fn used_resources(&self) -> impl Iterator<Item = Arc<T>> + '_ { std::iter::empty() }

    /// Returns a list of all resources tracked. May contain duplicates.
    pub fn drain_resources(&self) -> impl Iterator<Item = Arc<T>> + '_ { std::iter::empty() }

    /// Adds the given resource.
    pub fn add_single<'a>(&self, storage: &'a Storage<T>, id: Id<T::Marker>) -> Option<&'a T> { todo!() }
}

/// Stores all resource state within a command buffer or device.
#[derive(Debug)]
pub(crate) struct StatelessTracker<T: Resource> {
    metadata: ResourceMetadata<T>,
}

impl<T: Resource> ResourceTracker for StatelessTracker<T> {
    /// Try to remove the given resource from the tracker iff we have the last reference to the
    /// resource and the epoch matches.
    ///
    /// Returns true if the resource was removed or if not existing in metadata.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// false will be returned.
    fn remove_abandoned(&mut self, index: TrackerIndex) -> bool { todo!() }
}

impl<T: Resource> StatelessTracker<T> {
    pub fn new() -> Self { todo!() }

    fn tracker_assert_in_bounds(&self, index: usize) { todo!() }

    /// Sets the size of all the vectors inside the tracker.
    ///
    /// Must be called with the highest possible Resource ID of this type
    /// before all unsafe functions are called.
    pub fn set_size(&mut self, size: usize) { todo!() }

    /// Extend the vectors to let the given index be valid.
    fn allow_index(&mut self, index: usize) { todo!() }

    /// Returns a list of all resources tracked.
    pub fn used_resources(&self) -> impl Iterator<Item = Arc<T>> + '_ { std::iter::empty() }

    /// Returns a list of all resources tracked.
    pub fn drain_resources(&mut self) -> impl Iterator<Item = Arc<T>> + '_ { std::iter::empty() }

    /// Inserts a single resource into the resource tracker.
    ///
    /// If the resource already exists in the tracker, it will be overwritten.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn insert_single(&mut self, resource: Arc<T>) { todo!() }

    /// Adds the given resource to the tracker.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn add_single<'a>(
        &mut self,
        storage: &'a Storage<T>,
        id: Id<T::Marker>,
    ) -> Option<&'a Arc<T>> { todo!() }

    /// Adds the given resources from the given tracker.
    ///
    /// If the ID is higher than the length of internal vectors,
    /// the vectors will be extended. A call to set_size is not needed.
    pub fn add_from_tracker(&mut self, other: &Self) { todo!() }
}
