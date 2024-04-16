//! The `ResourceMetadata` type.

use crate::resource::Resource;
use bit_vec::BitVec;
use std::{borrow::Cow, mem, sync::Arc};
use wgt::strict_assert;

/// A set of resources, holding a `Arc<T>` and epoch for each member.
///
/// Testing for membership is fast, and iterating over members is
/// reasonably fast in practice. Storage consumption is proportional
/// to the largest id index of any member, not to the number of
/// members, but a bit vector tracks occupancy, so iteration touches
/// only occupied elements.
#[derive(Debug)]
pub(super) struct ResourceMetadata<T: Resource> {
    /// If the resource with index `i` is a member, `owned[i]` is `true`.
    owned: BitVec<usize>,

    /// A vector holding clones of members' `T`s.
    resources: Vec<Option<Arc<T>>>,
}

impl<T: Resource> ResourceMetadata<T> {
    pub(super) fn new() -> Self { todo!() }

    /// Returns the number of indices we can accommodate.
    pub(super) fn size(&self) -> usize { todo!() }

    pub(super) fn set_size(&mut self, size: usize) { todo!() }

    pub(super) fn clear(&mut self) { todo!() }

    /// Ensures a given index is in bounds for all arrays and does
    /// sanity checks of the presence of a refcount.
    ///
    /// In release mode this function is completely empty and is removed.
    #[cfg_attr(not(feature = "strict_asserts"), allow(unused_variables))]
    pub(super) fn tracker_assert_in_bounds(&self, index: usize) { todo!() }

    /// Returns true if the tracker owns no resources.
    ///
    /// This is a O(n) operation.
    pub(super) fn is_empty(&self) -> bool { todo!() }

    /// Returns true if the set contains the resource with the given index.
    pub(super) fn contains(&self, index: usize) -> bool { todo!() }

    /// Returns true if the set contains the resource with the given index.
    ///
    /// # Safety
    ///
    /// The given `index` must be in bounds for this `ResourceMetadata`'s
    /// existing tables. See `tracker_assert_in_bounds`.
    #[inline(always)]
    pub(super) unsafe fn contains_unchecked(&self, index: usize) -> bool { todo!() }

    /// Insert a resource into the set.
    ///
    /// Add the resource with the given index, epoch, and reference count to the
    /// set.
    ///
    /// # Safety
    ///
    /// The given `index` must be in bounds for this `ResourceMetadata`'s
    /// existing tables. See `tracker_assert_in_bounds`.
    #[inline(always)]
    pub(super) unsafe fn insert(&mut self, index: usize, resource: Arc<T>) { todo!() }

    /// Get the resource with the given index.
    ///
    /// # Safety
    ///
    /// The given `index` must be in bounds for this `ResourceMetadata`'s
    /// existing tables. See `tracker_assert_in_bounds`.
    #[inline(always)]
    pub(super) unsafe fn get_resource_unchecked(&self, index: usize) -> &Arc<T> { todo!() }

    /// Get the reference count of the resource with the given index.
    ///
    /// # Safety
    ///
    /// The given `index` must be in bounds for this `ResourceMetadata`'s
    /// existing tables. See `tracker_assert_in_bounds`.
    #[inline(always)]
    pub(super) unsafe fn get_ref_count_unchecked(&self, index: usize) -> usize { todo!() }

    /// Returns an iterator over the resources owned by `self`.
    pub(super) fn owned_resources(&self) -> impl Iterator<Item = Arc<T>> + '_ { std::iter::empty() }

    /// Returns an iterator over the resources owned by `self`.
    pub(super) fn drain_resources(&mut self) -> Vec<Arc<T>> { todo!() }

    /// Returns an iterator over the indices of all resources owned by `self`.
    pub(super) fn owned_indices(&self) -> impl Iterator<Item = usize> + '_ { std::iter::empty() }

    /// Remove the resource with the given index from the set.
    pub(super) unsafe fn remove(&mut self, index: usize) { todo!() }
}

/// A source of resource metadata.
///
/// This is used to abstract over the various places
/// trackers can get new resource metadata from.
pub(super) enum ResourceMetadataProvider<'a, T: Resource> {
    /// Comes directly from explicit values.
    Direct { resource: Cow<'a, Arc<T>> },
    /// Comes from another metadata tracker.
    Indirect { metadata: &'a ResourceMetadata<T> },
}
impl<T: Resource> ResourceMetadataProvider<'_, T> {
    /// Get the epoch and an owned refcount from this.
    ///
    /// # Safety
    ///
    /// - The index must be in bounds of the metadata tracker if this uses an indirect source.
    /// - info must be Some if this uses a Resource source.
    #[inline(always)]
    pub(super) unsafe fn get_own(self, index: usize) -> Arc<T> { todo!() }
}

/// Resizes the given bitvec to the given size. I'm not sure why this is hard to do but it is.
fn resize_bitvec<B: bit_vec::BitBlock>(vec: &mut BitVec<B>, size: usize) { todo!() }

/// Produces an iterator that yields the indexes of all bits that are set in the bitvec.
///
/// Will skip entire usize's worth of bits if they are all false.
fn iterate_bitvec_indices(ownership: &BitVec<usize>) -> impl Iterator<Item = usize> + '_ { std::iter::empty() }
