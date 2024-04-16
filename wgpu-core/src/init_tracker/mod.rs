/*! Lazy initialization of texture and buffer memory.

The WebGPU specification requires all texture & buffer memory to be
zero initialized on first read. To avoid unnecessary inits, we track
the initialization status of every resource and perform inits lazily.

The granularity is different for buffers and textures:

- Buffer: Byte granularity to support usecases with large, partially
  bound buffers well.

- Texture: Mip-level per layer. That is, a 2D surface is either
  completely initialized or not, subrects are not tracked.

Every use of a buffer/texture generates a InitTrackerAction which are
recorded and later resolved at queue submit by merging them with the
current state and each other in execution order.

It is important to note that from the point of view of the memory init
system there are two kind of writes:

- **Full writes**: Any kind of memcpy operation. These cause a
  `MemoryInitKind.ImplicitlyInitialized` action.

- **(Potentially) partial writes**: For example, write use in a
  Shader. The system is not able to determine if a resource is fully
  initialized afterwards but is no longer allowed to perform any
  clears, therefore this leads to a
  `MemoryInitKind.ImplicitlyInitialized` action, exactly like a read
  would.

 */

use smallvec::SmallVec;
use std::{fmt, iter, ops::Range};

mod buffer;
mod texture;

pub(crate) use buffer::{BufferInitTracker, BufferInitTrackerAction};
pub(crate) use texture::{
    has_copy_partial_init_tracker_coverage, TextureInitRange, TextureInitTracker,
    TextureInitTrackerAction,
};

#[derive(Debug, Clone, Copy)]
pub(crate) enum MemoryInitKind {
    // The memory range is going to be written by an already initialized source,
    // thus doesn't need extra attention other than marking as initialized.
    ImplicitlyInitialized,
    // The memory range is going to be read, therefore needs to ensure prior
    // initialization.
    NeedsInitializedMemory,
}

// Most of the time a resource is either fully uninitialized (one element) or
// initialized (zero elements).
type UninitializedRangeVec<Idx> = SmallVec<[Range<Idx>; 1]>;

/// Tracks initialization status of a linear range from 0..size
#[derive(Debug, Clone)]
pub(crate) struct InitTracker<Idx: Ord + Copy + Default> {
    /// Non-overlapping list of all uninitialized ranges, sorted by
    /// range end.
    uninitialized_ranges: UninitializedRangeVec<Idx>,
}

pub(crate) struct InitTrackerDrain<'a, Idx: fmt::Debug + Ord + Copy> {
    uninitialized_ranges: &'a mut UninitializedRangeVec<Idx>,
    drain_range: Range<Idx>,
    first_index: usize,
    next_index: usize,
}

impl<'a, Idx> Iterator for InitTrackerDrain<'a, Idx>
where
    Idx: fmt::Debug + Ord + Copy,
{
    type Item = Range<Idx>;

    fn next(&mut self) -> Option<Self::Item> { todo!() }
}

impl<'a, Idx> Drop for InitTrackerDrain<'a, Idx>
where
    Idx: fmt::Debug + Ord + Copy,
{
    fn drop(&mut self) { todo!() }
}

impl<Idx> InitTracker<Idx>
where
    Idx: fmt::Debug + Ord + Copy + Default,
{
    pub(crate) fn new(size: Idx) -> Self { todo!() }

    /// Checks for uninitialized ranges within a given query range.
    ///
    /// If `query_range` includes any uninitialized portions of this init
    /// tracker's resource, return the smallest subrange of `query_range` that
    /// covers all uninitialized regions.
    ///
    /// The returned range may be larger than necessary, to keep this function
    /// O(log n).
    pub(crate) fn check(&self, query_range: Range<Idx>) -> Option<Range<Idx>> { todo!() }

    // Drains uninitialized ranges in a query range.
    pub(crate) fn drain(&mut self, drain_range: Range<Idx>) -> InitTrackerDrain<Idx> { todo!() }
}

impl InitTracker<u32> {
    // Makes a single entry uninitialized if not already uninitialized
    #[allow(dead_code)]
    pub(crate) fn discard(&mut self, pos: u32) { todo!() }
}

#[cfg(test)]
mod test {
    use std::ops::Range;

    type Tracker = super::InitTracker<u32>;

    #[test]
    fn check_for_newly_created_tracker() { todo!() }

    #[test]
    fn check_for_drained_tracker() { todo!() }

    #[test]
    fn check_for_partially_filled_tracker() { todo!() }

    #[test]
    fn drain_already_drained() { todo!() }

    #[test]
    fn drain_never_returns_ranges_twice_for_same_range() { todo!() }

    #[test]
    fn drain_splits_ranges_correctly() { todo!() }

    #[test]
    fn discard_adds_range_on_cleared() { todo!() }

    #[test]
    fn discard_does_nothing_on_uncleared() { todo!() }

    #[test]
    fn discard_extends_ranges() { todo!() }

    #[test]
    fn discard_merges_ranges() { todo!() }
}
