//Note: this could be the only place where we need `SmallVec`.
//TODO: consider getting rid of it.
use smallvec::SmallVec;

use std::{fmt::Debug, iter, ops::Range};

/// Structure that keeps track of a I -> T mapping,
/// optimized for a case where keys of the same values
/// are often grouped together linearly.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct RangedStates<I, T> {
    /// List of ranges, each associated with a singe value.
    /// Ranges of keys have to be non-intersecting and ordered.
    ranges: SmallVec<[(Range<I>, T); 1]>,
}

impl<I: Copy + Ord, T: Copy + PartialEq> RangedStates<I, T> {
    pub fn from_range(range: Range<I>, value: T) -> Self { todo!() }

    /// Construct a new instance from a slice of ranges.
    #[cfg(test)]
    pub fn from_slice(values: &[(Range<I>, T)]) -> Self { todo!() }

    pub fn iter(&self) -> impl Iterator<Item = &(Range<I>, T)> + Clone { std::iter::empty() }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut (Range<I>, T)> { std::iter::empty() }

    /// Check that all the ranges are non-intersecting and ordered.
    /// Panics otherwise.
    #[cfg(test)]
    fn check_sanity(&self) { todo!() }

    /// Merge the neighboring ranges together, where possible.
    pub fn coalesce(&mut self) { todo!() }

    pub fn iter_filter<'a>(
        &'a self,
        range: &'a Range<I>,
    ) -> impl Iterator<Item = (Range<I>, &T)> + 'a { std::iter::empty() }

    /// Split the storage ranges in such a way that there is a linear subset of
    /// them occupying exactly `index` range, which is returned mutably.
    ///
    /// Gaps in the ranges are filled with `default` value.
    pub fn isolate(&mut self, index: &Range<I>, default: T) -> &mut [(Range<I>, T)] { todo!() }

    /// Helper method for isolation that checks the sanity of the results.
    #[cfg(test)]
    pub fn sanely_isolated(&self, index: Range<I>, default: T) -> Vec<(Range<I>, T)> { todo!() }
}

#[cfg(test)]
mod test {
    //TODO: randomized/fuzzy testing
    use super::RangedStates;

    #[test]
    fn sane_good() { todo!() }

    #[test]
    #[should_panic]
    fn sane_empty() { todo!() }

    #[test]
    #[should_panic]
    fn sane_intersect() { todo!() }

    #[test]
    fn coalesce() { todo!() }

    #[test]
    fn isolate() { todo!() }
}
