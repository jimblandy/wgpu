use parking_lot::Mutex;
use wgt::Backend;

use crate::{
    id::{Id, Marker},
    Epoch, Index,
};
use std::{fmt::Debug, marker::PhantomData};

#[derive(Copy, Clone, Debug, PartialEq)]
enum IdSource {
}

/// A simple structure to allocate [`Id`] identifiers.
///
/// Calling [`alloc`] returns a fresh, never-before-seen id. Calling [`free`]
/// marks an id as dead; it will never be returned again by `alloc`.
///
/// Use `IdentityManager::default` to construct new instances.
///
/// `IdentityManager` returns `Id`s whose index values are suitable for use as
/// indices into a `Storage<T>` that holds those ids' referents:
///
/// - Every live id has a distinct index value. Each live id's index selects a
///   distinct element in the vector.
///
/// - `IdentityManager` prefers low index numbers. If you size your vector to
///   accommodate the indices produced here, the vector's length will reflect
///   the highwater mark of actual occupancy.
///
/// - `IdentityManager` reuses the index values of freed ids before returning
///   ids with new index values. Freed vector entries get reused.
///
/// See the module-level documentation for an overview of how this
/// fits together.
///
/// [`Id`]: crate::id::Id
/// [`Backend`]: wgt::Backend;
/// [`alloc`]: IdentityManager::alloc
/// [`free`]: IdentityManager::free
#[derive(Debug)]
pub(super) struct IdentityValues {
}

#[derive(Debug)]
pub struct IdentityManager<T: Marker> {
    _phantom: PhantomData<T>,
}

