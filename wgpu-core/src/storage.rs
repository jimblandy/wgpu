use std::ops;
use std::sync::Arc;

use wgt::Backend;

use crate::id::Id;
use crate::resource::Resource;
use crate::{Epoch, Index};

/// An entry in a `Storage::map` table.
#[derive(Debug)]
pub(crate) enum Element<T> {
    /// There are no live ids with this index.
    Vacant,

    /// There is one live id with this index, allocated at the given
    /// epoch.
    Occupied(Arc<T>, Epoch),

    /// Like `Occupied`, but an error occurred when creating the
    /// resource.
    ///
    /// The given `String` is the resource's descriptor label.
    Error(Epoch, String),
}

#[derive(Clone, Debug)]
pub(crate) struct InvalidId;

/// A table of `T` values indexed by the id type `I`.
///
/// `Storage` implements [`std::ops::Index`], accepting `Id` values as
/// indices.
///
/// The table is represented as a vector indexed by the ids' index
/// values, so you should use an id allocator like `IdentityManager`
/// that keeps the index values dense and close to zero.
#[derive(Debug)]
pub(crate) struct Storage<T>
where
    T: Resource,
{
    pub(crate) map: Vec<Element<T>>,
    kind: &'static str,
}

impl<T> ops::Index<Id<T::Marker>> for Storage<T>
where
    T: Resource,
{
    type Output = Arc<T>;
    fn index(&self, id: Id<T::Marker>) -> &Arc<T> { todo!() }
}
impl<T> Storage<T>
where
    T: Resource,
{
    pub(crate) fn new() -> Self { todo!() }
}

impl<T> Storage<T>
where
    T: Resource,
{
    #[allow(dead_code)]
    pub(crate) fn contains(&self, id: Id<T::Marker>) -> bool { todo!() }

    /// Attempts to get a reference to an item behind a potentially invalid ID.
    ///
    /// Returns [`None`] if there is an epoch mismatch, or the entry is empty.
    ///
    /// This function is primarily intended for the `as_hal` family of functions
    /// where you may need to fallibly get a object backed by an id that could
    /// be in a different hub.
    pub(crate) fn try_get(&self, id: Id<T::Marker>) -> Result<Option<&Arc<T>>, InvalidId> { todo!() }

    /// Get a reference to an item behind a potentially invalid ID.
    /// Panics if there is an epoch mismatch, or the entry is empty.
    pub(crate) fn get(&self, id: Id<T::Marker>) -> Result<&Arc<T>, InvalidId> { todo!() }

    /// Get an owned reference to an item behind a potentially invalid ID.
    /// Panics if there is an epoch mismatch, or the entry is empty.
    pub(crate) fn get_owned(&self, id: Id<T::Marker>) -> Result<Arc<T>, InvalidId> { todo!() }

    pub(crate) fn label_for_invalid_id(&self, id: Id<T::Marker>) -> &str { todo!() }

    fn insert_impl(&mut self, index: usize, epoch: Epoch, element: Element<T>) { todo!() }

    pub(crate) fn insert(&mut self, id: Id<T::Marker>, value: Arc<T>) { todo!() }

    pub(crate) fn insert_error(&mut self, id: Id<T::Marker>, label: &str) { todo!() }

    pub(crate) fn replace_with_error(&mut self, id: Id<T::Marker>) -> Result<Arc<T>, InvalidId> { todo!() }

    pub(crate) fn force_replace(&mut self, id: Id<T::Marker>, value: T) { todo!() }

    pub(crate) fn remove(&mut self, id: Id<T::Marker>) -> Option<Arc<T>> { todo!() }

    pub(crate) fn iter(&self, backend: Backend) -> impl Iterator<Item = (Id<T::Marker>, &Arc<T>)> { std::iter::empty() }

    pub(crate) fn kind(&self) -> &str { todo!() }

    pub(crate) fn len(&self) -> usize { todo!() }
}
