use std::sync::Arc;

use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use wgt::Backend;

use crate::{
    id::Id,
    identity::IdentityManager,
    resource::Resource,
    storage::{Element, InvalidId, Storage},
};

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct RegistryReport {
    pub num_allocated: usize,
    pub num_kept_from_user: usize,
    pub num_released_from_user: usize,
    pub num_error: usize,
    pub element_size: usize,
}

impl RegistryReport {
    pub fn is_empty(&self) -> bool { todo!() }
}

/// Registry is the primary holder of each resource type
/// Every resource is now arcanized so the last arc released
/// will in the end free the memory and release the inner raw resource
///
/// Registry act as the main entry point to keep resource alive
/// when created and released from user land code
///
/// A resource may still be alive when released from user land code
/// if it's used in active submission or anyway kept alive from
/// any other dependent resource
///
#[derive(Debug)]
pub(crate) struct Registry<T: Resource> {
    identity: Arc<IdentityManager<T::Marker>>,
    storage: RwLock<Storage<T>>,
    backend: Backend,
}

impl<T: Resource> Registry<T> {
    pub(crate) fn new(backend: Backend) -> Self { todo!() }

    pub(crate) fn without_backend() -> Self { todo!() }
}

#[must_use]
pub(crate) struct FutureId<'a, T: Resource> {
    id: Id<T::Marker>,
    data: &'a RwLock<Storage<T>>,
}

impl<T: Resource> FutureId<'_, T> {
    #[allow(dead_code)]
    pub fn id(&self) -> Id<T::Marker> { todo!() }

    pub fn into_id(self) -> Id<T::Marker> { todo!() }

    pub fn init(&self, mut value: T) -> Arc<T> { todo!() }

    pub fn init_in_place(&self, mut value: Arc<T>) -> Arc<T> { todo!() }

    /// Assign a new resource to this ID.
    ///
    /// Registers it with the registry, and fills out the resource info.
    pub fn assign(self, value: Arc<T>) -> (Id<T::Marker>, Arc<T>) { todo!() }

    /// Assign an existing resource to a new ID.
    ///
    /// Registers it with the registry.
    ///
    /// This _will_ leak the ID, and it will not be recycled again.
    /// See https://github.com/gfx-rs/wgpu/issues/4912.
    pub fn assign_existing(self, value: &Arc<T>) -> Id<T::Marker> { todo!() }

    pub fn assign_error(self, label: &str) -> Id<T::Marker> { todo!() }
}

impl<T: Resource> Registry<T> {
    pub(crate) fn prepare(&self, id_in: Option<Id<T::Marker>>) -> FutureId<T> { todo!() }

    pub(crate) fn request(&self) -> FutureId<T> { todo!() }
    pub(crate) fn try_get(&self, id: Id<T::Marker>) -> Result<Option<Arc<T>>, InvalidId> { todo!() }
    pub(crate) fn get(&self, id: Id<T::Marker>) -> Result<Arc<T>, InvalidId> { todo!() }
    pub(crate) fn read<'a>(&'a self) -> RwLockReadGuard<'a, Storage<T>> { todo!() }
    pub(crate) fn write<'a>(&'a self) -> RwLockWriteGuard<'a, Storage<T>> { todo!() }
    pub(crate) fn unregister_locked(
        &self,
        id: Id<T::Marker>,
        storage: &mut Storage<T>,
    ) -> Option<Arc<T>> { todo!() }
    pub(crate) fn force_replace(&self, id: Id<T::Marker>, mut value: T) { todo!() }
    pub(crate) fn force_replace_with_error(&self, id: Id<T::Marker>, label: &str) { todo!() }
    pub(crate) fn unregister(&self, id: Id<T::Marker>) -> Option<Arc<T>> { todo!() }

    pub(crate) fn label_for_resource(&self, id: Id<T::Marker>) -> String { todo!() }

    pub(crate) fn generate_report(&self) -> RegistryReport { todo!() }
}
