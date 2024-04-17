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
    marker: std::marker::PhantomData<T>,
}
