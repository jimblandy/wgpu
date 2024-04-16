use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
    sync::{Arc, Weak},
};

use once_cell::sync::OnceCell;
use parking_lot::Mutex;

use crate::{PreHashedKey, PreHashedMap};

type SlotInner<V> = Weak<V>;
type ResourcePoolSlot<V> = Arc<OnceCell<SlotInner<V>>>;

pub struct ResourcePool<K, V> {
    // We use a pre-hashed map as we never actually need to read the keys.
    //
    // This additionally allows us to not need to hash more than once on get_or_init.
    inner: Mutex<PreHashedMap<K, ResourcePoolSlot<V>>>,
}

impl<K: Clone + Eq + Hash, V> ResourcePool<K, V> {
    pub fn new() -> Self { todo!() }

    /// Get a resource from the pool with the given entry map, or create a new one if it doesn't exist using the given constructor.
    ///
    /// Behaves such that only one resource will be created for each unique entry map at any one time.
    pub fn get_or_init<F, E>(&self, key: K, constructor: F) -> Result<Arc<V>, E>
    where
        F: FnOnce(K) -> Result<Arc<V>, E>,
    { todo!() }

    /// Remove the given entry map from the pool.
    ///
    /// Must *only* be called in the Drop impl of [`BindGroupLayout`].
    pub fn remove(&self, key: &K) { todo!() }
}

#[cfg(test)]
mod tests {
    use std::sync::{
        atomic::{AtomicU32, Ordering},
        Barrier,
    };

    use super::*;

    #[test]
    fn deduplication() { todo!() }

    // Test name has "2_threads" in the name so nextest reserves two threads for it.
    #[test]
    fn concurrent_creation_2_threads() { todo!() }

    // Test name has "2_threads" in the name so nextest reserves two threads for it.
    #[test]
    fn create_while_drop_2_threads() { todo!() }
}
