//! Module for hashing utilities.
//!
//! Named hash_utils to prevent clashing with the std::hash module.

/// HashMap using a fast, non-cryptographic hash algorithm.
pub type FastHashMap<K, V> =
    std::collections::HashMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;
/// HashSet using a fast, non-cryptographic hash algorithm.
pub type FastHashSet<K> =
    std::collections::HashSet<K, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

/// A pre-hashed key using FxHash which allows the hashing operation to be disconnected
/// from the storage in the map.
pub struct PreHashedKey<K>(u64, std::marker::PhantomData<fn() -> K>);

impl<K> std::fmt::Debug for PreHashedKey<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { todo!() }
}

impl<K> Copy for PreHashedKey<K> {}

impl<K> Clone for PreHashedKey<K> {
    fn clone(&self) -> Self { todo!() }
}

impl<K> PartialEq for PreHashedKey<K> {
    fn eq(&self, other: &Self) -> bool { todo!() }
}

impl<K> Eq for PreHashedKey<K> {}

impl<K> std::hash::Hash for PreHashedKey<K> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { todo!() }
}

/// A hasher which does nothing. Useful for when you want to use a map with pre-hashed keys.
///
/// When hashing with this hasher, you must provide exactly 8 bytes. Multiple calls to `write`
/// will overwrite the previous value.
#[derive(Default)]
pub struct IdentityHasher {
}

impl std::hash::Hasher for IdentityHasher {
    fn write(&mut self, bytes: &[u8]) { todo!() }

    fn finish(&self) -> u64 { todo!() }
}
