use std::hash::{Hash, Hasher};

use crate::{
    binding_model::{self},
    FastIndexMap,
};

/// Where a given BGL came from.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Origin {
    /// The bind group layout was created by the user and is present in the BGL resource pool.
    Pool,
    /// The bind group layout was derived and is not present in the BGL resource pool.
    Derived,
}

/// A HashMap-like structure that stores a BindGroupLayouts [`wgt::BindGroupLayoutEntry`]s.
///
/// It is hashable, so bind group layouts can be deduplicated.
#[derive(Debug, Default, Clone, Eq)]
pub struct EntryMap {
    /// We use a IndexMap here so that we can sort the entries by their binding index,
    /// guaranteeing that the hash of equivalent layouts will be the same.
    inner: FastIndexMap<u32, wgt::BindGroupLayoutEntry>,
    /// We keep track of whether the map is sorted or not, so that we can assert that
    /// it is sorted, so that PartialEq and Hash will be stable.
    ///
    /// We only need sorted if it is used in a Hash or PartialEq, so we never need
    /// to actively sort it.
    sorted: bool,
}

impl PartialEq for EntryMap {
    fn eq(&self, other: &Self) -> bool { todo!() }
}

impl Hash for EntryMap {
    fn hash<H: Hasher>(&self, state: &mut H) { todo!() }
}

impl EntryMap {
    fn assert_sorted(&self) { todo!() }

    /// Create a new [`BindGroupLayoutEntryMap`] from a slice of [`wgt::BindGroupLayoutEntry`]s.
    ///
    /// Errors if there are duplicate bindings or if any binding index is greater than
    /// the device's limits.
    pub fn from_entries(
        device_limits: &wgt::Limits,
        entries: &[wgt::BindGroupLayoutEntry],
    ) -> Result<Self, binding_model::CreateBindGroupLayoutError> { todo!() }

    /// Get the count of [`wgt::BindGroupLayoutEntry`]s in this map.
    pub fn len(&self) -> usize { todo!() }

    /// Get the [`wgt::BindGroupLayoutEntry`] for the given binding index.
    pub fn get(&self, binding: u32) -> Option<&wgt::BindGroupLayoutEntry> { todo!() }

    /// Iterator over all the binding indices in this map.
    pub fn indices(&self) -> impl ExactSizeIterator<Item = u32> + '_ { std::iter::empty() }

    /// Iterator over all the [`wgt::BindGroupLayoutEntry`]s in this map.
    pub fn values(&self) -> impl ExactSizeIterator<Item = &wgt::BindGroupLayoutEntry> + '_ { std::iter::empty() }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&u32, &wgt::BindGroupLayoutEntry)> + '_ { std::iter::empty() }

    pub fn is_empty(&self) -> bool { todo!() }

    pub fn contains_key(&self, key: u32) -> bool { todo!() }

    pub fn entry(&mut self, key: u32) -> indexmap::map::Entry<'_, u32, wgt::BindGroupLayoutEntry> { todo!() }
}
