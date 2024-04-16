use std::hash::{Hash, Hasher};

use crate::{
    binding_model::{self},
    FastIndexMap,
};

/// Where a given BGL came from.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Origin {
}

/// A HashMap-like structure that stores a BindGroupLayouts [`wgt::BindGroupLayoutEntry`]s.
///
/// It is hashable, so bind group layouts can be deduplicated.
#[derive(Debug, Default, Clone, Eq)]
pub struct EntryMap {
}

impl PartialEq for EntryMap {
    fn eq(&self, other: &Self) -> bool { todo!() }
}

impl Hash for EntryMap {
    fn hash<H: Hasher>(&self, state: &mut H) { todo!() }
}

