use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
    sync::{Arc, Weak},
};

use once_cell::sync::OnceCell;
use parking_lot::Mutex;

use crate::{PreHashedKey};

pub struct ResourcePool<K, V> {
    marker: std::marker::PhantomData<(K, V)>,
}

