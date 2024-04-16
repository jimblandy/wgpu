#![allow(unused)]

use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::{
    backtrace::Backtrace,
    cell::{Cell, RefCell, UnsafeCell},
    panic::{self, Location},
    thread,
};

/// A guard that provides read access to snatchable data.
pub struct SnatchGuard<'a>(RwLockReadGuard<'a, ()>);
/// A guard that allows snatching the snatchable data.
pub struct ExclusiveSnatchGuard<'a>(RwLockWriteGuard<'a, ()>);

/// A value that is mostly immutable but can be "snatched" if we need to destroy
/// it early.
///
/// In order to safely access the underlying data, the device's global snatchable
/// lock must be taken. To guarantee it, methods take a read or write guard of that
/// special lock.
pub struct Snatchable<T> {
    value: UnsafeCell<Option<T>>,
}

impl<T> Snatchable<T> {
    pub fn new(val: T) -> Self { todo!() }

    /// Get read access to the value. Requires a the snatchable lock's read guard.
    pub fn get(&self, _guard: &SnatchGuard) -> Option<&T> { todo!() }

    /// Get write access to the value. Requires a the snatchable lock's write guard.
    pub fn get_mut(&self, _guard: &mut ExclusiveSnatchGuard) -> Option<&mut T> { todo!() }

    /// Take the value. Requires a the snatchable lock's write guard.
    pub fn snatch(&self, _guard: ExclusiveSnatchGuard) -> Option<T> { todo!() }

    /// Take the value without a guard. This can only be used with exclusive access
    /// to self, so it does not require locking.
    ///
    /// Typically useful in a drop implementation.
    pub fn take(&mut self) -> Option<T> { todo!() }
}

// Can't safely print the contents of a snatchable object without holding
// the lock.
impl<T> std::fmt::Debug for Snatchable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { todo!() }
}

unsafe impl<T> Sync for Snatchable<T> {}

thread_local! {
    static READ_LOCK_LOCATION: Cell<Option<(&'static Location<'static>, Backtrace)>> = const { Cell::new(None) };
}

/// A Device-global lock for all snatchable data.
pub struct SnatchLock {
    lock: RwLock<()>,
}

impl SnatchLock {
    /// The safety of `Snatchable::get` and `Snatchable::snatch` rely on their using of the
    /// right SnatchLock (the one associated to the same device). This method is unsafe
    /// to force force sers to think twice about creating a SnatchLock. The only place this
    /// method should be called is when creating the device.
    pub unsafe fn new() -> Self { todo!() }

    /// Request read access to snatchable resources.
    #[track_caller]
    pub fn read(&self) -> SnatchGuard { todo!() }

    /// Request write access to snatchable resources.
    ///
    /// This should only be called when a resource needs to be snatched. This has
    /// a high risk of causing lock contention if called concurrently with other
    /// wgpu work.
    pub fn write(&self) -> ExclusiveSnatchGuard { todo!() }
}

impl Drop for SnatchGuard<'_> {
    fn drop(&mut self) { todo!() }
}
