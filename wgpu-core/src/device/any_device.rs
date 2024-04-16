use wgt::Backend;

use super::Device;
/// The `AnyDevice` type: a pointer to a `Device<A>` for any backend `A`.
use crate::hal_api::HalApi;

use std::fmt;
use std::mem::ManuallyDrop;
use std::ptr::NonNull;
use std::sync::Arc;

/// A pointer to a `Device<A>`, for any backend `A`.
///
/// Any `AnyDevice` is just like an `Arc<Device<A>>`, except that the `A` type
/// parameter is erased. To access the `Device`, you must downcast to a
/// particular backend with the \[`downcast_ref`\] or \[`downcast_clone`\]
/// methods.
pub struct AnyDevice {
}

impl AnyDevice {
}

impl Drop for AnyDevice {
    fn drop(&mut self) { todo!() }
}

impl fmt::Debug for AnyDevice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}

#[cfg(send_sync)]
unsafe impl Send for AnyDevice {}
#[cfg(send_sync)]
unsafe impl Sync for AnyDevice {}
