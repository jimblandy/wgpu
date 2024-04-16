use wgt::Backend;

/// The `AnySurface` type: a `Arc` of a `A::Surface` for any backend `A`.
use crate::hal_api::HalApi;

use std::fmt;
use std::mem::ManuallyDrop;
use std::ptr::NonNull;

/// An `A::Surface`, for any backend `A`.
///
/// Any `AnySurface` is just like an `A::Surface`, except that the `A` type
/// parameter is erased. To access the `Surface`, you must downcast to a
/// particular backend with the \[`downcast_ref`\] or \[`take`\] methods.
pub struct AnySurface {
}

impl AnySurface {
}

impl Drop for AnySurface {
    fn drop(&mut self) { todo!() }
}

impl fmt::Debug for AnySurface {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}

#[cfg(send_sync)]
unsafe impl Send for AnySurface {}
#[cfg(send_sync)]
unsafe impl Sync for AnySurface {}
