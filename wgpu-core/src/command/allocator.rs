use crate::hal_api::HalApi;
use crate::resource_log;
use hal::Device as _;

use parking_lot::Mutex;

/// A pool of free [`wgpu_hal::CommandEncoder`]s, owned by a `Device`.
///
/// Each encoder in this list is in the "closed" state.
///
/// Since a raw [`CommandEncoder`][ce] is itself a pool for allocating
/// raw [`CommandBuffer`][cb]s, this is a pool of pools.
///
/// [ce]: wgpu_hal::CommandEncoder
/// [cb]: wgpu_hal::Api::CommandBuffer
pub(crate) struct CommandAllocator<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}
