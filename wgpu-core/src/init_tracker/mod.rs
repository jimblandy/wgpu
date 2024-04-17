/*! Lazy initialization of texture and buffer memory.

The WebGPU specification requires all texture & buffer memory to be
zero initialized on first read. To avoid unnecessary inits, we track
the initialization status of every resource and perform inits lazily.

The granularity is different for buffers and textures:

- Buffer: Byte granularity to support usecases with large, partially
  bound buffers well.

- Texture: Mip-level per layer. That is, a 2D surface is either
  completely initialized or not, subrects are not tracked.

Every use of a buffer/texture generates a InitTrackerAction which are
recorded and later resolved at queue submit by merging them with the
current state and each other in execution order.

It is important to note that from the point of view of the memory init
system there are two kind of writes:

- **Full writes**: Any kind of memcpy operation. These cause a
  `MemoryInitKind.ImplicitlyInitialized` action.

- **(Potentially) partial writes**: For example, write use in a
  Shader. The system is not able to determine if a resource is fully
  initialized afterwards but is no longer allowed to perform any
  clears, therefore this leads to a
  `MemoryInitKind.ImplicitlyInitialized` action, exactly like a read
  would.

 */

use smallvec::SmallVec;
use std::{fmt, iter, ops::Range};

mod buffer;
mod texture;

pub(crate) use buffer::{BufferInitTracker, BufferInitTrackerAction};
pub(crate) use texture::{
    TextureInitRange, TextureInitTracker,
    TextureInitTrackerAction,
};

#[derive(Debug, Clone, Copy)]
pub(crate) enum MemoryInitKind {
}

/// Tracks initialization status of a linear range from 0..size
#[derive(Debug, Clone)]
pub(crate) struct InitTracker<Idx: Ord + Copy + Default> {
    marker: std::marker::PhantomData<Idx>,
}
