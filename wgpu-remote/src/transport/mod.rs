/*! The [`Sender`]  and [`Receiver`] traits, for wgpu transports.

This module defines the [`Sender`] and [`Receiver`] traits, representing the
endpoints of a one-way communications channel between a wgpu client and server,
based on shared memory. A single `Sender` sends messages to a single `Receiver`.
For two-way communication, each side needs its own `Sender` and `Receiver`. This
documentation refers to the two sides of a connection as "counterparts".

The [`Sender`] trait abstracts over platform APIs to define a portable but
low-level interface for communication based on shared memory. To keep [`Sender`]
easy to implement, it is a low-level interface: byte-oriented, unbuffered, and
with no enforcement of synchronized access to shared memory. However, it should
be sufficient for applications to build well-typed, thread-safe abstractions
that are generic over any `Sender` implementation.

As messages arrive from the counterpart, a `Sender` implementation passes them
to a [`Receiver`] implementation provided by the user, which serves the role of
"callback" or "event handler". Like [`Sender`], [`Receiver`] is a low-level,
byte-oriented interface; interpretation of the contents as data meaningful to
the application is left to the [`Receiver`] implementation.

The `Sender` trait is meant to be easy to implement in terms of a wide range of
operating system mechanisms:

- A Unix implementation might use [`AF_UNIX`] address family sockets
  (also known as "Unix domain sockets") to exchange messages. Shared
  memory segments would be created with [`memfd_create`], conveyed to
  the counterpart over the socket using `SCM_RIGHTS` ancillary
  messages, and mapped into each side's address space with [`mmap`].

- A Windows implementation might use ordinary sockets for
  communication, [`CreateFileMappingW`] to create memory segments, and
  then use [`DuplicateHandle`] to share them.

- For testing, the [`transport::local`] module provides a [`Sender`]
  implementation in which both sides of the connection live in the
  same process. "Shared" memory is simply an ordinary block of memory.

Exactly how [`Receiver`]s get called when messages arrive is specific to the
`Sender` implementation:

- A `Sender` implementation might spawn a thread to read messages from a socket
  and invoke the `Receiver` when complete messages have been received.

- A `Sender` implementation might register an internal listener with
  some sort of platform event loop, and have that listener call the
`Receiver` when appropriate.

While the [`Sender`] and [`Receiver`] traits can be implemented directly in
terms of operating system facilities, they are also meant to integrate smoothly
with existing interprocess communication mechanisms and event loops, like
Firefox's [`IPDL`] and [`nsISerialEventTarget`].

Although these traits are designed for use with `wgpu`, this module attempts to
fully specify the contract between a transport and its user, independently of
the details of `wgpu` or WebGPU. This is meant to help developers implement and
test transports in isolation from graphics APIs, browsers, applications, and so
on.

## Shared memory

The [`Sender`] and [`Receiver`] traits are intended for use in situations where
the endpoints can share memory with each other, such that writes to a shared
memory segment on one side are immediately visible on the other.

- [`Sender::allocate_shared_memory`] creates a shared memory segment, and
  returns an id by which both counterparts can refer to it.

- [`Sender::send_message`] sends the counterpart a message whose content resides
  in a given shared memory segment.

- [`Sender::map_shared_memory`] takes a given shared memory segment and makes
  it visible in the caller's address space.

- [`Sender::close_shared_memory`] frees a shared memory segment.

The application can create as many shared memory segments as it needs. Shared
memory handles are transparent newtypes around integers, so they are easy to
refer to in messages or data structures held in other shared memory segments.

For example, a WebGPU implementation might create a shared memory segment to
hold a queue of API calls made by web content that are waiting to be conveyed to
a GPU sandbox process for execution; and it might create additional shared
memory segments representing mappable buffer contents.

## Using `Sender` without shared memory

It is possible to implement `Sender` without using shared memory (for example,
over a network connection), if the transport's user is willing to make calls to
[`Sender::flush_shared_memory_range`] to explicitly indicate which regions of
its shared memory segments have new content that must be copied to the
counterpart. Although this interface is trickier to use, it allows the transport
to behave as expected whether or not it can actually create memory segments
shared with the counterpart, which in turn allows the application to work over a
broader range of transports. Shared memory becomes merely a transparent
optimization, not an architectural feature.

The requirement to flush modified regions is not as onerous as one might expect.
In practice, it is often the case that, by the time one has ensured that the
application's interactions with shared memory segments are free of data races,
it is also apparent where flushes would be necessary.

For example, although WebGPU's buffer mapping behavior is intended to be
implemented using memory regions that are shared between the web content process
and a sandboxed process that interacts directly with the GPU, it is also
possible to implement WebGPU without shared memory. To ensure consistent
behavior across browsers and GPUs, WebGPU's buffer API segregates web content
access from GPU access: web content can access a buffer only after mapping it,
and the GPU can access a buffer only when it is unmapped. These ownership
transitioning operations are where flushes would need to occur, in the case that
the web content and GPU process do not actually share memory:

- Before a buffer is mapped by web content, the GPU process must flush any
  regions of the buffer it may have written to.

- When web content unmaps a buffer, the content process must flush any regions
  of the buffer web content modified (conservatively, the entire buffer).

An implementation of [`Sender`] may guarantee that it uses shared memory. Users
of such an implementation need not call [`flush_shared_memory_range`], but
naturally, taking advantage of this looser contract limits which transport
implementations they can use.

[`Sender`] implementations may even decide whether or not to use shared memory
dynamically. Users of such implementations mustx assume the worst, and call
[`flush_shared_memory_range`] as described in its documentation.

In Rust, data races are undefined behavior. Users of these traits are
responsible for using shared memory in a way that is free of data races. If a
[`Sender`] implementation does not use shared memory, its users must assume that
calls to [`flush_shared_memory_range`] and [`send_message`] are the only
synchronization operations that establish an ordering between memory accesses
for the purposes of avoiding data races.

[`mmap`]: https://man7.org/linux/man-pages/man2/mmap.2.html
[`AF_UNIX`]: https://man7.org/linux/man-pages/man7/unix.7.html
[`CreateFileMappingW`]: https://learn.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-createfilemappingw
[`DuplicateHandle`]: https://learn.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-duplicatehandle
[`transport::local`]: crate::transport::local
[`IPDL`]: https://firefox-source-docs.mozilla.org/ipc/ipdl.html
[`nsISerialEventTarget`]: https://searchfox.org/mozilla-central/rev/7f7e8f6e4b8e09b145d29a57a1b341c6b11f4225/xpcom/threads/nsISerialEventTarget.idl#25
[`flush_shared_memory_range`]: Sender::flush_shared_memory_range
[`send_message`]: Sender::send_message

*/

pub mod local;
pub mod unix;

pub use crate::Block;

use std::ops::Range;

/// The sending side of a connection between wgpu client and server processes.
///
/// When a `Sender` is dropped, all shared memory handles created with it or
/// received from the counterpart are closed. This does not affect the
/// counterpart's ability to use these handles, as the counterpart's handles are
/// closed separately.
///
/// Note that, even after dropping a `Sender`:
///
/// - The `Receiver` on the same side as the dropped `Sender` may still receive
///   messages.
///
/// - Shared memory mappings made via this `Sender` remain accessible until
///   their [`Block`]s are dropped.
pub trait Sender {
    /// Allocate a segment of memory shared with the counterpart.
    ///
    /// `size` is rounded up to the next multiple of
    /// [`SHARED_MEMORY_ALIGNMENT`].
    ///
    /// The returned handle is an ordinary integer, so it can be included in
    /// messages passed via [`Sender::send_message`].
    ///
    /// The handle is immediately valid to use with this `Sender`.
    ///
    /// The handle is valid to use with the counterpart's `Sender` by the time
    /// the counterpart receives the next message sent on this `Sender`.
    fn allocate_shared_memory(&mut self, size: usize) -> std::io::Result<SharedMemoryHandle>;

    /// Close `handle`.
    ///
    /// After this call, `handle` may no longer be passed to
    /// [`map_shared_memory`] or [`send_message`] in this `Sender`. This has no
    /// effect on our counterpart's ability to use `handle` with their `Sender`.
    ///
    /// Closing `handle` does not affect any [`Block`]s mapping it.
    ///
    /// When a `Sender` is dropped, all open handles created with it are
    /// automatically closed.
    ///
    /// [`map_shared_memory`]: Self::map_shared_memory
    /// [`send_message`]: Self::send_message
    fn close_shared_memory(&mut self, handle: SharedMemoryHandle);

    /// Map `handle` into the caller's address space.
    ///
    /// Make the shared memory represented by `handle` visible in the caller's
    /// address space, and return a [`Block`] managing it.
    ///
    /// When the returned [`Block`] is dropped, the mapping is removed. The
    /// `Block` is allowed to outlive `self` and `handle`.
    ///
    /// The address of the mapped memory is a multiple of
    /// [`SHARED_MEMORY_ALIGNMENT`].
    ///
    /// You may map a given shared memory multiple times; each call returns a
    /// distinct [`Block`]. Calling [`Block::bytes_mut`] on those blocks may or
    /// may not return aliasing slices.
    ///
    /// Since the returned [`Block`] refers to shared memory, you must generally
    /// assume that it is visible to many threads in many processes
    /// simultaneously. It is up to you to ensure proper synchronization among
    /// all its users.
    fn map_shared_memory(&mut self, handle: SharedMemoryHandle) -> Block;

    /// Send a message to the counterpart.
    ///
    /// Send a message to the counterpart, consisting of a shared memory handle
    /// and a subrange of its contents, to be passed to counterpart by calling
    /// its [`Receiver`]'s [`receive_message`] method.
    ///
    /// The message is sent immediately, without buffering. If the counterpart
    /// is not receiving messages, this may block.
    ///
    /// If this returns an error, the `Sender` implementation is not obliged to
    /// handle any future calls correctly, although such calls must not cause
    /// undefined behavior.
    ///
    /// The exact interpretation of `range` is entirely up to the user of the
    /// transport. In typical use, `range` refers to a byte range of `handle`
    /// that holds serialized messages of some sort, but the transport itself
    /// doesn't assume that.
    ///
    /// Since shared memory handles are ordinary integers and are automatically
    /// usable in both counterparts, handles other than `handle` itself can be
    /// conveyed to the counterpart simply by serializing them in the content of
    /// messages, or storing them in other shared memory segments where the
    /// counterpart can find them.
    ///
    /// [`receive_message`]: Receiver::receive_message
    fn send_message(
        &mut self,
        handle: SharedMemoryHandle,
        range: Range<usize>,
    ) -> std::io::Result<()>;

    /// Ensure writes to a region of shared memory are visible to the counterpart.
    ///
    /// Read the contents of the subrange `range` of the memory segment referred
    /// to by `handle` in this process, and then write those contents to the
    /// corresponding shared memory range in our counterpart. The `handle` may
    /// have been created by either counterpart.
    ///
    /// The user must ensure that these copies are free of data races. This rule
    /// is meant to allow consistent behavior over a range of different
    /// implementations:
    ///
    /// - In implementations based on shared memory, the memory's contents are always
    ///   equal in both counterparts, so this call may do nothing; such an
    ///   implementation effectively performs both the read and write right now.
    ///
    /// - Implementations without shared memory can transmit the contents of the
    ///   given range to the counterpart, and have the freedom to defer and coalesce
    ///   flushes, as long as the contents are there by the time the message is
    ///   delivered.
    ///
    /// An implementation of [`Sender`] may guarantee that it uses shared memory.
    /// Users of such implementations need not call this function. However, they
    /// should keep in mind that doing so limits which transport implementations they
    /// can use.
    ///
    /// [`send_message`]: Self::send_message
    fn flush_shared_memory_range(
        &mut self,
        handle: SharedMemoryHandle,
        range: Range<usize>,
    ) -> std::io::Result<()>;
}

/// A dynamically dispatched `Sender`.
pub type DynSender = dyn Sender + Send + 'static;

/// The receiving side of a connection between wgpu client and server processes.
///
/// The user must provide an implementation of this class when creating the
/// corresponding `Sender`, which will invoke its methods to report messages
/// received, and errors encountered trying to receive messages. Exactly how the
/// `Receiver` is supplied depends on the `Sender` implementation.
///
/// The `Sender` implementation should specify on which thread the
/// [`receive_message`] method is invoked.
///
/// A `Receiver` is dropped:
///
/// - when the counterpart closes their `Sender`,
///
/// - when [`receive_message`] returns an error, or
///
/// - after a call to [`receive_error`] returns.
///
/// Even after a `Receiver` is dropped, shared memory handles received from the
/// counterpart can still be used with this side's `Sender`.
///
/// [`receive_message`]: Self::receive_message
/// [`receive_error`]: Self::receive_error
pub trait Receiver {
    /// Called when a message has been received from the counterpart.
    ///
    /// If this returns `Result::Err`, then this method will not be called again
    /// on this `Receiver`, and the `Receiver` will be dropped.
    fn receive_message(
        &mut self,
        handle: SharedMemoryHandle,
        range: Range<usize>,
    ) -> Result<(), ReceiverError>;

    /// Report an error encountered trying to receive a message.
    ///
    /// After this call returns, this `Receiver` will be dropped.
    fn receive_error(&mut self, error: std::io::Error) {
        let _ = error;
    }
}

/// A dynamically dispatched `Receiver`.
pub type DynReceiver = dyn Receiver + Send + 'static;

/// A handle to a segment of shared memory managed by a [`Sender`].
///
/// You can call [`Sender::allocate_shared_memory`] to create a
/// `SharedMemoryHandle`, use its serialization and deserialization
/// implementations to send it to your counterpart, and then both
/// sides can use [`Sender::map_shared_memory`] to map the memory
/// into their address spaces.
///
/// Note that this `SharedMemoryHandle` may only be used with the
/// [`Sender`] that created it, and its counterpart. This means you
/// can't use it to create memory shared with a third process.
///
/// [`Sender`] implementations are encouraged to avoid ever reusing handle
/// values, to catch stale handle reuse.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SharedMemoryHandle(pub usize);

/// The minimum guaranteed alignment for [`Sender`] shared memory.
///
/// Any [`Block`] returned by [`Sender::map_shared_memory`] has
/// an address that is a multiple of this.
///
/// This is always a power of two, and always at least 4096.
pub const SHARED_MEMORY_ALIGNMENT: usize = 4096;

#[derive(Debug)]
pub struct ReceiverError;

impl std::fmt::Display for ReceiverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{wgpu_remote::ReceiverError}")
    }
}
