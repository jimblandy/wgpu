# Locking in `wgpu-core`

## What users expect from `wgpu-core`

The `wgpu-core` crate offers a safe API, for the most part. This
implies that well-typed users are free of data races. `Global` is
`Send` and `Sync`, so its methods (which constitute most of
`wgpu-core`'s API) must be safe to call on any thread, and its `&self`
methods must be safe to call simultaneously from multiple threads.

The only unsafe part of `wgpu-core`'s API is buffer mapping, discussed
below.

## What `wgpu-core` expects from `wgpu-hal`

Although the `wgpu-hal` traits are full of `unsafe` methods, their
safety conditions only concern relative lifetimes and states, not
concurrent access; hal methods are free of data races when used in
well-typed code.

## User-visible synchronization in `wgpu-core`

Why does `wgpu-core` need any locks at all?

In general, WebGPU DOM objects have little mutable state: once objects are
constructed, their contents don't change. Since `wgpu_core` is modeled on
WebGPU, there are actually only a few cases where one thread can observe actions
taken by another:

- Creating resources and dropping ids affects the set of valid ids. (

- Buffers can be mapped and unmapped. See "Buffer mapping", below.

- Command and pass encoding appends commands to the encoder. (This is not
  inevitable: See "Using ids increases synchronization", below.)
  
- Submitting commands affects whether buffers can be mapped. (As a negligible
  side effect, it also affects texture and buffer contents, but `wgpu-core`
  itself doesn't have to track that.)

- Device polling settles work items like buffer mappings, work
  done closures, and device loss callbacks.

- I don't know about presentation, but it seems likely to have effects visible
  to other threads. Requesting surfaces?

- Buffers and textures can be destroyed.

- Devices can become lost.

Technically, WebGPU error scopes are also visible between threads, since they
are global to the device. But error scopes are implemented outside `wgpu-core`.

## Internal synchronization in `wgpu-core`



## Using ids instead of references requires more synchronization

Most of `wgpu-core`'s public methods take resource ids as arguments, rather than
Rust references to those resources' actual types. This forces `wgpu-core` to use
internal synchronization, in addition to that required for the id registry
itself.

Because ids are `Copy` and `Send`, Rust's alias analysis doesn't help us much.
Where an idiomatic Rust interface could simply assume that a mutable reference
has ensured exclusive access to the resource, or that a shared reference has
ensured the resource will remain alive for the duration of the call, `wgpu-core`
must incorporate an `Arc` to protect the resource's lifetime, and a `Mutex` to
protect its mutable state.

This is especially unfortunate for command encoding. Encoding shouldn't require
any locking of the encoder's own state, nor should encoders need to be
referenced via an `Arc`. Command encoders exist to enable recording commands
concurrently in different threads without synchronization; for example, [Vulkan
command pools are externally synchronized][vcpx],

> ... meaning that a command pool must not be used concurrently in multiple
> threads. That includes use via recording commands on any command buffers
> allocated from the pool, as well as operations that allocate, free, and reset
> command buffers or the pool itself.

[vcpx]: https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-pools

The easiest way to ensure "external synchronization" (in Vulkan terms) during
command recording is to use each command buffer on only one thread: no
synchronization is needed at all. This is what an idiomatic Rust API would give
us for free, and what using ids for command buffers loses.

For most other `wgpu-core` resources, multiple ownership is inherent in the API.
A buffer may appear in many bind groups; many pipelines may share a bind group
layout; and so on. For these, incorporating `Arc` into `wgpu-core`'s API makes
sense.

One might wonder if devices also should be passed by Rust reference. But
`Global::poll_all_devices` requires `wgpu-core` to maintain a list of all the
global's devices, so weak ownership is visible in the API, meaning that an `Arc`
is necessary. And since devices are meant to be used from many threads, even if
the API took devices by `&mut` reference, that would only force most users to
wrap them in an `Mutex` themselves. In this light, internal synchronization and
`Arc` ownership are probably the most convenient arrangement for `Device`.

## Buffer mapping

## Command and pass encoding

