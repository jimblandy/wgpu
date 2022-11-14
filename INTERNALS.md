# `wgpu` internals guide

The `wgpu` project divides responsibilities as follows:

- Crates like `ash`, `metal`, and `d3d12` provide Rust bindings for
  the platform's graphics APIs.

- `wgpu-hal` provides an unsafe abstraction over the platform graphics
  APIs.

- `wgpu-core` performs validation and resource tracking, providing a
  safe API built on top of `wgpu-hal`.
  
- `wgpu` core provides an idiomatic Rust interface on top of `wgpu-core`.

Bindings for other languages like C, Python, and .NET generally use
`wgpu-core` directly, which tries to keep its interface FFI-friendly.
Firefox also uses `wgpu-core` directly for its WebGPU implementation.

Almost all validation is handled in platform-independent code in
`wgpu-core`. The `wgpu-hal` crate aims to be as thin a layer over the
platform APIs as is possible, while still offering a consistent
contract across the three key platforms: Direct3D 12, Metal, and
Vulkan. Then, `wgpu-core` takes care of enforcing the rules of that
contract.

# See also

- `wgpu-core` [resource lifetime management](wgpu-core/src/device/resource-lifetime-management.md)
