# Resource lifetime management

The platform APIs upon which `wgpu` is built have various requirements
for safe usage, usually common-sense restrictions like, "You must not
free a buffer until all operations using it have completed." But
`wgpu-core` is more forgiving: for example, you can call
`Global::buffer_drop` on a given buffer ID as soon as you are ready to
stop passing it to `wgpu-core` methods, even if the buffer is used by
command buffers you have not yet submitted for execution. Internally,
`wgpu-core` tracks all references to the buffer, and frees it only
when it will no longer be used.

This sort of lifetime tracking is entirely `wgpu-core`'s
responsibility. The `wgpu_hal` crate itself makes no effort to enforce
these rules, and `wgpu_hal::Api` implementations are encouraged to use
the most lightweight definitions for types like `Api::Buffer`
possible. For example, Vulkan's `Buffer` type is:

    pub struct Buffer {
        raw: vk::Buffer,
        block: Mutex<gpu_alloc::MemoryBlock<vk::DeviceMemory>>,
    }

This doesn't even implement `Drop`. It is entirely up to the caller to
call `wgpu_hal::Device::destroy_buffer` at an appropriate time. Said
caller is usually `wgpu-core`.



    pub trait Api: Clone + Sized {
        ...
        type Buffer: fmt::Debug + Send + Sync + 'static;
        type Texture: fmt::Debug + Send + Sync + 'static;
        type SurfaceTexture: fmt::Debug + Send + Sync + Borrow<Self::Texture>;
        type TextureView: fmt::Debug + Send + Sync;
        ...
    }

Methods like `CommandEncoder::copy_buffer_to_buffer` simply take
references to these resource types:

    pub trait CommandEncoder<A: Api> + Send + Sync + fmt::Debug {
        ...
        unsafe fn copy_buffer_to_buffer<T>(&mut self, src: &A::Buffer, dst: &A::Buffer, regions: T)
        where
            T: Iterator<Item = BufferCopy>;
        ...
    }

