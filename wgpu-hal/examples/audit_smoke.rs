//! A smoke test for `wgpu_hal::audit`.
//!
//! This drives a real Vulkan instance through the auditing wrapper end to
//! end: enumerate adapters, open a device, create a buffer and write to it
//! via mapping, then record and submit an (empty) command buffer and wait
//! for it to finish. Every call in this program passes through
//! `wgpu_hal::audit`'s `Audited` wrappers before reaching the real Vulkan
//! backend.
//!
//! Run with `WGPU_AUDIT_HAL_USAGE` unset to compare against the
//! unaudited path; the two should behave identically.

use wgpu_hal::Instance as _;

fn main() {
    env_logger::init();
    unsafe { run() };
}

unsafe fn run() {
    let instance_desc = wgpu_hal::InstanceDescriptor {
        name: "audit-smoke",
        flags: wgpu_types::InstanceFlags::debugging(),
        memory_budget_thresholds: Default::default(),
        backend_options: Default::default(),
        telemetry: None,
        display: None,
    };

    let instance: Box<dyn wgpu_hal::DynInstance> = Box::new(
        unsafe { <wgpu_hal::vulkan::Api as wgpu_hal::Api>::Instance::init(&instance_desc) }
            .expect("failed to create a vulkan instance"),
    );

    let audit_enabled = std::env::var("WGPU_AUDIT_HAL_USAGE").is_ok_and(|v| v != "0");
    let instance = if audit_enabled {
        println!("WGPU_AUDIT_HAL_USAGE set: wrapping the instance in the auditing layer");
        wgpu_hal::audit::new_auditing_instance(
            instance,
            wgpu_types::Backend::Vulkan,
            wgpu_hal::audit::report_by_log(log::Level::Info),
        )
    } else {
        println!("WGPU_AUDIT_HAL_USAGE not set: using the instance unaudited");
        instance
    };

    let exposed = unsafe { instance.enumerate_adapters(None) }
        .into_iter()
        .next()
        .expect("no adapters found");
    println!("Using adapter: {}", exposed.info.name);

    let open_device = unsafe {
        exposed.adapter.open(
            exposed.features,
            &wgpu_types::Limits::default(),
            &wgpu_types::MemoryHints::default(),
        )
    }
    .expect("failed to open device");
    let device = open_device.device;
    let queue = open_device.queue;

    const SIZE: wgpu_types::BufferAddress = 256;
    let buffer = unsafe {
        device.create_buffer(&wgpu_hal::BufferDescriptor {
            label: Some("audit-smoke buffer"),
            size: SIZE,
            usage: wgpu_types::BufferUses::MAP_WRITE
                | wgpu_types::BufferUses::MAP_READ
                | wgpu_types::BufferUses::COPY_SRC
                | wgpu_types::BufferUses::COPY_DST,
            memory_flags: wgpu_hal::MemoryFlags::empty(),
        })
    }
    .expect("failed to create buffer");

    unsafe {
        let mapping = device
            .map_buffer(&*buffer, 0..SIZE)
            .expect("failed to map buffer");
        core::ptr::write_bytes(mapping.ptr.as_ptr(), 0x42, SIZE as usize);
        #[allow(clippy::single_range_in_vec_init)]
        device.flush_mapped_ranges(&*buffer, &[0..SIZE]);
        device.unmap_buffer(&*buffer);
    }
    println!("Wrote {SIZE} bytes to a mapped buffer");

    let mut encoder = unsafe {
        device.create_command_encoder(&wgpu_hal::CommandEncoderDescriptor {
            label: Some("audit-smoke encoder"),
            queue: &*queue,
        })
    }
    .expect("failed to create command encoder");

    let command_buffer = unsafe {
        encoder
            .begin_encoding(Some("audit-smoke pass"))
            .expect("failed to begin encoding");
        encoder.end_encoding().expect("failed to end encoding")
    };

    let fence = unsafe { device.create_fence() }.expect("failed to create fence");
    unsafe {
        queue
            .submit(&[&*command_buffer], &[], (&*fence, 1))
            .expect("submit failed");
        device.wait(&*fence, 1, None).expect("wait failed");
    }
    println!("Submitted and waited on an empty command buffer");

    unsafe {
        encoder.reset_all(vec![command_buffer]);
        device.destroy_fence(fence);
        device.destroy_buffer(buffer);
    }

    println!("audit smoke test completed successfully");
}
