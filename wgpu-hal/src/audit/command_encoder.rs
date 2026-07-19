/*! Implementation of [`validation_layer::Device`]. */
#![allow(unused_variables)]

use alloc::vec::Vec;

fn convert_attachment<'a>(
    a: &crate::Attachment<'a, super::TextureView>,
) -> crate::Attachment<'a, dyn crate::DynTextureView> {
    crate::Attachment {
        view: a.view.as_dyn(),
        usage: a.usage,
    }
}

fn convert_color_attachment<'a>(
    c: &crate::ColorAttachment<'a, super::TextureView>,
) -> crate::ColorAttachment<'a, dyn crate::DynTextureView> {
    crate::ColorAttachment {
        target: convert_attachment(&c.target),
        depth_slice: c.depth_slice,
        resolve_target: c.resolve_target.as_ref().map(convert_attachment),
        ops: c.ops,
        clear_value: c.clear_value,
    }
}

fn convert_depth_stencil_attachment<'a>(
    d: &crate::DepthStencilAttachment<'a, super::TextureView>,
) -> crate::DepthStencilAttachment<'a, dyn crate::DynTextureView> {
    crate::DepthStencilAttachment {
        target: convert_attachment(&d.target),
        depth_ops: d.depth_ops,
        stencil_ops: d.stencil_ops,
        clear_value: d.clear_value,
    }
}

fn convert_timestamp_writes<'a>(
    w: &crate::PassTimestampWrites<'a, super::QuerySet>,
) -> crate::PassTimestampWrites<'a, dyn crate::DynQuerySet> {
    crate::PassTimestampWrites {
        query_set: w.query_set.as_dyn(),
        beginning_of_pass_write_index: w.beginning_of_pass_write_index,
        end_of_pass_write_index: w.end_of_pass_write_index,
    }
}

fn convert_group_data<'a>(
    d: crate::PipelineGroupData<'a, super::Buffer>,
) -> crate::PipelineGroupData<'a, dyn crate::DynBuffer> {
    crate::PipelineGroupData {
        buffer: d.buffer.as_dyn(),
        offset: d.offset,
        stride: d.stride,
        count: d.count,
    }
}

impl crate::CommandEncoder for super::CommandEncoder {
    type A = super::Api;

    unsafe fn begin_encoding(&mut self, label: crate::Label) -> Result<(), crate::DeviceError> {
        unsafe { self.inner.begin_encoding(label) }
    }

    unsafe fn discard_encoding(&mut self) {
        unsafe { self.inner.discard_encoding() }
    }

    unsafe fn end_encoding(&mut self) -> Result<super::CommandBuffer, crate::DeviceError> {
        let inner = unsafe { self.inner.end_encoding()? };
        Ok(super::CommandBuffer::wrap(inner, self.shared.clone()))
    }

    unsafe fn reset_all<I>(&mut self, command_buffers: I)
    where
        I: Iterator<Item = super::CommandBuffer>,
    {
        let command_buffers: Vec<_> = command_buffers.map(|cb| cb.inner).collect();
        unsafe { self.inner.reset_all(command_buffers) }
    }

    unsafe fn transition_buffers<'a, T>(&mut self, barriers: T)
    where
        T: Iterator<Item = crate::BufferBarrier<'a, super::Buffer>>,
    {
        let barriers: Vec<_> = barriers
            .map(|b| crate::BufferBarrier {
                buffer: b.buffer.as_dyn(),
                usage: b.usage,
            })
            .collect();
        unsafe { self.inner.transition_buffers(&barriers) }
    }

    unsafe fn transition_textures<'a, T>(&mut self, barriers: T)
    where
        T: Iterator<Item = crate::TextureBarrier<'a, super::Texture>>,
    {
        let barriers: Vec<_> = barriers
            .map(|b| crate::TextureBarrier {
                texture: b.texture.as_dyn(),
                range: b.range,
                usage: b.usage,
            })
            .collect();
        unsafe { self.inner.transition_textures(&barriers) }
    }

    unsafe fn clear_buffer(&mut self, buffer: &super::Buffer, range: crate::MemoryRange) {
        unsafe { self.inner.clear_buffer(buffer.as_dyn(), range) }
    }

    unsafe fn copy_buffer_to_buffer<T>(
        &mut self,
        src: &super::Buffer,
        dst: &super::Buffer,
        regions: T,
    ) where
        T: Iterator<Item = crate::BufferCopy>,
    {
        let regions: Vec<_> = regions.collect();
        unsafe {
            self.inner
                .copy_buffer_to_buffer(src.as_dyn(), dst.as_dyn(), &regions)
        }
    }

    unsafe fn copy_texture_to_texture<T>(
        &mut self,
        src: &super::Texture,
        src_usage: wgt::TextureUses,
        dst: &super::Texture,
        regions: T,
    ) where
        T: Iterator<Item = crate::TextureCopy>,
    {
        let regions: Vec<_> = regions.collect();
        unsafe {
            self.inner
                .copy_texture_to_texture(src.as_dyn(), src_usage, dst.as_dyn(), &regions)
        }
    }

    unsafe fn copy_buffer_to_texture<T>(
        &mut self,
        src: &super::Buffer,
        dst: &super::Texture,
        regions: T,
    ) where
        T: Iterator<Item = crate::BufferTextureCopy>,
    {
        let regions: Vec<_> = regions.collect();
        unsafe {
            self.inner
                .copy_buffer_to_texture(src.as_dyn(), dst.as_dyn(), &regions)
        }
    }

    unsafe fn copy_texture_to_buffer<T>(
        &mut self,
        src: &super::Texture,
        src_usage: wgt::TextureUses,
        dst: &super::Buffer,
        regions: T,
    ) where
        T: Iterator<Item = crate::BufferTextureCopy>,
    {
        let regions: Vec<_> = regions.collect();
        unsafe {
            self.inner
                .copy_texture_to_buffer(src.as_dyn(), src_usage, dst.as_dyn(), &regions)
        }
    }

    unsafe fn copy_acceleration_structure_to_acceleration_structure(
        &mut self,
        src: &super::AccelerationStructure,
        dst: &super::AccelerationStructure,
        copy: wgt::AccelerationStructureCopy,
    ) {
        unsafe {
            self.inner
                .copy_acceleration_structure_to_acceleration_structure(
                    src.as_dyn(),
                    dst.as_dyn(),
                    copy,
                )
        }
    }

    unsafe fn set_bind_group(
        &mut self,
        layout: &super::PipelineLayout,
        index: u32,
        group: &super::BindGroup,
        dynamic_offsets: &[wgt::DynamicOffset],
    ) {
        unsafe {
            self.inner
                .set_bind_group(layout.as_dyn(), index, group.as_dyn(), dynamic_offsets)
        }
    }

    unsafe fn set_immediates(
        &mut self,
        layout: &super::PipelineLayout,
        offset_bytes: u32,
        data: &[u32],
    ) {
        unsafe {
            self.inner
                .set_immediates(layout.as_dyn(), offset_bytes, data)
        }
    }

    unsafe fn insert_debug_marker(&mut self, label: &str) {
        unsafe { self.inner.insert_debug_marker(label) }
    }

    unsafe fn begin_debug_marker(&mut self, group_label: &str) {
        unsafe { self.inner.begin_debug_marker(group_label) }
    }

    unsafe fn end_debug_marker(&mut self) {
        unsafe { self.inner.end_debug_marker() }
    }

    unsafe fn begin_query(&mut self, set: &super::QuerySet, index: u32) {
        unsafe { self.inner.begin_query(set.as_dyn(), index) }
    }

    unsafe fn end_query(&mut self, set: &super::QuerySet, index: u32) {
        unsafe { self.inner.end_query(set.as_dyn(), index) }
    }

    unsafe fn write_timestamp(&mut self, set: &super::QuerySet, index: u32) {
        unsafe { self.inner.write_timestamp(set.as_dyn(), index) }
    }

    unsafe fn reset_queries(&mut self, set: &super::QuerySet, range: core::ops::Range<u32>) {
        unsafe { self.inner.reset_queries(set.as_dyn(), range) }
    }

    unsafe fn copy_query_results(
        &mut self,
        set: &super::QuerySet,
        range: core::ops::Range<u32>,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        stride: wgt::BufferSize,
    ) {
        unsafe {
            self.inner
                .copy_query_results(set.as_dyn(), range, buffer.as_dyn(), offset, stride)
        }
    }

    unsafe fn begin_render_pass(
        &mut self,
        desc: &crate::RenderPassDescriptor<super::QuerySet, super::TextureView>,
    ) -> Result<(), crate::DeviceError> {
        let color_attachments: Vec<_> = desc
            .color_attachments
            .iter()
            .map(|a| a.as_ref().map(convert_color_attachment))
            .collect();
        let desc = crate::RenderPassDescriptor {
            label: desc.label,
            extent: desc.extent,
            sample_count: desc.sample_count,
            color_attachments: &color_attachments,
            depth_stencil_attachment: desc
                .depth_stencil_attachment
                .as_ref()
                .map(convert_depth_stencil_attachment),
            multiview_mask: desc.multiview_mask,
            timestamp_writes: desc.timestamp_writes.as_ref().map(convert_timestamp_writes),
            occlusion_query_set: desc.occlusion_query_set.map(|q| q.as_dyn()),
        };
        unsafe { self.inner.begin_render_pass(&desc) }
    }

    unsafe fn end_render_pass(&mut self) {
        unsafe { self.inner.end_render_pass() }
    }

    unsafe fn set_render_pipeline(&mut self, pipeline: &super::RenderPipeline) {
        unsafe { self.inner.set_render_pipeline(pipeline.as_dyn()) }
    }

    unsafe fn set_index_buffer<'a>(
        &mut self,
        binding: crate::BufferBinding<'a, super::Buffer>,
        format: wgt::IndexFormat,
    ) {
        let binding = crate::BufferBinding {
            buffer: binding.buffer.as_dyn(),
            offset: binding.offset,
            size: binding.size,
        };
        unsafe { self.inner.set_index_buffer(binding, format) }
    }

    unsafe fn set_vertex_buffer<'a>(
        &mut self,
        index: u32,
        binding: crate::BufferBinding<'a, super::Buffer>,
    ) {
        let binding = crate::BufferBinding {
            buffer: binding.buffer.as_dyn(),
            offset: binding.offset,
            size: binding.size,
        };
        unsafe { self.inner.set_vertex_buffer(index, binding) }
    }

    unsafe fn set_viewport(&mut self, rect: &crate::Rect<f32>, depth_range: core::ops::Range<f32>) {
        unsafe { self.inner.set_viewport(rect, depth_range) }
    }

    unsafe fn set_scissor_rect(&mut self, rect: &crate::Rect<u32>) {
        unsafe { self.inner.set_scissor_rect(rect) }
    }

    unsafe fn set_stencil_reference(&mut self, value: u32) {
        unsafe { self.inner.set_stencil_reference(value) }
    }

    unsafe fn set_blend_constants(&mut self, color: &[f32; 4]) {
        unsafe { self.inner.set_blend_constants(color) }
    }

    unsafe fn draw(
        &mut self,
        first_vertex: u32,
        vertex_count: u32,
        first_instance: u32,
        instance_count: u32,
    ) {
        unsafe {
            self.inner
                .draw(first_vertex, vertex_count, first_instance, instance_count)
        }
    }

    unsafe fn draw_indexed(
        &mut self,
        first_index: u32,
        index_count: u32,
        base_vertex: i32,
        first_instance: u32,
        instance_count: u32,
    ) {
        unsafe {
            self.inner.draw_indexed(
                first_index,
                index_count,
                base_vertex,
                first_instance,
                instance_count,
            )
        }
    }

    unsafe fn draw_indirect(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        draw_count: u32,
    ) {
        unsafe { self.inner.draw_indirect(buffer.as_dyn(), offset, draw_count) }
    }

    unsafe fn draw_indexed_indirect(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        draw_count: u32,
    ) {
        unsafe {
            self.inner
                .draw_indexed_indirect(buffer.as_dyn(), offset, draw_count)
        }
    }

    unsafe fn draw_indirect_count(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        count_buffer: &super::Buffer,
        count_offset: wgt::BufferAddress,
        max_count: u32,
    ) {
        unsafe {
            self.inner.draw_indirect_count(
                buffer.as_dyn(),
                offset,
                count_buffer.as_dyn(),
                count_offset,
                max_count,
            )
        }
    }

    unsafe fn draw_indexed_indirect_count(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        count_buffer: &super::Buffer,
        count_offset: wgt::BufferAddress,
        max_count: u32,
    ) {
        unsafe {
            self.inner.draw_indexed_indirect_count(
                buffer.as_dyn(),
                offset,
                count_buffer.as_dyn(),
                count_offset,
                max_count,
            )
        }
    }

    unsafe fn draw_mesh_tasks(
        &mut self,
        group_count_x: u32,
        group_count_y: u32,
        group_count_z: u32,
    ) {
        unsafe {
            self.inner
                .draw_mesh_tasks(group_count_x, group_count_y, group_count_z)
        }
    }

    unsafe fn draw_mesh_tasks_indirect(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        draw_count: u32,
    ) {
        unsafe {
            self.inner
                .draw_mesh_tasks_indirect(buffer.as_dyn(), offset, draw_count)
        }
    }

    unsafe fn draw_mesh_tasks_indirect_count(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        count_buffer: &super::Buffer,
        count_offset: wgt::BufferAddress,
        max_count: u32,
    ) {
        unsafe {
            self.inner.draw_mesh_tasks_indirect_count(
                buffer.as_dyn(),
                offset,
                count_buffer.as_dyn(),
                count_offset,
                max_count,
            )
        }
    }

    unsafe fn begin_compute_pass(&mut self, desc: &crate::ComputePassDescriptor<super::QuerySet>) {
        let desc = crate::ComputePassDescriptor {
            label: desc.label,
            timestamp_writes: desc.timestamp_writes.as_ref().map(convert_timestamp_writes),
        };
        unsafe { self.inner.begin_compute_pass(&desc) }
    }

    unsafe fn end_compute_pass(&mut self) {
        unsafe { self.inner.end_compute_pass() }
    }

    unsafe fn set_compute_pipeline(&mut self, pipeline: &super::ComputePipeline) {
        unsafe { self.inner.set_compute_pipeline(pipeline.as_dyn()) }
    }

    unsafe fn dispatch_workgroups(&mut self, count: [u32; 3]) {
        unsafe { self.inner.dispatch_workgroups(count) }
    }

    unsafe fn dispatch_workgroups_indirect(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
    ) {
        unsafe {
            self.inner
                .dispatch_workgroups_indirect(buffer.as_dyn(), offset)
        }
    }

    unsafe fn begin_ray_tracing_pass(&mut self, desc: &crate::RayTracingPassDescriptor<'_>) {
        unsafe { self.inner.begin_ray_tracing_pass(desc) }
    }

    unsafe fn end_ray_tracing_pass(&mut self) {
        unsafe { self.inner.end_ray_tracing_pass() }
    }

    unsafe fn set_ray_tracing_pipeline(&mut self, pipeline: &super::RayTracingPipeline) {
        unsafe { self.inner.set_ray_tracing_pipeline(pipeline.as_dyn()) }
    }

    unsafe fn trace_rays<'a>(
        &mut self,
        count: [u32; 3],
        ray_generation_group_data: crate::PipelineGroupData<'a, super::Buffer>,
        miss_group_data: crate::PipelineGroupData<'a, super::Buffer>,
        intersection_group_data: crate::PipelineGroupData<'a, super::Buffer>,
    ) {
        unsafe {
            self.inner.trace_rays(
                count,
                convert_group_data(ray_generation_group_data),
                convert_group_data(miss_group_data),
                convert_group_data(intersection_group_data),
            )
        }
    }

    unsafe fn set_acceleration_structure_dependencies(
        command_buffers: &[&super::CommandBuffer],
        dependencies: &[&super::AccelerationStructure],
    ) {
        // The static `CommandEncoder` trait declares this as a
        // receiverless associated function (mirrored, in the dynamic
        // trait, by an `&self` parameter that's simply ignored). Since
        // there is no `self` here, we have no `inner` encoder to forward
        // to, and thus no sound way to make this call transparent.
        todo!()
    }

    unsafe fn build_acceleration_structures<'a, T>(&mut self, descriptor_count: u32, descriptors: T)
    where
        Self::A: 'a,
        T: IntoIterator<
            Item = crate::BuildAccelerationStructureDescriptor<
                'a,
                super::Buffer,
                super::AccelerationStructure,
            >,
        >,
    {
        let descriptors: Vec<_> = descriptors.into_iter().collect();
        let descriptor_entries: Vec<_> = descriptors
            .iter()
            .map(|d| super::convert_entries(d.entries))
            .collect();
        let descriptors: Vec<_> = descriptors
            .iter()
            .zip(descriptor_entries.iter())
            .map(|(d, entries)| crate::BuildAccelerationStructureDescriptor {
                entries,
                mode: d.mode,
                flags: d.flags,
                source_acceleration_structure: d
                    .source_acceleration_structure
                    .map(|a| a.as_dyn()),
                destination_acceleration_structure: d.destination_acceleration_structure.as_dyn(),
                scratch_buffer: d.scratch_buffer.as_dyn(),
                scratch_buffer_offset: d.scratch_buffer_offset,
            })
            .collect();
        unsafe { self.inner.build_acceleration_structures(&descriptors) }
    }

    unsafe fn place_acceleration_structure_barrier(
        &mut self,
        barrier: crate::AccelerationStructureBarrier,
    ) {
        unsafe { self.inner.place_acceleration_structure_barrier(barrier) }
    }

    unsafe fn read_acceleration_structure_compact_size(
        &mut self,
        acceleration_structure: &super::AccelerationStructure,
        buf: &super::Buffer,
    ) {
        unsafe {
            self.inner
                .read_acceleration_structure_compact_size(acceleration_structure.as_dyn(), buf.as_dyn())
        }
    }
}
