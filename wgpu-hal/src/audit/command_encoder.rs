/*! Implementation of [`validation_layer::Device`]. */
#![allow(unused_variables)]

impl crate::CommandEncoder for super::CommandEncoder {
    type A = super::Api;

    unsafe fn begin_encoding(&mut self, label: crate::Label) -> Result<(), crate::DeviceError> {
        todo!()
    }

    unsafe fn discard_encoding(&mut self) {
        todo!()
    }

    unsafe fn end_encoding(&mut self) -> Result<super::CommandBuffer, crate::DeviceError> {
        todo!()
    }

    unsafe fn reset_all<I>(&mut self, command_buffers: I)
    where
        I: Iterator<Item = super::CommandBuffer>,
    {
        todo!()
    }

    unsafe fn transition_buffers<'a, T>(&mut self, barriers: T)
    where
        T: Iterator<Item = crate::BufferBarrier<'a, super::Buffer>>,
    {
        todo!()
    }

    unsafe fn transition_textures<'a, T>(&mut self, barriers: T)
    where
        T: Iterator<Item = crate::TextureBarrier<'a, super::Texture>>,
    {
        todo!()
    }

    unsafe fn clear_buffer(&mut self, buffer: &super::Buffer, range: crate::MemoryRange) {
        todo!()
    }

    unsafe fn copy_buffer_to_buffer<T>(
        &mut self,
        src: &super::Buffer,
        dst: &super::Buffer,
        regions: T,
    ) where
        T: Iterator<Item = crate::BufferCopy>,
    {
        todo!()
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
        todo!()
    }

    unsafe fn copy_buffer_to_texture<T>(
        &mut self,
        src: &super::Buffer,
        dst: &super::Texture,
        regions: T,
    ) where
        T: Iterator<Item = crate::BufferTextureCopy>,
    {
        todo!()
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
        todo!()
    }

    unsafe fn copy_acceleration_structure_to_acceleration_structure(
        &mut self,
        src: &super::AccelerationStructure,
        dst: &super::AccelerationStructure,
        copy: wgt::AccelerationStructureCopy,
    ) {
        todo!()
    }

    unsafe fn set_bind_group(
        &mut self,
        layout: &super::PipelineLayout,
        index: u32,
        group: &super::BindGroup,
        dynamic_offsets: &[wgt::DynamicOffset],
    ) {
        todo!()
    }

    unsafe fn set_immediates(
        &mut self,
        layout: &super::PipelineLayout,
        offset_bytes: u32,
        data: &[u32],
    ) {
        todo!()
    }

    unsafe fn insert_debug_marker(&mut self, label: &str) {
        todo!()
    }

    unsafe fn begin_debug_marker(&mut self, group_label: &str) {
        todo!()
    }

    unsafe fn end_debug_marker(&mut self) {
        todo!()
    }

    unsafe fn begin_query(&mut self, set: &super::QuerySet, index: u32) {
        todo!()
    }

    unsafe fn end_query(&mut self, set: &super::QuerySet, index: u32) {
        todo!()
    }

    unsafe fn write_timestamp(&mut self, set: &super::QuerySet, index: u32) {
        todo!()
    }

    unsafe fn reset_queries(&mut self, set: &super::QuerySet, range: core::ops::Range<u32>) {
        todo!()
    }

    unsafe fn copy_query_results(
        &mut self,
        set: &super::QuerySet,
        range: core::ops::Range<u32>,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        stride: wgt::BufferSize,
    ) {
        todo!()
    }

    unsafe fn begin_render_pass(
        &mut self,
        desc: &crate::RenderPassDescriptor<super::QuerySet, super::TextureView>,
    ) -> Result<(), crate::DeviceError> {
        todo!()
    }

    unsafe fn end_render_pass(&mut self) {
        todo!()
    }

    unsafe fn set_render_pipeline(&mut self, pipeline: &super::RenderPipeline) {
        todo!()
    }

    unsafe fn set_index_buffer<'a>(
        &mut self,
        binding: crate::BufferBinding<'a, super::Buffer>,
        format: wgt::IndexFormat,
    ) {
        todo!()
    }

    unsafe fn set_vertex_buffer<'a>(
        &mut self,
        index: u32,
        binding: crate::BufferBinding<'a, super::Buffer>,
    ) {
        todo!()
    }

    unsafe fn set_viewport(&mut self, rect: &crate::Rect<f32>, depth_range: core::ops::Range<f32>) {
        todo!()
    }

    unsafe fn set_scissor_rect(&mut self, rect: &crate::Rect<u32>) {
        todo!()
    }

    unsafe fn set_stencil_reference(&mut self, value: u32) {
        todo!()
    }

    unsafe fn set_blend_constants(&mut self, color: &[f32; 4]) {
        todo!()
    }

    unsafe fn draw(
        &mut self,
        first_vertex: u32,
        vertex_count: u32,
        first_instance: u32,
        instance_count: u32,
    ) {
        todo!()
    }

    unsafe fn draw_indexed(
        &mut self,
        first_index: u32,
        index_count: u32,
        base_vertex: i32,
        first_instance: u32,
        instance_count: u32,
    ) {
        todo!()
    }

    unsafe fn draw_indirect(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        draw_count: u32,
    ) {
        todo!()
    }

    unsafe fn draw_indexed_indirect(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        draw_count: u32,
    ) {
        todo!()
    }

    unsafe fn draw_indirect_count(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        count_buffer: &super::Buffer,
        count_offset: wgt::BufferAddress,
        max_count: u32,
    ) {
        todo!()
    }

    unsafe fn draw_indexed_indirect_count(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        count_buffer: &super::Buffer,
        count_offset: wgt::BufferAddress,
        max_count: u32,
    ) {
        todo!()
    }

    unsafe fn draw_mesh_tasks(
        &mut self,
        group_count_x: u32,
        group_count_y: u32,
        group_count_z: u32,
    ) {
        todo!()
    }

    unsafe fn draw_mesh_tasks_indirect(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        draw_count: u32,
    ) {
        todo!()
    }

    unsafe fn draw_mesh_tasks_indirect_count(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
        count_buffer: &super::Buffer,
        count_offset: wgt::BufferAddress,
        max_count: u32,
    ) {
        todo!()
    }

    unsafe fn begin_compute_pass(&mut self, desc: &crate::ComputePassDescriptor<super::QuerySet>) {
        todo!()
    }

    unsafe fn end_compute_pass(&mut self) {
        todo!()
    }

    unsafe fn set_compute_pipeline(&mut self, pipeline: &super::ComputePipeline) {
        todo!()
    }

    unsafe fn dispatch_workgroups(&mut self, count: [u32; 3]) {
        todo!()
    }

    unsafe fn dispatch_workgroups_indirect(
        &mut self,
        buffer: &super::Buffer,
        offset: wgt::BufferAddress,
    ) {
        todo!()
    }

    unsafe fn begin_ray_tracing_pass(&mut self, desc: &crate::RayTracingPassDescriptor<'_>) {
        todo!()
    }

    unsafe fn end_ray_tracing_pass(&mut self) {
        todo!()
    }

    unsafe fn set_ray_tracing_pipeline(&mut self, pipeline: &super::RayTracingPipeline) {
        todo!()
    }

    unsafe fn trace_rays<'a>(
        &mut self,
        count: [u32; 3],
        ray_generation_group_data: crate::PipelineGroupData<'a, super::Buffer>,
        miss_group_data: crate::PipelineGroupData<'a, super::Buffer>,
        intersection_group_data: crate::PipelineGroupData<'a, super::Buffer>,
    ) {
        todo!()
    }

    unsafe fn set_acceleration_structure_dependencies(
        command_buffers: &[&super::CommandBuffer],
        dependencies: &[&super::AccelerationStructure],
    ) {
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
        todo!()
    }

    unsafe fn place_acceleration_structure_barrier(
        &mut self,
        barrier: crate::AccelerationStructureBarrier,
    ) {
        todo!()
    }

    unsafe fn read_acceleration_structure_compact_size(
        &mut self,
        acceleration_structure: &super::AccelerationStructure,
        buf: &super::Buffer,
    ) {
        todo!()
    }
}
