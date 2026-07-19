/*! Implementation of [`validation_layer::Device`]. */
#![allow(unused_variables)]

use alloc::vec::Vec;

impl crate::Device for super::Device {
    type A = super::Api;

    unsafe fn create_buffer(
        &self,
        desc: &crate::BufferDescriptor,
    ) -> Result<super::Buffer, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_buffer(&self, buffer: super::Buffer) {
        todo!()
    }

    unsafe fn add_raw_buffer(&self, buffer: &super::Buffer) {
        todo!()
    }

    unsafe fn map_buffer(
        &self,
        buffer: &super::Buffer,
        range: crate::MemoryRange,
    ) -> Result<crate::BufferMapping, crate::DeviceError> {
        todo!()
    }

    unsafe fn unmap_buffer(&self, buffer: &super::Buffer) {
        todo!()
    }

    unsafe fn flush_mapped_ranges<I>(&self, buffer: &super::Buffer, ranges: I)
    where
        I: Iterator<Item = crate::MemoryRange>,
    {
        todo!()
    }

    unsafe fn invalidate_mapped_ranges<I>(&self, buffer: &super::Buffer, ranges: I)
    where
        I: Iterator<Item = crate::MemoryRange>,
    {
        todo!()
    }

    unsafe fn create_texture(
        &self,
        desc: &crate::TextureDescriptor,
    ) -> Result<super::Texture, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_texture(&self, texture: super::Texture) {
        todo!()
    }

    unsafe fn add_raw_texture(&self, texture: &super::Texture) {
        todo!()
    }

    unsafe fn create_texture_view(
        &self,
        texture: &super::Texture,
        desc: &crate::TextureViewDescriptor,
    ) -> Result<super::TextureView, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_texture_view(&self, view: super::TextureView) {
        todo!()
    }

    unsafe fn create_sampler(
        &self,
        desc: &crate::SamplerDescriptor,
    ) -> Result<super::Sampler, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_sampler(&self, sampler: super::Sampler) {
        todo!()
    }

    unsafe fn create_command_encoder(
        &self,
        desc: &crate::CommandEncoderDescriptor<super::Queue>,
    ) -> Result<super::CommandEncoder, crate::DeviceError> {
        todo!()
    }

    unsafe fn create_bind_group_layout(
        &self,
        desc: &crate::BindGroupLayoutDescriptor,
    ) -> Result<super::BindGroupLayout, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_bind_group_layout(&self, bg_layout: super::BindGroupLayout) {
        todo!()
    }

    unsafe fn create_pipeline_layout(
        &self,
        desc: &crate::PipelineLayoutDescriptor<super::BindGroupLayout>,
    ) -> Result<super::PipelineLayout, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_pipeline_layout(&self, pipeline_layout: super::PipelineLayout) {
        todo!()
    }

    unsafe fn create_bind_group(
        &self,
        desc: &crate::BindGroupDescriptor<
            super::BindGroupLayout,
            super::Buffer,
            super::Sampler,
            super::TextureView,
            super::AccelerationStructure,
        >,
    ) -> Result<super::BindGroup, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_bind_group(&self, group: super::BindGroup) {
        todo!()
    }

    unsafe fn create_shader_module(
        &self,
        desc: &crate::ShaderModuleDescriptor,
        shader: crate::ShaderInput,
    ) -> Result<super::ShaderModule, crate::ShaderError> {
        todo!()
    }

    unsafe fn destroy_shader_module(&self, module: super::ShaderModule) {
        todo!()
    }

    unsafe fn create_render_pipeline(
        &self,
        desc: &crate::RenderPipelineDescriptor<
            super::PipelineLayout,
            super::ShaderModule,
            super::PipelineCache,
        >,
    ) -> Result<super::RenderPipeline, crate::PipelineError> {
        todo!()
    }

    unsafe fn destroy_render_pipeline(&self, pipeline: super::RenderPipeline) {
        todo!()
    }

    unsafe fn create_compute_pipeline(
        &self,
        desc: &crate::ComputePipelineDescriptor<
            super::PipelineLayout,
            super::ShaderModule,
            super::PipelineCache,
        >,
    ) -> Result<super::ComputePipeline, crate::PipelineError> {
        todo!()
    }

    unsafe fn destroy_compute_pipeline(&self, pipeline: super::ComputePipeline) {
        todo!()
    }

    unsafe fn create_ray_tracing_pipeline(
        &self,
        desc: &crate::RayTracingPipelineDescriptor<
            super::PipelineLayout,
            super::ShaderModule,
            super::PipelineCache,
        >,
    ) -> Result<super::RayTracingPipeline, crate::PipelineError> {
        todo!()
    }

    unsafe fn destroy_ray_tracing_pipeline(&self, pipeline: super::RayTracingPipeline) {
        todo!()
    }

    unsafe fn get_raytracing_pipeline_group_data(
        &self,
        pipeline: &super::RayTracingPipeline,
        groups: core::ops::Range<u32>,
    ) -> Result<Vec<u8>, crate::DeviceError> {
        todo!()
    }

    unsafe fn create_pipeline_cache(
        &self,
        desc: &crate::PipelineCacheDescriptor<'_>,
    ) -> Result<super::PipelineCache, crate::PipelineCacheError> {
        todo!()
    }

    unsafe fn destroy_pipeline_cache(&self, cache: super::PipelineCache) {
        todo!()
    }

    unsafe fn create_query_set(
        &self,
        desc: &wgt::QuerySetDescriptor<crate::Label>,
    ) -> Result<super::QuerySet, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_query_set(&self, set: super::QuerySet) {
        todo!()
    }

    unsafe fn create_fence(&self) -> Result<super::Fence, crate::DeviceError> {
        todo!()
    }

    unsafe fn destroy_fence(&self, fence: super::Fence) {
        todo!()
    }

    unsafe fn get_fence_value(
        &self,
        fence: &super::Fence,
    ) -> Result<crate::FenceValue, crate::DeviceError> {
        todo!()
    }

    unsafe fn wait(
        &self,
        fence: &super::Fence,
        value: crate::FenceValue,
        timeout: Option<core::time::Duration>,
    ) -> Result<bool, crate::DeviceError> {
        todo!()
    }

    unsafe fn start_graphics_debugger_capture(&self) -> bool {
        todo!()
    }

    unsafe fn stop_graphics_debugger_capture(&self) {
        todo!()
    }

    unsafe fn create_acceleration_structure(
        &self,
        desc: &crate::AccelerationStructureDescriptor,
    ) -> Result<super::AccelerationStructure, crate::DeviceError> {
        todo!()
    }

    unsafe fn get_acceleration_structure_build_sizes(
        &self,
        desc: &crate::GetAccelerationStructureBuildSizesDescriptor<super::Buffer>,
    ) -> crate::AccelerationStructureBuildSizes {
        todo!()
    }

    unsafe fn get_acceleration_structure_device_address(
        &self,
        acceleration_structure: &super::AccelerationStructure,
    ) -> wgt::BufferAddress {
        todo!()
    }

    unsafe fn destroy_acceleration_structure(
        &self,
        acceleration_structure: super::AccelerationStructure,
    ) {
        todo!()
    }

    fn tlas_instance_to_bytes(&self, instance: crate::TlasInstance) -> Vec<u8> {
        todo!()
    }

    fn get_internal_counters(&self) -> wgt::HalCounters {
        todo!()
    }

    fn check_if_oom(&self) -> Result<(), crate::DeviceError> {
        todo!()
    }
}
