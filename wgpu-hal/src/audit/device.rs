/*! Implementation of [`validation_layer::Device`]. */
#![allow(unused_variables)]

use alloc::vec::Vec;

/// Convert a `ProgrammableStage` referring to audited shader modules into
/// one referring to the erased dynamic type expected by `self.inner`.
fn convert_stage<'a>(
    stage: &crate::ProgrammableStage<'a, super::ShaderModule>,
) -> crate::ProgrammableStage<'a, dyn crate::DynShaderModule> {
    crate::ProgrammableStage {
        module: stage.module.as_dyn(),
        entry_point: stage.entry_point,
        constants: stage.constants,
        zero_initialize_workgroup_memory: stage.zero_initialize_workgroup_memory,
    }
}

impl crate::Device for super::Device {
    type A = super::Api;

    unsafe fn create_buffer(
        &self,
        desc: &crate::BufferDescriptor,
    ) -> Result<super::Buffer, crate::DeviceError> {
        let inner = unsafe { self.inner.create_buffer(desc)? };
        Ok(super::Buffer::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_buffer(&self, buffer: super::Buffer) {
        unsafe { self.inner.destroy_buffer(buffer.inner) }
    }

    unsafe fn add_raw_buffer(&self, buffer: &super::Buffer) {
        unsafe { self.inner.add_raw_buffer(buffer.as_dyn()) }
    }

    unsafe fn map_buffer(
        &self,
        buffer: &super::Buffer,
        range: crate::MemoryRange,
    ) -> Result<crate::BufferMapping, crate::DeviceError> {
        unsafe { self.inner.map_buffer(buffer.as_dyn(), range) }
    }

    unsafe fn unmap_buffer(&self, buffer: &super::Buffer) {
        unsafe { self.inner.unmap_buffer(buffer.as_dyn()) }
    }

    unsafe fn flush_mapped_ranges<I>(&self, buffer: &super::Buffer, ranges: I)
    where
        I: Iterator<Item = crate::MemoryRange>,
    {
        let ranges: Vec<_> = ranges.collect();
        unsafe { self.inner.flush_mapped_ranges(buffer.as_dyn(), &ranges) }
    }

    unsafe fn invalidate_mapped_ranges<I>(&self, buffer: &super::Buffer, ranges: I)
    where
        I: Iterator<Item = crate::MemoryRange>,
    {
        let ranges: Vec<_> = ranges.collect();
        unsafe {
            self.inner.invalidate_mapped_ranges(buffer.as_dyn(), &ranges)
        }
    }

    unsafe fn create_texture(
        &self,
        desc: &crate::TextureDescriptor,
    ) -> Result<super::Texture, crate::DeviceError> {
        let inner = unsafe { self.inner.create_texture(desc)? };
        Ok(super::Texture::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_texture(&self, texture: super::Texture) {
        unsafe { self.inner.destroy_texture(texture.inner) }
    }

    unsafe fn add_raw_texture(&self, texture: &super::Texture) {
        unsafe { self.inner.add_raw_texture(texture.as_dyn()) }
    }

    unsafe fn create_texture_view(
        &self,
        texture: &super::Texture,
        desc: &crate::TextureViewDescriptor,
    ) -> Result<super::TextureView, crate::DeviceError> {
        let inner = unsafe { self.inner.create_texture_view(texture.as_dyn(), desc)? };
        Ok(super::TextureView::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_texture_view(&self, view: super::TextureView) {
        unsafe { self.inner.destroy_texture_view(view.inner) }
    }

    unsafe fn create_sampler(
        &self,
        desc: &crate::SamplerDescriptor,
    ) -> Result<super::Sampler, crate::DeviceError> {
        let inner = unsafe { self.inner.create_sampler(desc)? };
        Ok(super::Sampler::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_sampler(&self, sampler: super::Sampler) {
        unsafe { self.inner.destroy_sampler(sampler.inner) }
    }

    unsafe fn create_command_encoder(
        &self,
        desc: &crate::CommandEncoderDescriptor<super::Queue>,
    ) -> Result<super::CommandEncoder, crate::DeviceError> {
        let desc = crate::CommandEncoderDescriptor {
            label: desc.label,
            queue: desc.queue.as_dyn(),
        };
        let inner = unsafe { self.inner.create_command_encoder(&desc)? };
        Ok(super::CommandEncoder::wrap(inner, self.shared.clone()))
    }

    unsafe fn create_bind_group_layout(
        &self,
        desc: &crate::BindGroupLayoutDescriptor,
    ) -> Result<super::BindGroupLayout, crate::DeviceError> {
        let inner = unsafe { self.inner.create_bind_group_layout(desc)? };
        Ok(super::BindGroupLayout::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_bind_group_layout(&self, bg_layout: super::BindGroupLayout) {
        unsafe { self.inner.destroy_bind_group_layout(bg_layout.inner) }
    }

    unsafe fn create_pipeline_layout(
        &self,
        desc: &crate::PipelineLayoutDescriptor<super::BindGroupLayout>,
    ) -> Result<super::PipelineLayout, crate::DeviceError> {
        let bind_group_layouts: Vec<Option<&dyn crate::DynBindGroupLayout>> = desc
            .bind_group_layouts
            .iter()
            .map(|bgl| bgl.map(|bgl| bgl.as_dyn()))
            .collect();
        let desc = crate::PipelineLayoutDescriptor {
            label: desc.label,
            flags: desc.flags,
            bind_group_layouts: &bind_group_layouts,
            immediate_size: desc.immediate_size,
        };
        let inner = unsafe { self.inner.create_pipeline_layout(&desc)? };
        Ok(super::PipelineLayout::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_pipeline_layout(&self, pipeline_layout: super::PipelineLayout) {
        unsafe { self.inner.destroy_pipeline_layout(pipeline_layout.inner) }
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
        let buffers: Vec<_> = desc
            .buffers
            .iter()
            .map(|b| crate::BufferBinding {
                buffer: b.buffer.as_dyn(),
                offset: b.offset,
                size: b.size,
            })
            .collect();
        let samplers: Vec<&dyn crate::DynSampler> =
            desc.samplers.iter().map(|s| s.as_dyn()).collect();
        let textures: Vec<_> = desc
            .textures
            .iter()
            .map(|t| crate::TextureBinding {
                view: t.view.as_dyn(),
                usage: t.usage,
            })
            .collect();
        let acceleration_structures: Vec<&dyn crate::DynAccelerationStructure> = desc
            .acceleration_structures
            .iter()
            .map(|a| a.as_dyn())
            .collect();
        let external_textures: Vec<_> = desc
            .external_textures
            .iter()
            .map(|et| crate::ExternalTextureBinding {
                planes: et.planes.clone().map(|p| crate::TextureBinding {
                    view: p.view.as_dyn(),
                    usage: p.usage,
                }),
                params: crate::BufferBinding {
                    buffer: et.params.buffer.as_dyn(),
                    offset: et.params.offset,
                    size: et.params.size,
                },
            })
            .collect();

        let desc = crate::BindGroupDescriptor {
            label: desc.label,
            layout: desc.layout.as_dyn(),
            buffers: &buffers,
            samplers: &samplers,
            textures: &textures,
            entries: desc.entries,
            acceleration_structures: &acceleration_structures,
            external_textures: &external_textures,
        };

        let inner = unsafe { self.inner.create_bind_group(&desc)? };
        Ok(super::BindGroup::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_bind_group(&self, group: super::BindGroup) {
        unsafe { self.inner.destroy_bind_group(group.inner) }
    }

    unsafe fn create_shader_module(
        &self,
        desc: &crate::ShaderModuleDescriptor,
        shader: crate::ShaderInput,
    ) -> Result<super::ShaderModule, crate::ShaderError> {
        let inner = unsafe { self.inner.create_shader_module(desc, shader)? };
        Ok(super::ShaderModule::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_shader_module(&self, module: super::ShaderModule) {
        unsafe { self.inner.destroy_shader_module(module.inner) }
    }

    unsafe fn create_render_pipeline(
        &self,
        desc: &crate::RenderPipelineDescriptor<
            super::PipelineLayout,
            super::ShaderModule,
            super::PipelineCache,
        >,
    ) -> Result<super::RenderPipeline, crate::PipelineError> {
        let vertex_processor = match &desc.vertex_processor {
            crate::VertexProcessor::Standard {
                vertex_buffers,
                vertex_stage,
            } => crate::VertexProcessor::Standard {
                vertex_buffers,
                vertex_stage: convert_stage(vertex_stage),
            },
            crate::VertexProcessor::Mesh {
                task_stage,
                mesh_stage,
            } => crate::VertexProcessor::Mesh {
                task_stage: task_stage.as_ref().map(convert_stage),
                mesh_stage: convert_stage(mesh_stage),
            },
        };
        let desc = crate::RenderPipelineDescriptor {
            label: desc.label,
            layout: desc.layout.as_dyn(),
            vertex_processor,
            primitive: desc.primitive,
            depth_stencil: desc.depth_stencil.clone(),
            multisample: desc.multisample,
            fragment_stage: desc.fragment_stage.as_ref().map(convert_stage),
            color_targets: desc.color_targets,
            multiview_mask: desc.multiview_mask,
            cache: desc.cache.map(|c| c.as_dyn()),
        };
        let inner = unsafe { self.inner.create_render_pipeline(&desc)? };
        Ok(super::RenderPipeline::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_render_pipeline(&self, pipeline: super::RenderPipeline) {
        unsafe { self.inner.destroy_render_pipeline(pipeline.inner) }
    }

    unsafe fn create_compute_pipeline(
        &self,
        desc: &crate::ComputePipelineDescriptor<
            super::PipelineLayout,
            super::ShaderModule,
            super::PipelineCache,
        >,
    ) -> Result<super::ComputePipeline, crate::PipelineError> {
        let desc = crate::ComputePipelineDescriptor {
            label: desc.label,
            layout: desc.layout.as_dyn(),
            stage: convert_stage(&desc.stage),
            cache: desc.cache.map(|c| c.as_dyn()),
        };
        let inner = unsafe { self.inner.create_compute_pipeline(&desc)? };
        Ok(super::ComputePipeline::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_compute_pipeline(&self, pipeline: super::ComputePipeline) {
        unsafe { self.inner.destroy_compute_pipeline(pipeline.inner) }
    }

    unsafe fn create_ray_tracing_pipeline(
        &self,
        desc: &crate::RayTracingPipelineDescriptor<
            super::PipelineLayout,
            super::ShaderModule,
            super::PipelineCache,
        >,
    ) -> Result<super::RayTracingPipeline, crate::PipelineError> {
        let intersection: Vec<_> = desc
            .intersection
            .iter()
            .map(|stage| crate::RayObjectIntersectionState {
                closest_hit: convert_stage(&stage.closest_hit),
                any_hit: stage.any_hit.as_ref().map(convert_stage),
            })
            .collect();
        let desc = crate::RayTracingPipelineDescriptor {
            label: desc.label,
            layout: desc.layout.as_dyn(),
            ray_generation: convert_stage(&desc.ray_generation),
            miss: convert_stage(&desc.miss),
            intersection: &intersection,
            max_recursion_depth: desc.max_recursion_depth,
            cache: desc.cache.map(|c| c.as_dyn()),
        };
        let inner = unsafe { self.inner.create_ray_tracing_pipeline(&desc)? };
        Ok(super::RayTracingPipeline::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_ray_tracing_pipeline(&self, pipeline: super::RayTracingPipeline) {
        unsafe { self.inner.destroy_ray_tracing_pipeline(pipeline.inner) }
    }

    unsafe fn get_raytracing_pipeline_group_data(
        &self,
        pipeline: &super::RayTracingPipeline,
        groups: core::ops::Range<u32>,
    ) -> Result<Vec<u8>, crate::DeviceError> {
        unsafe {
            self.inner
                .get_raytracing_pipeline_group_data(pipeline.as_dyn(), groups)
        }
    }

    unsafe fn create_pipeline_cache(
        &self,
        desc: &crate::PipelineCacheDescriptor<'_>,
    ) -> Result<super::PipelineCache, crate::PipelineCacheError> {
        let inner = unsafe { self.inner.create_pipeline_cache(desc)? };
        Ok(super::PipelineCache::wrap(inner, self.shared.clone()))
    }

    fn pipeline_cache_validation_key(&self) -> Option<[u8; 16]> {
        self.inner.pipeline_cache_validation_key()
    }

    unsafe fn destroy_pipeline_cache(&self, cache: super::PipelineCache) {
        unsafe { self.inner.destroy_pipeline_cache(cache.inner) }
    }

    unsafe fn pipeline_cache_get_data(&self, cache: &super::PipelineCache) -> Option<Vec<u8>> {
        unsafe { self.inner.pipeline_cache_get_data(cache.as_dyn()) }
    }

    unsafe fn create_query_set(
        &self,
        desc: &wgt::QuerySetDescriptor<crate::Label>,
    ) -> Result<super::QuerySet, crate::DeviceError> {
        let inner = unsafe { self.inner.create_query_set(desc)? };
        Ok(super::QuerySet::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_query_set(&self, set: super::QuerySet) {
        unsafe { self.inner.destroy_query_set(set.inner) }
    }

    unsafe fn create_fence(&self) -> Result<super::Fence, crate::DeviceError> {
        let inner = unsafe { self.inner.create_fence()? };
        Ok(super::Fence::wrap(inner, self.shared.clone()))
    }

    unsafe fn destroy_fence(&self, fence: super::Fence) {
        unsafe { self.inner.destroy_fence(fence.inner) }
    }

    unsafe fn get_fence_value(
        &self,
        fence: &super::Fence,
    ) -> Result<crate::FenceValue, crate::DeviceError> {
        unsafe { self.inner.get_fence_value(fence.as_dyn()) }
    }

    unsafe fn wait(
        &self,
        fence: &super::Fence,
        value: crate::FenceValue,
        timeout: Option<core::time::Duration>,
    ) -> Result<bool, crate::DeviceError> {
        unsafe { self.inner.wait(fence.as_dyn(), value, timeout) }
    }

    unsafe fn start_graphics_debugger_capture(&self) -> bool {
        unsafe { self.inner.start_graphics_debugger_capture() }
    }

    unsafe fn stop_graphics_debugger_capture(&self) {
        unsafe { self.inner.stop_graphics_debugger_capture() }
    }

    unsafe fn create_acceleration_structure(
        &self,
        desc: &crate::AccelerationStructureDescriptor,
    ) -> Result<super::AccelerationStructure, crate::DeviceError> {
        let inner = unsafe { self.inner.create_acceleration_structure(desc)? };
        Ok(super::AccelerationStructure::wrap(inner, self.shared.clone()))
    }

    unsafe fn get_acceleration_structure_build_sizes(
        &self,
        desc: &crate::GetAccelerationStructureBuildSizesDescriptor<super::Buffer>,
    ) -> crate::AccelerationStructureBuildSizes {
        let entries = super::convert_entries(desc.entries);
        let desc = crate::GetAccelerationStructureBuildSizesDescriptor {
            entries: &entries,
            flags: desc.flags,
        };
        unsafe { self.inner.get_acceleration_structure_build_sizes(&desc) }
    }

    unsafe fn get_acceleration_structure_device_address(
        &self,
        acceleration_structure: &super::AccelerationStructure,
    ) -> wgt::BufferAddress {
        unsafe {
            self.inner
                .get_acceleration_structure_device_address(acceleration_structure.as_dyn())
        }
    }

    unsafe fn destroy_acceleration_structure(
        &self,
        acceleration_structure: super::AccelerationStructure,
    ) {
        unsafe {
            self.inner
                .destroy_acceleration_structure(acceleration_structure.inner)
        }
    }

    fn tlas_instance_to_bytes(&self, instance: crate::TlasInstance) -> Vec<u8> {
        self.inner.tlas_instance_to_bytes(instance)
    }

    fn get_internal_counters(&self) -> wgt::HalCounters {
        self.inner.get_internal_counters()
    }

    fn generate_allocator_report(&self) -> Option<wgt::AllocatorReport> {
        self.inner.generate_allocator_report()
    }

    fn check_if_oom(&self) -> Result<(), crate::DeviceError> {
        self.inner.check_if_oom()
    }
}
