use std::sync::Arc;

use crate::{
    binding_model::{BindGroup, LateMinBufferBindingSizeMismatch, PipelineLayout},
    device::SHADER_STAGE_COUNT,
    hal_api::HalApi,
    id::BindGroupId,
    pipeline::LateSizedBufferGroup,
    resource::Resource,
};

use arrayvec::ArrayVec;

type BindGroupMask = u8;

mod compat {
    use arrayvec::ArrayVec;

    use crate::{binding_model::BindGroupLayout, device::bgl, hal_api::HalApi, resource::Resource};
    use std::{ops::Range, sync::Arc};

    #[derive(Debug, Clone)]
    struct Entry<A: HalApi> {
        assigned: Option<Arc<BindGroupLayout<A>>>,
        expected: Option<Arc<BindGroupLayout<A>>>,
    }

    impl<A: HalApi> Entry<A> {
        fn empty() -> Self { todo!() }
        fn is_active(&self) -> bool { todo!() }

        fn is_valid(&self) -> bool { todo!() }

        fn is_incompatible(&self) -> bool { todo!() }

        // Describe how bind group layouts are incompatible, for validation
        // error message.
        fn bgl_diff(&self) -> Vec<String> { todo!() }
    }

    #[derive(Debug, Default)]
    pub(crate) struct BoundBindGroupLayouts<A: HalApi> {
        entries: ArrayVec<Entry<A>, { hal::MAX_BIND_GROUPS }>,
    }

    impl<A: HalApi> BoundBindGroupLayouts<A> {
        pub fn new() -> Self { todo!() }
        fn make_range(&self, start_index: usize) -> Range<usize> { todo!() }

        pub fn update_expectations(
            &mut self,
            expectations: &[Arc<BindGroupLayout<A>>],
        ) -> Range<usize> { todo!() }

        pub fn assign(&mut self, index: usize, value: Arc<BindGroupLayout<A>>) -> Range<usize> { todo!() }

        pub fn list_active(&self) -> impl Iterator<Item = usize> + '_ { std::iter::empty() }

        pub fn invalid_mask(&self) -> super::BindGroupMask { todo!() }

        pub fn bgl_diff(&self) -> Vec<String> { todo!() }
    }
}

#[derive(Debug)]
struct LateBufferBinding {
    shader_expect_size: wgt::BufferAddress,
    bound_size: wgt::BufferAddress,
}

#[derive(Debug)]
pub(super) struct EntryPayload<A: HalApi> {
    pub(super) group: Option<Arc<BindGroup<A>>>,
    pub(super) dynamic_offsets: Vec<wgt::DynamicOffset>,
    late_buffer_bindings: Vec<LateBufferBinding>,
    /// Since `LateBufferBinding` may contain information about the bindings
    /// not used by the pipeline, we need to know when to stop validating.
    pub(super) late_bindings_effective_count: usize,
}

impl<A: HalApi> Default for EntryPayload<A> {
    fn default() -> Self { todo!() }
}

impl<A: HalApi> EntryPayload<A> {
    fn reset(&mut self) { todo!() }
}

#[derive(Debug, Default)]
pub(super) struct Binder<A: HalApi> {
    pub(super) pipeline_layout: Option<Arc<PipelineLayout<A>>>,
    manager: compat::BoundBindGroupLayouts<A>,
    payloads: [EntryPayload<A>; hal::MAX_BIND_GROUPS],
}

impl<A: HalApi> Binder<A> {
    pub(super) fn new() -> Self { todo!() }
    pub(super) fn reset(&mut self) { todo!() }

    pub(super) fn change_pipeline_layout<'a>(
        &'a mut self,
        new: &Arc<PipelineLayout<A>>,
        late_sized_buffer_groups: &[LateSizedBufferGroup],
    ) -> (usize, &'a [EntryPayload<A>]) { todo!() }

    pub(super) fn assign_group<'a>(
        &'a mut self,
        index: usize,
        bind_group: &Arc<BindGroup<A>>,
        offsets: &[wgt::DynamicOffset],
    ) -> &'a [EntryPayload<A>] { todo!() }

    pub(super) fn list_active(&self) -> impl Iterator<Item = BindGroupId> + '_ {
        std::iter::empty()
    }

    pub(super) fn invalid_mask(&self) -> BindGroupMask { todo!() }

    pub(super) fn bgl_diff(&self) -> Vec<String> { todo!() }

    /// Scan active buffer bindings corresponding to layouts without `min_binding_size` specified.
    pub(super) fn check_late_buffer_bindings(
        &self,
    ) -> Result<(), LateMinBufferBindingSizeMismatch> { todo!() }
}

struct PushConstantChange {
    stages: wgt::ShaderStages,
    offset: u32,
    enable: bool,
}

/// Break up possibly overlapping push constant ranges into a set of
/// non-overlapping ranges which contain all the stage flags of the
/// original ranges. This allows us to zero out (or write any value)
/// to every possible value.
pub fn compute_nonoverlapping_ranges(
    ranges: &[wgt::PushConstantRange],
) -> ArrayVec<wgt::PushConstantRange, { SHADER_STAGE_COUNT * 2 }> { todo!() }
