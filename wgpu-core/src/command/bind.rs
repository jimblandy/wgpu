use std::sync::Arc;

use crate::{
    binding_model::{BindGroup, LateMinBufferBindingSizeMismatch, PipelineLayout},
    hal_api::HalApi,
    pipeline::LateSizedBufferGroup,
    resource::Resource,
};

use arrayvec::ArrayVec;

mod compat {
    use arrayvec::ArrayVec;

    use crate::{binding_model::BindGroupLayout, device::bgl, hal_api::HalApi, resource::Resource};
    use std::{ops::Range, sync::Arc};

    #[derive(Debug, Clone)]
    struct Entry<A: HalApi> {
        marker: std::marker::PhantomData<A>,
    }

    #[derive(Debug, Default)]
    pub(crate) struct BoundBindGroupLayouts<A: HalApi> {
        marker: std::marker::PhantomData<A>,
    }
}

#[derive(Debug)]
struct LateBufferBinding {
}

#[derive(Debug)]
pub(super) struct EntryPayload<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}

impl<A: HalApi> Default for EntryPayload<A> {
    fn default() -> Self { todo!() }
}

#[derive(Debug, Default)]
pub(super) struct Binder<A: HalApi> {
    marker: std::marker::PhantomData<A>,
}
