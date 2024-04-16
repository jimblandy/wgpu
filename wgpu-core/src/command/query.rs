use hal::CommandEncoder as _;

#[cfg(feature = "trace")]
use crate::device::trace::Command as TraceCommand;
use crate::{
    command::{CommandBuffer, CommandEncoderError},
    device::{DeviceError, MissingFeatures},
    global::Global,
    hal_api::HalApi,
    id::{self, Id},
    init_tracker::MemoryInitKind,
    resource::{QuerySet, Resource},
    storage::Storage,
    Epoch, FastHashMap, Index,
};
use std::{iter, marker::PhantomData};
use thiserror::Error;
use wgt::BufferAddress;

#[derive(Debug)]
pub(crate) struct QueryResetMap<A: HalApi> {
    map: FastHashMap<Index, (Vec<bool>, Epoch)>,
    _phantom: PhantomData<A>,
}
impl<A: HalApi> QueryResetMap<A> {
    pub fn new() -> Self { todo!() }

    pub fn use_query_set(
        &mut self,
        id: id::QuerySetId,
        query_set: &QuerySet<A>,
        query: u32,
    ) -> bool { todo!() }

    pub fn reset_queries(
        &mut self,
        raw_encoder: &mut A::CommandEncoder,
        query_set_storage: &Storage<QuerySet<A>>,
        backend: wgt::Backend,
    ) -> Result<(), id::QuerySetId> { todo!() }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SimplifiedQueryType {
    Occlusion,
    Timestamp,
    PipelineStatistics,
}
impl From<wgt::QueryType> for SimplifiedQueryType {
    fn from(q: wgt::QueryType) -> Self { todo!() }
}

/// Error encountered when dealing with queries
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum QueryError {
    #[error(transparent)]
    Device(#[from] DeviceError),
    #[error(transparent)]
    Encoder(#[from] CommandEncoderError),
    #[error(transparent)]
    MissingFeature(#[from] MissingFeatures),
    #[error("Error encountered while trying to use queries")]
    Use(#[from] QueryUseError),
    #[error("Error encountered while trying to resolve a query")]
    Resolve(#[from] ResolveError),
    #[error("Buffer {0:?} is invalid or destroyed")]
    InvalidBuffer(id::BufferId),
    #[error("QuerySet {0:?} is invalid or destroyed")]
    InvalidQuerySet(id::QuerySetId),
}

impl crate::error::PrettyError for QueryError {
    fn fmt_pretty(&self, fmt: &mut crate::error::ErrorFormatter) { todo!() }
}

/// Error encountered while trying to use queries
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum QueryUseError {
    #[error("Query {query_index} is out of bounds for a query set of size {query_set_size}")]
    OutOfBounds {
        query_index: u32,
        query_set_size: u32,
    },
    #[error("Query {query_index} has already been used within the same renderpass. Queries must only be used once per renderpass")]
    UsedTwiceInsideRenderpass { query_index: u32 },
    #[error("Query {new_query_index} was started while query {active_query_index} was already active. No more than one statistic or occlusion query may be active at once")]
    AlreadyStarted {
        active_query_index: u32,
        new_query_index: u32,
    },
    #[error("Query was stopped while there was no active query")]
    AlreadyStopped,
    #[error("A query of type {query_type:?} was started using a query set of type {set_type:?}")]
    IncompatibleType {
        set_type: SimplifiedQueryType,
        query_type: SimplifiedQueryType,
    },
}

/// Error encountered while trying to resolve a query.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ResolveError {
    #[error("Queries can only be resolved to buffers that contain the QUERY_RESOLVE usage")]
    MissingBufferUsage,
    #[error("Resolve buffer offset has to be aligned to `QUERY_RESOLVE_BUFFER_ALIGNMENT")]
    BufferOffsetAlignment,
    #[error("Resolving queries {start_query}..{end_query} would overrun the query set of size {query_set_size}")]
    QueryOverrun {
        start_query: u32,
        end_query: u32,
        query_set_size: u32,
    },
    #[error("Resolving queries {start_query}..{end_query} ({stride} byte queries) will end up overrunning the bounds of the destination buffer of size {buffer_size} using offsets {buffer_start_offset}..{buffer_end_offset}")]
    BufferOverrun {
        start_query: u32,
        end_query: u32,
        stride: u32,
        buffer_size: BufferAddress,
        buffer_start_offset: BufferAddress,
        buffer_end_offset: BufferAddress,
    },
}

impl<A: HalApi> QuerySet<A> {
    fn validate_query(
        &self,
        query_set_id: id::QuerySetId,
        query_type: SimplifiedQueryType,
        query_index: u32,
        reset_state: Option<&mut QueryResetMap<A>>,
    ) -> Result<&A::QuerySet, QueryUseError> { todo!() }

    pub(super) fn validate_and_write_timestamp(
        &self,
        raw_encoder: &mut A::CommandEncoder,
        query_set_id: id::QuerySetId,
        query_index: u32,
        reset_state: Option<&mut QueryResetMap<A>>,
    ) -> Result<(), QueryUseError> { todo!() }

    pub(super) fn validate_and_begin_occlusion_query(
        &self,
        raw_encoder: &mut A::CommandEncoder,
        query_set_id: id::QuerySetId,
        query_index: u32,
        reset_state: Option<&mut QueryResetMap<A>>,
        active_query: &mut Option<(id::QuerySetId, u32)>,
    ) -> Result<(), QueryUseError> { todo!() }

    pub(super) fn validate_and_begin_pipeline_statistics_query(
        &self,
        raw_encoder: &mut A::CommandEncoder,
        query_set_id: id::QuerySetId,
        query_index: u32,
        reset_state: Option<&mut QueryResetMap<A>>,
        active_query: &mut Option<(id::QuerySetId, u32)>,
    ) -> Result<(), QueryUseError> { todo!() }
}

pub(super) fn end_occlusion_query<A: HalApi>(
    raw_encoder: &mut A::CommandEncoder,
    storage: &Storage<QuerySet<A>>,
    active_query: &mut Option<(id::QuerySetId, u32)>,
) -> Result<(), QueryUseError> { todo!() }

pub(super) fn end_pipeline_statistics_query<A: HalApi>(
    raw_encoder: &mut A::CommandEncoder,
    storage: &Storage<QuerySet<A>>,
    active_query: &mut Option<(id::QuerySetId, u32)>,
) -> Result<(), QueryUseError> { todo!() }

impl Global {
    pub fn command_encoder_write_timestamp<A: HalApi>(
        &self,
        command_encoder_id: id::CommandEncoderId,
        query_set_id: id::QuerySetId,
        query_index: u32,
    ) -> Result<(), QueryError> { todo!() }

    pub fn command_encoder_resolve_query_set<A: HalApi>(
        &self,
        command_encoder_id: id::CommandEncoderId,
        query_set_id: id::QuerySetId,
        start_query: u32,
        query_count: u32,
        destination: id::BufferId,
        destination_offset: BufferAddress,
    ) -> Result<(), QueryError> { todo!() }
}
