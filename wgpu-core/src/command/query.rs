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
    _phantom: PhantomData<A>,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SimplifiedQueryType {
}
impl From<wgt::QueryType> for SimplifiedQueryType {
    fn from(q: wgt::QueryType) -> Self { todo!() }
}

/// Error encountered when dealing with queries
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum QueryError {
}

impl crate::error::PrettyError for QueryError {
    fn fmt_pretty(&self, fmt: &mut crate::error::ErrorFormatter) { todo!() }
}

/// Error encountered while trying to use queries
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum QueryUseError {
}

/// Error encountered while trying to resolve a query.
#[derive(Clone, Debug, Error)]
#[non_exhaustive]
pub enum ResolveError {
}

