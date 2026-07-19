//! Module defining the [`Op`] type, representing a `wgpu_hal` operation.

pub use crate::audit::{Adapter, Id, Instance, Surface};

use alloc::boxed::Box;
use alloc::vec::Vec;

#[derive(Debug)]
pub enum Op {
    /// An operation on the [`Instance`] with the given id.
    Instance { id: Id<Instance>, op: InstanceOp },
}

/// Operations on [`Instance`]s.
///
/// [`Instance`]: crate::Instance
#[derive(Debug)]
pub enum InstanceOp {
    /// Create a surface.
    ///
    /// On success, this will be followed by a [`CreateSurface`] result.
    ///
    /// [`CreateSurfaceSurface`]: Finished::CreateSurface
    CreateSurface {
        display_handle: raw_window_handle::RawDisplayHandle,
        window_handle: raw_window_handle::RawWindowHandle,
    },

    /// Enumerate adapters.
    ///
    /// On success, this will be followed by a [`EnumerateAdapters`] result.
    ///
    /// [`EnumerateAdapters`]: Finished::EnumerateAdapters
    EnumerateAdapters {
        surface_hint: Option<Id<Surface>>,
    },
}

#[derive(Debug)]
pub struct OpExposedAdapter {
    pub adapter: Id<Adapter>,
    pub info: wgt::AdapterInfo,
    pub features: wgt::Features,
    pub capabilities: crate::Capabilities,
}

/// The result from a successful `wgpu_hal` operation.
#[derive(Debug)]
pub enum Finished {
    /// The previous operation succeeded, but it returns `()`.
    Unit,

    /// A new `Instance` has been created.
    ///
    /// This is always the first callback on a new [`Auditor`]. All
    /// other activity reported to that `Auditor` will involve
    /// descendants of this instance.
    NewInstance { id: Id<Instance>, backend: wgpu_types::Backend },

    /// A new `Surface` has been created.
    CreateSurface(Id<Surface>),

    /// Result from `enumerate_adapters`.
    EnumerateAdapters(Vec<OpExposedAdapter>),
}

/// Some sort of underlying error.
pub type Error = Box<dyn core::error::Error + Send + Sync + 'static>;
