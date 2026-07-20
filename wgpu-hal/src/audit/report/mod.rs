//! Reporting violations of `wgpu_hal` safety requirements.

use crate::audit::location::Location;
use crate::audit::{Described, Device, Id};
use crate::DynResource;

use alloc::vec::Vec;
use core::fmt;

/// A specific violation of a `wgpu_hal` safety requirement.
#[derive(Debug)]
pub enum Violation {
    /// A resource was passed to a `Device` method other than the one
    /// that created it.
    WrongDevice {
        /// The `wgpu_hal` trait and method name being called, e.g.
        /// `"Device::destroy_buffer"`.
        method: &'static str,
        /// The resource that was passed in.
        resource: Described<dyn DynResource>,
        /// The device that actually created `resource`.
        actual_device: Id<Device>,
        /// The device the method was called on.
        expected_device: Id<Device>,
        location: Location,
    },

    /// A `Device` was dropped while resources it created still exist.
    ///
    /// `wgpu_hal` requires that all of a `Device`'s resources be
    /// destroyed before the `Device` itself is dropped, even though
    /// destroying those resources doesn't require the `Device` to still
    /// be around.
    DeviceDroppedWithLiveResources {
        device: Id<dyn DynResource>,
        /// The resources that were still alive when `device` was
        /// dropped.
        resources: Vec<Described<dyn DynResource>>,
        location: Location,
    },
}

impl fmt::Display for Violation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Violation::WrongDevice {
                method,
                resource,
                actual_device,
                expected_device,
                location,
            } => {
                writeln!(
                    f,
                    "{method}: resource {resource} belongs to {actual_device}, \
                     not the device {expected_device} it was passed to"
                )?;
                write!(f, "Stack:\n{location}")
            }
            Violation::DeviceDroppedWithLiveResources {
                device,
                resources,
                location,
            } => {
                writeln!(
                    f,
                    "{device} dropped while {} resource(s) it created still exist:",
                    resources.len()
                )?;
                for resource in resources {
                    writeln!(f, "  {resource}")?;
                }
                write!(f, "Stack:\n{location}")
            }
        }
    }
}
