#[cfg(feature = "trace")]
use crate::device::trace;
use crate::{
    api_log, binding_model, command, conv,
    device::{
        bgl, life::WaitIdleError, queue, DeviceError, DeviceLostClosure,
        DeviceLostReason, HostMap,
    },
    global::Global,
    hal_api::HalApi,
    id::{self, AdapterId, DeviceId, QueueId, SurfaceId},
    init_tracker::TextureInitTracker,
    instance::{self, Adapter, Surface},
    pipeline, present,
    resource::{self, BufferAccessResult},
    resource::{BufferAccessError, BufferMapOperation, CreateBufferError, Resource},
    validation::check_buffer_usage,
    Label, LabelHelpers as _,
};

use arrayvec::ArrayVec;
use hal::Device as _;
use parking_lot::RwLock;

use wgt::{BufferAddress, TextureFormat};

use std::{
    borrow::Cow,
    iter, ptr,
    sync::{atomic::Ordering, Arc},
};

use super::{InvalidDevice, UserClosures};

