use core::fmt;
use std::error::Error;

use crate::{gfx_select, global::Global};

pub struct ErrorFormatter<'a> {
    writer: &'a mut dyn fmt::Write,
    global: &'a Global,
}

impl<'a> ErrorFormatter<'a> {
    pub fn error(&mut self, err: &dyn Error) { todo!() }

    pub fn note(&mut self, note: &dyn fmt::Display) { todo!() }

    pub fn label(&mut self, label_key: &str, label_value: &String) { todo!() }

    pub fn bind_group_label(&mut self, id: &crate::id::BindGroupId) { todo!() }

    pub fn bind_group_layout_label(&mut self, id: &crate::id::BindGroupLayoutId) { todo!() }

    pub fn render_pipeline_label(&mut self, id: &crate::id::RenderPipelineId) { todo!() }

    pub fn compute_pipeline_label(&mut self, id: &crate::id::ComputePipelineId) { todo!() }

    pub fn buffer_label_with_key(&mut self, id: &crate::id::BufferId, key: &str) { todo!() }

    pub fn buffer_label(&mut self, id: &crate::id::BufferId) { todo!() }

    pub fn texture_label_with_key(&mut self, id: &crate::id::TextureId, key: &str) { todo!() }

    pub fn texture_label(&mut self, id: &crate::id::TextureId) { todo!() }

    pub fn texture_view_label_with_key(&mut self, id: &crate::id::TextureViewId, key: &str) { todo!() }

    pub fn texture_view_label(&mut self, id: &crate::id::TextureViewId) { todo!() }

    pub fn sampler_label(&mut self, id: &crate::id::SamplerId) { todo!() }

    pub fn command_buffer_label(&mut self, id: &crate::id::CommandBufferId) { todo!() }

    pub fn query_set_label(&mut self, id: &crate::id::QuerySetId) { todo!() }
}

pub trait PrettyError: Error + Sized {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

pub fn format_pretty_any(
    writer: &mut dyn fmt::Write,
    global: &Global,
    error: &(dyn Error + 'static),
) { todo!() }

#[derive(Debug)]
pub struct ContextError {
    pub string: &'static str,
    #[cfg(send_sync)]
    pub cause: Box<dyn Error + Send + Sync + 'static>,
    #[cfg(not(send_sync))]
    pub cause: Box<dyn Error + 'static>,
    pub label_key: &'static str,
    pub label: String,
}

impl PrettyError for ContextError {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

impl fmt::Display for ContextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() }
}

impl Error for ContextError {
    fn source(&self) -> Option<&(dyn Error + 'static)> { todo!() }
}
