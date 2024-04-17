use core::fmt;
use std::error::Error;

use crate::{gfx_select, global::Global};

pub struct ErrorFormatter {
}

pub trait PrettyError: Error + Sized {
    fn fmt_pretty(&self, fmt: &mut ErrorFormatter) { todo!() }
}

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
