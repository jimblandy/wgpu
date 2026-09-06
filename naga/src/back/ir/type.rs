/*! Lowering types from Naga IR to backend IR. */

use crate::arena::Handle;
use crate::span::Span;
use crate::{back, ir};

use alloc::vec;
use alloc::vec::Vec;

impl<'m> back::ir::ModuleContext<'m> {
    pub fn lower_type(&mut self, ty: Handle<ir::Type>, out: &mut back::ir::Module) -> Handle<back::ir::Type> {
        todo!()
    }
}
