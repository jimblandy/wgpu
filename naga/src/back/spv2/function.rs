/*! Generating SPIR-V functions from backend IR. */

use super::{builder, Builder};

use crate::arena::Handle;
use crate::back;

use spirv::Word;

use alloc::vec::Vec;

impl<'m> super::Context<'m> {
    pub fn generate_functions(&mut self, builder: &mut Builder) {
        for (handle, function) in self.module.functions.iter() {
            todo!()
        }
    }
}
