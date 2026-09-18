/*! Generating SPIR-V functions from backend IR. */

use super::{builder, Builder};

use crate::arena::Handle;
use crate::back;

use spirv::Word;

use alloc::vec::Vec;

impl<'m> super::Context<'m> {
    pub fn generate_functions(&mut self, builder: &mut Builder) {
        for (handle, function) in self.module.functions.iter() {
            let function_id = builder.next_id();
            let return_type_id = self.type_id(function.result.ty);
            let function_type_id = todo!();

            todo();
        }
    }
}
