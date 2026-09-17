/*! [`Builder`] methods for generating SPIR-V function instructions. */

use crate::back;
use crate::back::spv2;
use spv2::instruction::Instruction;

use spirv::{Op, Word};

impl super::Builder {
    pub(super) fn function(
        &mut self,
        return_type_id: Word,
        id: Word,
        function_control: spirv::FunctionControl,
        function_type_id: Word,
    ) {
        let mut instruction = Instruction::new(Op::Function);
        instruction.set_type(return_type_id);
        instruction.set_result(id);
        instruction.add_operand(function_control.bits());
        instruction.add_operand(function_type_id);
        instruction.to_words(&mut self.functions);
    }

    pub(super) fn function_parameter(&mut self, result_type_id: Word, id: Word) {
        let mut instruction = Instruction::new(Op::FunctionParameter);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.to_words(&mut self.functions);
    }

    pub(super) fn function_end(&mut self) {
        Instruction::new(Op::FunctionEnd).to_words(&mut self.functions);
    }

    pub(super) fn function_call(
        &mut self,
        result_type_id: Word,
        id: Word,
        function_id: Word,
        argument_ids: &[Word],
    ) {
        let mut instruction = Instruction::new(Op::FunctionCall);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(function_id);

        for argument_id in argument_ids {
            instruction.add_operand(*argument_id);
        }

        instruction.to_words(&mut self.functions);
    }
}
