/*! [`Builder`] methods for generating SPIR-V constant instructions. */

use crate::back;
use crate::back::spv2;
use spv2::instruction::Instruction;

use spirv::{Op, Word};

impl super::Builder {
    pub fn constant_null(&mut self, result_type_id: Word, id: Word) {
        let mut instruction = Instruction::new(Op::ConstantNull);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn constant_true(&mut self, result_type_id: Word, id: Word) {
        let mut instruction = Instruction::new(Op::ConstantTrue);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn constant_false(&mut self, result_type_id: Word, id: Word) {
        let mut instruction = Instruction::new(Op::ConstantFalse);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn constant_16bit(&mut self, result_type_id: Word, id: Word, low: Word) {
        self.constant(result_type_id, id, &[low])
    }

    pub fn constant_32bit(&mut self, result_type_id: Word, id: Word, value: Word) {
        self.constant(result_type_id, id, &[value])
    }

    pub fn constant_64bit(&mut self, result_type_id: Word, id: Word, low: Word, high: Word) {
        self.constant(result_type_id, id, &[low, high])
    }

    pub fn constant(&mut self, result_type_id: Word, id: Word, values: &[Word]) {
        let mut instruction = Instruction::new(Op::Constant);
        instruction.set_type(result_type_id);
        instruction.set_result(id);

        for value in values {
            instruction.add_operand(*value);
        }

        instruction.to_words(&mut self.declarations);
    }

    pub fn constant_composite(&mut self, 
        result_type_id: Word,
        id: Word,
        constituent_ids: &[Word],
    ) {
        let mut instruction = Instruction::new(Op::ConstantComposite);
        instruction.set_type(result_type_id);
        instruction.set_result(id);

        for constituent_id in constituent_ids {
            instruction.add_operand(*constituent_id);
        }

        instruction.to_words(&mut self.declarations);
    }
}
