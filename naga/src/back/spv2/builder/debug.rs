/*! [`Builder`] methods for generating SPIR-V debugging instructions. */

use super::utils;

use crate::back;
use crate::back::spv2;
use spv2::instruction::Instruction;

use spirv::{Op, Word};

impl super::Builder {
    pub fn string(&mut self, name: &str, id: Word) {
        let mut instruction = Instruction::new(Op::String);
        instruction.set_result(id);
        instruction.add_operands(utils::string_to_words(name));
        instruction.to_words(&mut self.debug_text);
    }

    pub fn name(&mut self, target_id: Word, name: &str) {
        let mut instruction = Instruction::new(Op::Name);
        instruction.add_operand(target_id);
        instruction.add_operands(utils::string_to_words(name));
        instruction.to_words(&mut self.debug_names);
    }

    pub fn member_name(&mut self, target_id: Word, member: Word, name: &str) {
        let mut instruction = Instruction::new(Op::MemberName);
        instruction.add_operand(target_id);
        instruction.add_operand(member);
        instruction.add_operands(utils::string_to_words(name));
        instruction.to_words(&mut self.debug_names);
    }

    pub fn line(&mut self, file: Word, line: Word, column: Word) {
        let mut instruction = Instruction::new(Op::Line);
        instruction.add_operand(file);
        instruction.add_operand(line);
        instruction.add_operand(column);
        instruction.to_words(&mut self.functions);
    }
}
