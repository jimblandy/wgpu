/*! [`Builder`] methods for generating SPIR-V type instructions. */

use crate::back;
use crate::back::spv2;
use spv2::instruction::Instruction;

use spirv::{Op, Word};

impl super::Builder {
    pub fn type_bool(&mut self, id: Word) {
        let mut instruction = Instruction::new(Op::TypeBool);
        instruction.set_result(id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_int(&mut self, id: Word, width: Word, signedness: spv2::instruction::Signedness) {
        let mut instruction = Instruction::new(Op::TypeInt);
        instruction.set_result(id);
        instruction.add_operand(width);
        instruction.add_operand(signedness as u32);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_float(&mut self, id: Word, width: Word) {
        let mut instruction = Instruction::new(Op::TypeFloat);
        instruction.set_result(id);
        instruction.add_operand(width);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_vector(
        &mut self,
        id: Word,
        component_type_id: Word,
        component_count: crate::VectorSize,
    ) {
        let mut instruction = Instruction::new(Op::TypeVector);
        instruction.set_result(id);
        instruction.add_operand(component_type_id);
        instruction.add_operand(component_count as u32);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_matrix(&mut self, id: Word, column_type_id: Word, column_count: crate::VectorSize) {
        let mut instruction = Instruction::new(Op::TypeMatrix);
        instruction.set_result(id);
        instruction.add_operand(column_type_id);
        instruction.add_operand(column_count as u32);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_coop_matrix(&mut self, 
        id: Word,
        scalar_type_id: Word,
        scope_id: Word,
        row_count_id: Word,
        column_count_id: Word,
        matrix_use_id: Word,
    ) {
        let mut instruction = Instruction::new(Op::TypeCooperativeMatrixKHR);
        instruction.set_result(id);
        instruction.add_operand(scalar_type_id);
        instruction.add_operand(scope_id);
        instruction.add_operand(row_count_id);
        instruction.add_operand(column_count_id);
        instruction.add_operand(matrix_use_id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_image(&mut self, 
        id: Word,
        sampled_type_id: Word,
        dim: spirv::Dim,
        flags: back::ir::ImageTypeFlags,
        image_format: spirv::ImageFormat,
    ) {
        use back::ir::ImageTypeFlags as Itf;
        let mut instruction = Instruction::new(Op::TypeImage);
        instruction.set_result(id);
        instruction.add_operand(sampled_type_id);
        instruction.add_operand(dim as u32);
        instruction.add_operand(flags.contains(Itf::DEPTH) as u32);
        instruction.add_operand(flags.contains(Itf::ARRAYED) as u32);
        instruction.add_operand(flags.contains(Itf::MULTISAMPLED) as u32);
        instruction.add_operand(if flags.contains(Itf::SAMPLED) {
            1
        } else {
            2
        });
        instruction.add_operand(image_format as u32);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_sampler(&mut self, id: Word) {
        let mut instruction = Instruction::new(Op::TypeSampler);
        instruction.set_result(id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_acceleration_structure(&mut self, id: Word) {
        let mut instruction = Instruction::new(Op::TypeAccelerationStructureKHR);
        instruction.set_result(id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_ray_query(&mut self, id: Word) {
        let mut instruction = Instruction::new(Op::TypeRayQueryKHR);
        instruction.set_result(id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_sampled_image(&mut self, id: Word, image_type_id: Word) {
        let mut instruction = Instruction::new(Op::TypeSampledImage);
        instruction.set_result(id);
        instruction.add_operand(image_type_id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_array(&mut self, id: Word, element_type_id: Word, length_id: Word) {
        let mut instruction = Instruction::new(Op::TypeArray);
        instruction.set_result(id);
        instruction.add_operand(element_type_id);
        instruction.add_operand(length_id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_runtime_array(&mut self, id: Word, element_type_id: Word) {
        let mut instruction = Instruction::new(Op::TypeRuntimeArray);
        instruction.set_result(id);
        instruction.add_operand(element_type_id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_struct(&mut self, id: Word, member_ids: impl Iterator<Item = Word>) {
        let mut instruction = Instruction::new(Op::TypeStruct);
        instruction.set_result(id);

        instruction.add_operands(member_ids);

        instruction.to_words(&mut self.declarations);
    }

    pub fn type_pointer(&mut self, id: Word, storage_class: spirv::StorageClass, type_id: Word) {
        let mut instruction = Instruction::new(Op::TypePointer);
        instruction.set_result(id);
        instruction.add_operand(storage_class as u32);
        instruction.add_operand(type_id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_function(&mut self, id: Word, return_type_id: Word, parameter_ids: &[Word]) {
        let mut instruction = Instruction::new(Op::TypeFunction);
        instruction.set_result(id);
        instruction.add_operand(return_type_id);

        for parameter_id in parameter_ids {
            instruction.add_operand(*parameter_id);
        }

        instruction.to_words(&mut self.declarations);
    }
}
