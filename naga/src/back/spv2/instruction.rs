/*! Helpers for building SPIR-V instructions. */

use spirv::{Op, Word};
use alloc::vec::Vec;
use alloc::vec; // the macro

#[derive(Clone)]
pub struct Instruction {
    op: Op,
    wc: u32,
    type_id: Option<Word>,
    result_id: Option<Word>,
    operands: Vec<Word>,
}

pub enum Signedness {
    Unsigned = 0,
    Signed = 1,
}

pub enum SampleLod {
    Explicit,
    Implicit,
}

pub struct Case {
    pub value: Word,
    pub label_id: Word,
}

impl Instruction {
    pub const fn new(op: Op) -> Self {
        Instruction {
            op,
            wc: 1, // Always start at 1 for the first word (OP + WC),
            type_id: None,
            result_id: None,
            operands: vec![],
        }
    }

    pub fn set_type(&mut self, id: Word) {
        assert!(self.type_id.is_none(), "Type can only be set once");
        self.type_id = Some(id);
        self.wc += 1;
    }

    pub fn set_result(&mut self, id: Word) {
        assert!(self.result_id.is_none(), "Result can only be set once");
        self.result_id = Some(id);
        self.wc += 1;
    }

    pub fn add_operand(&mut self, operand: Word) {
        self.operands.push(operand);
        self.wc += 1;
    }

    pub fn add_operands(&mut self, operands: impl IntoIterator<Item = Word>) {
        let original_len = self.operands.len();
        self.operands.extend(operands);
        self.wc += (self.operands.len() - original_len) as u32;
    }

    pub fn to_words(&self, sink: &mut impl Extend<Word>) {
        sink.extend(Some((self.wc << 16) | self.op as u32));
        sink.extend(self.type_id);
        sink.extend(self.result_id);
        sink.extend(self.operands.iter().cloned());
    }

    //
    //  Debug Instructions
    //

    pub fn string(name: &str, id: Word) -> Self {
        let mut instruction = Self::new(Op::String);
        instruction.set_result(id);
        instruction.add_operands(string_to_words(name));
        instruction
    }

    pub fn name(target_id: Word, name: &str) -> Self {
        let mut instruction = Self::new(Op::Name);
        instruction.add_operand(target_id);
        instruction.add_operands(string_to_words(name));
        instruction
    }

    pub fn member_name(target_id: Word, member: Word, name: &str) -> Self {
        let mut instruction = Self::new(Op::MemberName);
        instruction.add_operand(target_id);
        instruction.add_operand(member);
        instruction.add_operands(string_to_words(name));
        instruction
    }

    pub fn line(file: Word, line: Word, column: Word) -> Self {
        let mut instruction = Self::new(Op::Line);
        instruction.add_operand(file);
        instruction.add_operand(line);
        instruction.add_operand(column);
        instruction
    }

    //
    //  Annotation Instructions
    //

    pub fn decorate(
        target_id: Word,
        decoration: spirv::Decoration,
        operands: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::Decorate);
        instruction.add_operand(target_id);
        instruction.add_operand(decoration as u32);
        for operand in operands {
            instruction.add_operand(*operand)
        }
        instruction
    }

    pub fn member_decorate(
        target_id: Word,
        member_index: Word,
        decoration: spirv::Decoration,
        operands: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::MemberDecorate);
        instruction.add_operand(target_id);
        instruction.add_operand(member_index);
        instruction.add_operand(decoration as u32);
        for operand in operands {
            instruction.add_operand(*operand)
        }
        instruction
    }

    //
    //  Extension Instructions
    //

    pub fn extension(name: &str) -> Self {
        let mut instruction = Self::new(Op::Extension);
        instruction.add_operands(string_to_words(name));
        instruction
    }

    pub fn ext_inst_import(id: Word, name: &str) -> Self {
        let mut instruction = Self::new(Op::ExtInstImport);
        instruction.set_result(id);
        instruction.add_operands(string_to_words(name));
        instruction
    }

    pub fn ext_inst_gl_op(
        set_id: Word,
        op: spirv::GlslStd450Op,
        result_type_id: Word,
        id: Word,
        operands: &[Word],
    ) -> Self {
        Self::ext_inst(set_id, op as u32, result_type_id, id, operands)
    }

    pub fn ext_inst(
        set_id: Word,
        op: u32,
        result_type_id: Word,
        id: Word,
        operands: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::ExtInst);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(set_id);
        instruction.add_operand(op);
        for operand in operands {
            instruction.add_operand(*operand)
        }
        instruction
    }

    //
    //  Mode-Setting Instructions
    //

    pub fn memory_model(
        addressing_model: spirv::AddressingModel,
        memory_model: spirv::MemoryModel,
    ) -> Self {
        let mut instruction = Self::new(Op::MemoryModel);
        instruction.add_operand(addressing_model as u32);
        instruction.add_operand(memory_model as u32);
        instruction
    }

    pub fn entry_point(
        execution_model: spirv::ExecutionModel,
        entry_point_id: Word,
        name: &str,
        interface_ids: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::EntryPoint);
        instruction.add_operand(execution_model as u32);
        instruction.add_operand(entry_point_id);
        instruction.add_operands(string_to_words(name));

        for interface_id in interface_ids {
            instruction.add_operand(*interface_id);
        }

        instruction
    }

    pub fn execution_mode(
        entry_point_id: Word,
        execution_mode: spirv::ExecutionMode,
        args: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::ExecutionMode);
        instruction.add_operand(entry_point_id);
        instruction.add_operand(execution_mode as u32);
        for arg in args {
            instruction.add_operand(*arg);
        }
        instruction
    }

    pub fn capability(capability: spirv::Capability) -> Self {
        let mut instruction = Self::new(Op::Capability);
        instruction.add_operand(capability as u32);
        instruction
    }

    //
    //  Type-Declaration Instructions
    //

    pub fn type_void(id: Word) -> Self {
        let mut instruction = Self::new(Op::TypeVoid);
        instruction.set_result(id);
        instruction
    }

    pub fn type_bool(id: Word) -> Self {
        let mut instruction = Self::new(Op::TypeBool);
        instruction.set_result(id);
        instruction
    }

    //
    //  Constant-Creation Instructions
    //

    pub fn constant_null(result_type_id: Word, id: Word) -> Self {
        let mut instruction = Self::new(Op::ConstantNull);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction
    }

    pub fn constant_true(result_type_id: Word, id: Word) -> Self {
        let mut instruction = Self::new(Op::ConstantTrue);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction
    }

    pub fn constant_false(result_type_id: Word, id: Word) -> Self {
        let mut instruction = Self::new(Op::ConstantFalse);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction
    }

    pub fn constant_16bit(result_type_id: Word, id: Word, low: Word) -> Self {
        Self::constant(result_type_id, id, &[low])
    }

    pub fn constant_32bit(result_type_id: Word, id: Word, value: Word) -> Self {
        Self::constant(result_type_id, id, &[value])
    }

    pub fn constant_64bit(result_type_id: Word, id: Word, low: Word, high: Word) -> Self {
        Self::constant(result_type_id, id, &[low, high])
    }

    pub fn constant(result_type_id: Word, id: Word, values: &[Word]) -> Self {
        let mut instruction = Self::new(Op::Constant);
        instruction.set_type(result_type_id);
        instruction.set_result(id);

        for value in values {
            instruction.add_operand(*value);
        }

        instruction
    }

    pub fn constant_composite(
        result_type_id: Word,
        id: Word,
        constituent_ids: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::ConstantComposite);
        instruction.set_type(result_type_id);
        instruction.set_result(id);

        for constituent_id in constituent_ids {
            instruction.add_operand(*constituent_id);
        }

        instruction
    }

    //
    //  Memory Instructions
    //

    pub fn variable(
        result_type_id: Word,
        id: Word,
        storage_class: spirv::StorageClass,
        initializer_id: Option<Word>,
    ) -> Self {
        let mut instruction = Self::new(Op::Variable);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(storage_class as u32);

        if let Some(initializer_id) = initializer_id {
            instruction.add_operand(initializer_id);
        }

        instruction
    }

    pub fn load(
        result_type_id: Word,
        id: Word,
        pointer_id: Word,
        memory_access: Option<spirv::MemoryAccess>,
    ) -> Self {
        let mut instruction = Self::new(Op::Load);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(pointer_id);

        if let Some(memory_access) = memory_access {
            instruction.add_operand(memory_access.bits());
        }

        instruction
    }

    pub fn atomic_load(
        result_type_id: Word,
        id: Word,
        pointer_id: Word,
        scope_id: Word,
        semantics_id: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::AtomicLoad);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(pointer_id);
        instruction.add_operand(scope_id);
        instruction.add_operand(semantics_id);
        instruction
    }

    pub fn store(
        pointer_id: Word,
        value_id: Word,
        memory_access: Option<spirv::MemoryAccess>,
    ) -> Self {
        let mut instruction = Self::new(Op::Store);
        instruction.add_operand(pointer_id);
        instruction.add_operand(value_id);

        if let Some(memory_access) = memory_access {
            instruction.add_operand(memory_access.bits());
        }

        instruction
    }

    pub fn atomic_store(
        pointer_id: Word,
        scope_id: Word,
        semantics_id: Word,
        value_id: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::AtomicStore);
        instruction.add_operand(pointer_id);
        instruction.add_operand(scope_id);
        instruction.add_operand(semantics_id);
        instruction.add_operand(value_id);
        instruction
    }

    pub fn access_chain(
        result_type_id: Word,
        id: Word,
        base_id: Word,
        index_ids: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::AccessChain);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(base_id);

        for index_id in index_ids {
            instruction.add_operand(*index_id);
        }

        instruction
    }

    pub fn array_length(
        result_type_id: Word,
        id: Word,
        structure_id: Word,
        array_member: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::ArrayLength);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(structure_id);
        instruction.add_operand(array_member);
        instruction
    }

    //
    //  Function Instructions
    //

    pub fn function(
        return_type_id: Word,
        id: Word,
        function_control: spirv::FunctionControl,
        function_type_id: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::Function);
        instruction.set_type(return_type_id);
        instruction.set_result(id);
        instruction.add_operand(function_control.bits());
        instruction.add_operand(function_type_id);
        instruction
    }

    pub fn function_parameter(result_type_id: Word, id: Word) -> Self {
        let mut instruction = Self::new(Op::FunctionParameter);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction
    }

    pub const fn function_end() -> Self {
        Self::new(Op::FunctionEnd)
    }

    pub fn function_call(
        result_type_id: Word,
        id: Word,
        function_id: Word,
        argument_ids: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::FunctionCall);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(function_id);

        for argument_id in argument_ids {
            instruction.add_operand(*argument_id);
        }

        instruction
    }

    //
    //  Image Instructions
    //

    pub fn sampled_image(
        result_type_id: Word,
        id: Word,
        image: Word,
        sampler: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::SampledImage);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(image);
        instruction.add_operand(sampler);
        instruction
    }

    pub fn image_sample(
        result_type_id: Word,
        id: Word,
        lod: SampleLod,
        sampled_image: Word,
        coordinates: Word,
        depth_ref: Option<Word>,
    ) -> Self {
        let op = match (lod, depth_ref) {
            (SampleLod::Explicit, None) => Op::ImageSampleExplicitLod,
            (SampleLod::Implicit, None) => Op::ImageSampleImplicitLod,
            (SampleLod::Explicit, Some(_)) => Op::ImageSampleDrefExplicitLod,
            (SampleLod::Implicit, Some(_)) => Op::ImageSampleDrefImplicitLod,
        };

        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(sampled_image);
        instruction.add_operand(coordinates);
        if let Some(dref) = depth_ref {
            instruction.add_operand(dref);
        }

        instruction
    }

    pub fn image_gather(
        result_type_id: Word,
        id: Word,
        sampled_image: Word,
        coordinates: Word,
        component_id: Word,
        depth_ref: Option<Word>,
    ) -> Self {
        let op = match depth_ref {
            None => Op::ImageGather,
            Some(_) => Op::ImageDrefGather,
        };

        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(sampled_image);
        instruction.add_operand(coordinates);
        if let Some(dref) = depth_ref {
            instruction.add_operand(dref);
        } else {
            instruction.add_operand(component_id);
        }

        instruction
    }

    pub fn image_fetch_or_read(
        op: Op,
        result_type_id: Word,
        id: Word,
        image: Word,
        coordinates: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(image);
        instruction.add_operand(coordinates);
        instruction
    }

    pub fn image_write(image: Word, coordinates: Word, value: Word) -> Self {
        let mut instruction = Self::new(Op::ImageWrite);
        instruction.add_operand(image);
        instruction.add_operand(coordinates);
        instruction.add_operand(value);
        instruction
    }

    pub fn image_texel_pointer(
        result_type_id: Word,
        id: Word,
        image: Word,
        coordinates: Word,
        sample: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::ImageTexelPointer);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(image);
        instruction.add_operand(coordinates);
        instruction.add_operand(sample);
        instruction
    }

    pub fn image_atomic(
        op: Op,
        result_type_id: Word,
        id: Word,
        pointer: Word,
        scope_id: Word,
        semantics_id: Word,
        value: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(pointer);
        instruction.add_operand(scope_id);
        instruction.add_operand(semantics_id);
        instruction.add_operand(value);
        instruction
    }

    pub fn image_query(op: Op, result_type_id: Word, id: Word, image: Word) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(image);
        instruction
    }

    //
    //  Ray Query Instructions
    //
    #[allow(clippy::too_many_arguments)]
    pub fn ray_query_initialize(
        query: Word,
        acceleration_structure: Word,
        ray_flags: Word,
        cull_mask: Word,
        ray_origin: Word,
        ray_tmin: Word,
        ray_dir: Word,
        ray_tmax: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::RayQueryInitializeKHR);
        instruction.add_operand(query);
        instruction.add_operand(acceleration_structure);
        instruction.add_operand(ray_flags);
        instruction.add_operand(cull_mask);
        instruction.add_operand(ray_origin);
        instruction.add_operand(ray_tmin);
        instruction.add_operand(ray_dir);
        instruction.add_operand(ray_tmax);
        instruction
    }

    pub fn ray_query_proceed(result_type_id: Word, id: Word, query: Word) -> Self {
        let mut instruction = Self::new(Op::RayQueryProceedKHR);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(query);
        instruction
    }

    pub fn ray_query_generate_intersection(query: Word, hit: Word) -> Self {
        let mut instruction = Self::new(Op::RayQueryGenerateIntersectionKHR);
        instruction.add_operand(query);
        instruction.add_operand(hit);
        instruction
    }

    pub fn ray_query_confirm_intersection(query: Word) -> Self {
        let mut instruction = Self::new(Op::RayQueryConfirmIntersectionKHR);
        instruction.add_operand(query);
        instruction
    }

    pub fn ray_query_return_vertex_position(
        result_type_id: Word,
        id: Word,
        query: Word,
        intersection: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::RayQueryGetIntersectionTriangleVertexPositionsKHR);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(query);
        instruction.add_operand(intersection);
        instruction
    }

    pub fn ray_query_get_intersection(
        op: Op,
        result_type_id: Word,
        id: Word,
        query: Word,
        intersection: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(query);
        instruction.add_operand(intersection);
        instruction
    }

    pub fn ray_query_get_t_min(result_type_id: Word, id: Word, query: Word) -> Self {
        let mut instruction = Self::new(Op::RayQueryGetRayTMinKHR);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(query);
        instruction
    }

    pub fn ray_query_terminate(query: Word) -> Self {
        let mut instruction = Self::new(Op::RayQueryTerminateKHR);
        instruction.add_operand(query);
        instruction
    }

    //
    //  Ray Tracing Pipeline Instructions
    //

    #[expect(clippy::too_many_arguments)]
    pub fn trace_ray(
        acceleration_structure: Word,
        ray_flags: Word,
        cull_mask: Word,
        sbt_offset: Word,
        sbt_stride: Word,
        miss_idx: Word,
        ray_origin: Word,
        ray_tmin: Word,
        ray_dir: Word,
        ray_tmax: Word,
        payload: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::TraceRayKHR);
        instruction.add_operand(acceleration_structure);
        instruction.add_operand(ray_flags);
        instruction.add_operand(cull_mask);
        instruction.add_operand(sbt_offset);
        instruction.add_operand(sbt_stride);
        instruction.add_operand(miss_idx);
        instruction.add_operand(ray_origin);
        instruction.add_operand(ray_tmin);
        instruction.add_operand(ray_dir);
        instruction.add_operand(ray_tmax);
        instruction.add_operand(payload);
        instruction
    }

    //
    //  Conversion Instructions
    //
    pub fn unary(op: Op, result_type_id: Word, id: Word, value: Word) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(value);
        instruction
    }

    //
    //  Composite Instructions
    //

    pub fn composite_construct(
        result_type_id: Word,
        id: Word,
        constituent_ids: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::CompositeConstruct);
        instruction.set_type(result_type_id);
        instruction.set_result(id);

        for constituent_id in constituent_ids {
            instruction.add_operand(*constituent_id);
        }

        instruction
    }

    pub fn composite_extract(
        result_type_id: Word,
        id: Word,
        composite_id: Word,
        indices: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::CompositeExtract);
        instruction.set_type(result_type_id);
        instruction.set_result(id);

        instruction.add_operand(composite_id);
        for index in indices {
            instruction.add_operand(*index);
        }

        instruction
    }

    pub fn vector_extract_dynamic(
        result_type_id: Word,
        id: Word,
        vector_id: Word,
        index_id: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::VectorExtractDynamic);
        instruction.set_type(result_type_id);
        instruction.set_result(id);

        instruction.add_operand(vector_id);
        instruction.add_operand(index_id);

        instruction
    }

    pub fn vector_shuffle(
        result_type_id: Word,
        id: Word,
        v1_id: Word,
        v2_id: Word,
        components: &[Word],
    ) -> Self {
        let mut instruction = Self::new(Op::VectorShuffle);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(v1_id);
        instruction.add_operand(v2_id);

        for &component in components {
            instruction.add_operand(component);
        }

        instruction
    }

    //
    // Arithmetic Instructions
    //
    pub fn binary(
        op: Op,
        result_type_id: Word,
        id: Word,
        operand_1: Word,
        operand_2: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(operand_1);
        instruction.add_operand(operand_2);
        instruction
    }

    pub fn ternary(
        op: Op,
        result_type_id: Word,
        id: Word,
        operand_1: Word,
        operand_2: Word,
        operand_3: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(operand_1);
        instruction.add_operand(operand_2);
        instruction.add_operand(operand_3);
        instruction
    }

    pub fn quaternary(
        op: Op,
        result_type_id: Word,
        id: Word,
        operand_1: Word,
        operand_2: Word,
        operand_3: Word,
        operand_4: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(operand_1);
        instruction.add_operand(operand_2);
        instruction.add_operand(operand_3);
        instruction.add_operand(operand_4);
        instruction
    }

    pub fn relational(op: Op, result_type_id: Word, id: Word, expr_id: Word) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(expr_id);
        instruction
    }

    pub fn atomic_binary(
        op: Op,
        result_type_id: Word,
        id: Word,
        pointer: Word,
        scope_id: Word,
        semantics_id: Word,
        value: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(pointer);
        instruction.add_operand(scope_id);
        instruction.add_operand(semantics_id);
        instruction.add_operand(value);
        instruction
    }

    //
    // Bit Instructions
    //

    //
    // Relational and Logical Instructions
    //

    //
    // Derivative Instructions
    //

    pub fn derivative(op: Op, result_type_id: Word, id: Word, expr_id: Word) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(expr_id);
        instruction
    }

    //
    // Control-Flow Instructions
    //

    pub fn phi(
        result_type_id: Word,
        result_id: Word,
        var_parent_pairs: &[(Word, Word)],
    ) -> Self {
        let mut instruction = Self::new(Op::Phi);
        instruction.add_operand(result_type_id);
        instruction.add_operand(result_id);
        for &(variable, parent) in var_parent_pairs {
            instruction.add_operand(variable);
            instruction.add_operand(parent);
        }
        instruction
    }

    pub fn selection_merge(
        merge_id: Word,
        selection_control: spirv::SelectionControl,
    ) -> Self {
        let mut instruction = Self::new(Op::SelectionMerge);
        instruction.add_operand(merge_id);
        instruction.add_operand(selection_control.bits());
        instruction
    }

    pub fn loop_merge(
        merge_id: Word,
        continuing_id: Word,
        selection_control: spirv::SelectionControl,
    ) -> Self {
        let mut instruction = Self::new(Op::LoopMerge);
        instruction.add_operand(merge_id);
        instruction.add_operand(continuing_id);
        instruction.add_operand(selection_control.bits());
        instruction
    }

    pub fn label(id: Word) -> Self {
        let mut instruction = Self::new(Op::Label);
        instruction.set_result(id);
        instruction
    }

    pub fn branch(id: Word) -> Self {
        let mut instruction = Self::new(Op::Branch);
        instruction.add_operand(id);
        instruction
    }

    // TODO Branch Weights not implemented.
    pub fn branch_conditional(
        condition_id: Word,
        true_label: Word,
        false_label: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::BranchConditional);
        instruction.add_operand(condition_id);
        instruction.add_operand(true_label);
        instruction.add_operand(false_label);
        instruction
    }

    pub fn switch(selector_id: Word, default_id: Word, cases: &[Case]) -> Self {
        let mut instruction = Self::new(Op::Switch);
        instruction.add_operand(selector_id);
        instruction.add_operand(default_id);
        for case in cases {
            instruction.add_operand(case.value);
            instruction.add_operand(case.label_id);
        }
        instruction
    }

    pub fn select(
        result_type_id: Word,
        id: Word,
        condition_id: Word,
        accept_id: Word,
        reject_id: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::Select);
        instruction.add_operand(result_type_id);
        instruction.add_operand(id);
        instruction.add_operand(condition_id);
        instruction.add_operand(accept_id);
        instruction.add_operand(reject_id);
        instruction
    }

    pub const fn kill() -> Self {
        Self::new(Op::Kill)
    }

    pub const fn return_void() -> Self {
        Self::new(Op::Return)
    }

    pub fn return_value(value_id: Word) -> Self {
        let mut instruction = Self::new(Op::ReturnValue);
        instruction.add_operand(value_id);
        instruction
    }

    //
    //  Atomic Instructions
    //

    //
    //  Primitive Instructions
    //

    // Barriers

    pub fn control_barrier(
        exec_scope_id: Word,
        mem_scope_id: Word,
        semantics_id: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::ControlBarrier);
        instruction.add_operand(exec_scope_id);
        instruction.add_operand(mem_scope_id);
        instruction.add_operand(semantics_id);
        instruction
    }
    pub fn memory_barrier(mem_scope_id: Word, semantics_id: Word) -> Self {
        let mut instruction = Self::new(Op::MemoryBarrier);
        instruction.add_operand(mem_scope_id);
        instruction.add_operand(semantics_id);
        instruction
    }

    // Group Instructions

    pub fn group_non_uniform_ballot(
        result_type_id: Word,
        id: Word,
        exec_scope_id: Word,
        predicate: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::GroupNonUniformBallot);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(exec_scope_id);
        instruction.add_operand(predicate);

        instruction
    }
    pub fn group_non_uniform_broadcast_first(
        result_type_id: Word,
        id: Word,
        exec_scope_id: Word,
        value: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::GroupNonUniformBroadcastFirst);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(exec_scope_id);
        instruction.add_operand(value);

        instruction
    }
    pub fn group_non_uniform_gather(
        op: Op,
        result_type_id: Word,
        id: Word,
        exec_scope_id: Word,
        value: Word,
        index: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(exec_scope_id);
        instruction.add_operand(value);
        instruction.add_operand(index);

        instruction
    }
    pub fn group_non_uniform_arithmetic(
        op: Op,
        result_type_id: Word,
        id: Word,
        exec_scope_id: Word,
        group_op: Option<spirv::GroupOperation>,
        value: Word,
    ) -> Self {
        let mut instruction = Self::new(op);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(exec_scope_id);
        if let Some(group_op) = group_op {
            instruction.add_operand(group_op as u32);
        }
        instruction.add_operand(value);

        instruction
    }
    pub fn group_non_uniform_quad_swap(
        result_type_id: Word,
        id: Word,
        exec_scope_id: Word,
        value: Word,
        direction: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::GroupNonUniformQuadSwap);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(exec_scope_id);
        instruction.add_operand(value);
        instruction.add_operand(direction);

        instruction
    }

    // Cooperative operations
    pub fn coop_load(
        result_type_id: Word,
        id: Word,
        pointer_id: Word,
        layout_id: Word,
        stride_id: Word,
    ) -> Self {
        let mut instruction = Self::new(Op::CooperativeMatrixLoadKHR);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(pointer_id);
        instruction.add_operand(layout_id);
        instruction.add_operand(stride_id);
        instruction
    }
    pub fn coop_store(id: Word, pointer_id: Word, layout_id: Word, stride_id: Word) -> Self {
        let mut instruction = Self::new(Op::CooperativeMatrixStoreKHR);
        instruction.add_operand(pointer_id);
        instruction.add_operand(id);
        instruction.add_operand(layout_id);
        instruction.add_operand(stride_id);
        instruction
    }
    pub fn coop_mul_add(result_type_id: Word, id: Word, a: Word, b: Word, c: Word) -> Self {
        let mut instruction = Self::new(Op::CooperativeMatrixMulAddKHR);
        instruction.set_type(result_type_id);
        instruction.set_result(id);
        instruction.add_operand(a);
        instruction.add_operand(b);
        instruction.add_operand(c);

        instruction
    }
}

fn bytes_to_words(bytes: &[u8]) -> Vec<Word> {
    bytes
        .chunks(4)
        .map(|chars| chars.iter().rev().fold(0u32, |u, c| (u << 8) | *c as u32))
        .collect()
}

fn string_to_words(input: &str) -> Vec<Word> {
    let bytes = input.as_bytes();

    debug_str_bytes_to_words(bytes)
}

/// Convert bytes to a vector of SPIR-V words, replacing NUL bytes with `?`.
///
/// (Using the replacement character or NUL symbol would require changing
/// the length of the string, which would complicate chunking of the
/// program source.)
fn debug_str_bytes_to_words(bytes: &[u8]) -> Vec<Word> {
    let sanitized;
    let bytes = if bytes.contains(&0) {
        sanitized = bytes
            .iter()
            .map(|&b| if b == 0 { b'?' } else { b })
            .collect::<Vec<_>>();
        &sanitized[..]
    } else {
        bytes
    };

    let mut words = bytes_to_words(bytes);
    if bytes.len().is_multiple_of(4) {
        // nul-termination
        words.push(0x0u32);
    }

    words
}

/// split a string into chunks and keep utf8 valid
fn string_to_byte_chunks(input: &str, limit: usize) -> Vec<&[u8]> {
    let mut offset: usize = 0;
    let mut start: usize = 0;
    let mut words = vec![];
    while offset < input.len() {
        offset = input.floor_char_boundary_polyfill(offset + limit);
        // Clippy wants us to call as_bytes() first to avoid the UTF-8 check,
        // but we want to assert the output is valid UTF-8.
        #[allow(clippy::sliced_string_as_bytes)]
        words.push(input[start..offset].as_bytes());
        start = offset;
    }

    words
}

///HACK: this is taken from std unstable, remove it when std's floor_char_boundary is stable
/// and available in our msrv.
trait U8Internal {
    fn is_utf8_char_boundary_polyfill(&self) -> bool;
}

impl U8Internal for u8 {
    fn is_utf8_char_boundary_polyfill(&self) -> bool {
        // This is bit magic equivalent to: b < 128 || b >= 192
        (*self as i8) >= -0x40
    }
}

trait StrUnstable {
    fn floor_char_boundary_polyfill(&self, index: usize) -> usize;
}

impl StrUnstable for str {
    fn floor_char_boundary_polyfill(&self, index: usize) -> usize {
        if index >= self.len() {
            self.len()
        } else {
            let lower_bound = index.saturating_sub(3);
            let new_index = self.as_bytes()[lower_bound..=index]
                .iter()
                .rposition(|b| b.is_utf8_char_boundary_polyfill());

            // We know that the character boundary will be within four bytes.
            lower_bound + new_index.unwrap()
        }
    }
}
