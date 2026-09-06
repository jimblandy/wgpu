/*! A builder for SPIR-V modules. */

use super::instruction::Instruction;
use crate::back;

use alloc::vec; // the macro
use alloc::vec::Vec;
use spirv::{Op, Word};

pub use spirv::Capability;

pub struct Builder {
    /// The first words of the SPIR-V module.
    first_words: FirstWords,

    /// The set of capabilities used by this module.
    ///
    /// If `capabilities_available` is `Some`, then this is always a subset of
    /// that.
    capabilities_used: crate::FastIndexSet<Capability>,

    /// The set of spirv extensions used.
    extensions_used: crate::FastIndexSet<&'static str>,

    /// The `OpExtInstImport` instructions generated so far.
    ext_inst_imports: Vec<Word>,

    /// `OpEntryPoint` instructions.
    entry_points: Vec<Word>,

    /// `OpExecutionMode` instructions for those entry points.
    execution_modes: Vec<Word>,

    /// Debug instructions supplying source text and filenames:
    /// `OpString`, `OpSourceExtension`, `OpSource`, and `OpSourceContinued`
    debug_text: Vec<Word>,

    /// All `OpName` and `OpMemberName` instructions.
    debug_names: Vec<Word>,

    /// Decoration instructions.
    decorations: Vec<Word>,

    /// Types, constants, and global variables.
    declarations: Vec<Word>,

    /// Function bodies.
    functions: Vec<Word>,
}

/// The first words of the SPIR-V module.
///
/// Usually you'd call this a "header", but SPIR-V uses that term for
/// describing control flow; "first words" is the terminology used in
/// the SPIR-V specification.
#[derive(Clone)]
struct FirstWords {
    magic_number: Word,
    version: Word,
    generator: Word,
    bound: Word,
    instruction_schema: Word,
}

// https://github.com/KhronosGroup/SPIRV-Headers/pull/195
const GENERATOR: Word = 28;

impl Builder {
    /// Create a [`Builder`][Self] ready to create a new SPIR-V module.
    ///
    /// The pair `(major_version, minor_version)` gives the SPIR-V
    /// major and minor version to include in the SPIR-V module's
    /// first words.
    pub fn new((major_version, minor_version): (u8, u8)) -> Self {
        let version = ((major_version as u32) << 16) | ((minor_version as u32) << 8);
        let first_words = FirstWords {
            magic_number: spirv::MAGIC_NUMBER,
            version,
            generator: GENERATOR,
            bound: 0,
            instruction_schema: 0,
        };

        Builder {
            first_words,
            capabilities_used: Default::default(),
            extensions_used: Default::default(),
            ext_inst_imports: vec![],
            entry_points: vec![],
            execution_modes: vec![],
            debug_text: vec![],
            debug_names: vec![],
            decorations: vec![],
            declarations: vec![],
            functions: vec![],
        }
    }

    #[inline]
    pub fn next_id(&mut self) -> Word {
        let id = self.first_words.bound;
        self.first_words.bound += 1;
        id
    }

    pub fn build(&self) -> Vec<u32> {
        let mut out = Vec::with_capacity(FirstWords::LEN);

        self.first_words.write(&mut out);

        // Quoth the SPIR-V spec:
        //
        // "The instructions of a SPIR-V module must be in the following order.

        // All OpCapability instructions.
        out.extend(self.capabilities_used.iter().map(|c| *c as u32));

        // Optional OpExtension instructions (extensions to SPIR-V).
        for name in &self.extensions_used {
            Instruction::extension(name).to_words(&mut out);
        }

        // Optional OpExtInstImport instructions.
        out.extend(&self.ext_inst_imports);

        // The single required OpMemoryModel instruction.
        let addressing_model = spirv::AddressingModel::Logical;
        let memory_model = if self
            .capabilities_used
            .contains(&Capability::VulkanMemoryModel)
        {
            spirv::MemoryModel::Vulkan
        } else {
            spirv::MemoryModel::GLSL450
        };
        Instruction::memory_model(addressing_model, memory_model).to_words(&mut out);

        // All entry point declarations, using OpEntryPoint.
        out.extend(&self.entry_points);

        // All execution-mode declarations, using OpExecutionMode or OpExecutionModeId.
        out.extend(&self.execution_modes);

        // These debug instructions, which must be grouped in the following order:
        //     All OpString, OpSourceExtension, OpSource, and OpSourceContinued,
        //         without forward references.
        //     All OpName and all OpMemberName.
        //     All OpModuleProcessed instructions.
        out.extend(&self.debug_text);
        out.extend(&self.debug_names);

        // All annotation instructions:
        //     All decoration instructions.
        out.extend(&self.decorations);

        // All type declarations (OpTypeXXX instructions), all constant
        //     instructions, and all global variable declarations (all
        //     OpVariable instructions whose Storage Class is not Function).
        out.extend(&self.declarations);

        // All function definitions (functions with a body).
        out.extend(&self.functions);

        out
    }

    pub fn ext_inst_import(&mut self, name: &str) -> Word {
        let id = self.next_id();
        Instruction::ext_inst_import(id, name).to_words(&mut self.ext_inst_imports);
        id
    }

    // Adding type instructions.

    pub fn type_bool(&mut self, id: Word) {
        let mut instruction = Instruction::new(Op::TypeBool);
        instruction.set_result(id);
        instruction.to_words(&mut self.declarations);
    }

    pub fn type_int(&mut self, id: Word, width: Word, signedness: super::instruction::Signedness) {
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

    //  Annotation Instructions

    pub fn decorate(
        &mut self,
        target_id: Word,
        decoration: spirv::Decoration,
        operands: &[Word],
    ) {
        let mut instruction = Instruction::new(Op::Decorate);
        instruction.add_operand(target_id);
        instruction.add_operand(decoration as u32);
        for operand in operands {
            instruction.add_operand(*operand)
        }
        instruction.to_words(&mut self.decorations);
    }

    pub fn member_decorate(
        &mut self,
        target_id: Word,
        member_index: Word,
        decoration: spirv::Decoration,
        operands: &[Word],
    ) {
        let mut instruction = Instruction::new(Op::MemberDecorate);
        instruction.add_operand(target_id);
        instruction.add_operand(member_index);
        instruction.add_operand(decoration as u32);
        for operand in operands {
            instruction.add_operand(*operand)
        }
        instruction.to_words(&mut self.decorations);
    }

    // Adding constant instructions.

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

    //  Debug Instructions

    pub fn string(&mut self, name: &str, id: Word) {
        let mut instruction = Instruction::new(Op::String);
        instruction.set_result(id);
        instruction.add_operands(string_to_words(name));
        instruction.to_words(&mut self.debug_text);
    }

    pub fn name(&mut self, target_id: Word, name: &str) {
        let mut instruction = Instruction::new(Op::Name);
        instruction.add_operand(target_id);
        instruction.add_operands(string_to_words(name));
        instruction.to_words(&mut self.debug_names);
    }

    pub fn member_name(&mut self, target_id: Word, member: Word, name: &str) {
        let mut instruction = Instruction::new(Op::MemberName);
        instruction.add_operand(target_id);
        instruction.add_operand(member);
        instruction.add_operands(string_to_words(name));
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

impl FirstWords {
    const LEN: usize = 5;

    fn write(&self, out: &mut Vec<u32>) {
        let FirstWords {
            magic_number,
            version,
            generator,
            bound,
            instruction_schema,
        } = *self;

        out.push(magic_number);
        out.push(version);
        out.push(generator);
        out.push(bound);
        out.push(instruction_schema);
    }
}

pub const fn map_storage_class(space: back::ir::AddressSpace) -> spirv::StorageClass {
    use back::ir::AddressSpace as As;
    match space {
        As::Handle => spirv::StorageClass::UniformConstant,
        As::Function => spirv::StorageClass::Function,
        As::Private => spirv::StorageClass::Private,
        As::Storage { .. } => spirv::StorageClass::StorageBuffer,
        As::Uniform => spirv::StorageClass::Uniform,
        As::WorkGroup => spirv::StorageClass::Workgroup,
        As::Immediate => spirv::StorageClass::PushConstant,
        As::TaskPayload => spirv::StorageClass::TaskPayloadWorkgroupEXT,
        As::RayPayload => spirv::StorageClass::RayPayloadKHR,
        As::IncomingRayPayload => spirv::StorageClass::IncomingRayPayloadKHR,
        As::Input => todo!(),
        As::Output => todo!(),
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
