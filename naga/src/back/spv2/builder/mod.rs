/*! [`Builder`]: a builder type for SPIR-V modules. */

mod constant;
mod debug;
mod function;
mod r#type;
mod utils;

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
