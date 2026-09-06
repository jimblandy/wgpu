/*! Constructing SPIR-V types for Naga backend IR types. */

use super::{builder, Builder};

use crate::arena::Handle;
use crate::back;

use spirv::Word;

use alloc::vec::Vec;

impl<'m> super::Context<'m> {
    pub fn generate_types(&mut self, builder: &mut Builder) {
        for (_handle, ty) in self.module.types.iter() {
            self.generate_type(ty, builder);
        }
    }

    fn type_id(&self, ty: Handle<back::ir::Type>) -> Word {
        self.ir_types[self.module.types[ty].inner]
    }

    fn generate_type(&mut self, ty: &back::ir::Type, builder: &mut Builder) {
        use back::ir::TypeInner as Ti;
        let id = builder.next_id();
        match self.module.inner_types[ty.inner] {
            Ti::Scalar(scalar) => self.generate_scalar(id, scalar, builder),
            Ti::Vector { size, scalar } => {
                let scalar_id = self.type_id(scalar);
                builder.type_vector(id, scalar_id, size);
            }
            Ti::Matrix {
                columns,
                column_type,
            } => {
                let column_type_id = self.type_id(column_type);
                builder.type_matrix(id, column_type_id, columns);
            }
            Ti::Pointer { base, space } => {
                let base_id = self.type_id(base);
                let storage_class = builder::map_storage_class(space);
                builder.type_pointer(id, storage_class, base_id);
            }
            Ti::Array {
                base,
                size: back::ir::ArraySize::Constant { size, size_type },
            } => {
                let element_type_id = self.type_id(base);
                let size_type_id = self.type_id(size_type);
                let length_id = builder.next_id();
                builder.constant_32bit(size_type_id, length_id, size);
                builder.type_array(id, element_type_id, length_id);
            }
            Ti::Array {
                base,
                size: back::ir::ArraySize::Dynamic,
            } => {
                let element_type_id = self.type_id(base);
                builder.type_runtime_array(id, element_type_id);
            }
            Ti::Struct { ref members } => {
                self.generate_struct(id, members, builder);
            }
            Ti::Atomic(_) => lowering_failure!("requested no atomic types"),
            Ti::Image(ref image_type) => {
                self.generate_image(id, image_type, builder);
            }
            Ti::Sampler { comparison } => todo!(),
            Ti::AccelerationStructure { vertex_return } => todo!(),
            Ti::RayQuery { vertex_return } => todo!(),
            Ti::BindingArray { base, size } => todo!(),
        };
        self.ir_types.insert(ty.inner, id);
    }

    fn generate_scalar(&mut self, id: Word, scalar: back::ir::Scalar, builder: &mut Builder) {
        use super::instruction::Signedness;
        use back::ir::ScalarKind as Sk;
        match scalar.kind {
            Sk::Sint => {
                builder.type_int(id, scalar.width as Word, Signedness::Signed);
            }
            Sk::Uint => {
                builder.type_int(id, scalar.width as Word, Signedness::Unsigned);
            }
            Sk::Float => {
                builder.type_float(id, scalar.width as Word);
            }
            Sk::Bool => {
                builder.type_bool(id);
            }
            Sk::AbstractInt | Sk::AbstractFloat => {
                validation_failure!("Abstract integer types should not be present in backend IR");
            }
        }
    }

    fn generate_struct(
        &mut self,
        struct_id: Word,
        members: &[back::ir::StructMember],
        builder: &mut Builder,
    ) {
        builder.type_struct(
            struct_id,
            members.iter().map(|member| self.type_id(member.ty)),
        );
        for (i, member) in members.iter().enumerate() {
            let i = i as u32;
            if let Some(ref name) = member.name {
                builder.member_name(struct_id, i, name);
            }
            builder.member_decorate(struct_id, i, spirv::Decoration::Offset, &[member.offset]);
        }
    }

    fn generate_image(
        &mut self,
        image_id: Word,
        image_type: &back::ir::ImageType, 
        builder: &mut Builder,
    ) {
        todo!()
    }
}
