/*! Lowering types from Naga IR to backend IR. */

use crate::{back, ir};
use back::ir::builder::ModuleBuilder;

use crate::arena::Handle;
use crate::span::Span;

use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;

impl<'m> back::ir::ModuleContext<'m> {
    pub fn lower_type(
        &self,
        ty: Handle<ir::Type>,
        builder: &mut ModuleBuilder,
    ) -> Handle<back::ir::Type> {
        use ir::TypeInner as Ti;

        // We don't want to use `Entry` here. In the process of lowering this
        // type, we may recurse to lower other types, and you can't have
        // multiple `Entry`s open on a hash table at the same time.
        if let Some(&lowered) = builder.lowered_types.get(&ty) {
            return lowered;
        }

        let r#type = &self.input.types[ty];
        let type_span = self.input.types.get_span(ty);

        match r#type.inner {
            Ti::Scalar(scalar) => self.lower_scalar(scalar, &r#type.name, type_span, builder),
            Ti::Vector { size, scalar } => {
                self.lower_vector(size, scalar, &r#type.name, type_span, builder)
            }
            Ti::Matrix {
                columns,
                rows,
                scalar,
            } => self.lower_matrix(columns, rows, scalar, &r#type.name, type_span, builder),
            Ti::CooperativeMatrix {
                columns,
                rows,
                scalar,
                role,
            } => todo!(),
            Ti::Atomic(scalar) => todo!(),
            Ti::Pointer { base, space } => todo!(),
            Ti::ValuePointer {
                size,
                scalar,
                space,
            } => todo!(),
            Ti::Array { base, size, stride } => todo!(),
            Ti::Struct { ref members, span } => todo!(),
            Ti::Image {
                dim,
                arrayed,
                class,
            } => todo!(),
            Ti::Sampler { comparison } => todo!(),
            Ti::AccelerationStructure { vertex_return } => todo!(),
            Ti::RayQuery { vertex_return } => todo!(),
            Ti::BindingArray { base, size } => todo!(),
        }
    }

    fn lower_scalar(
        &self,
        scalar: ir::Scalar,
        name: &Option<String>,
        span: Span,
        builder: &mut ModuleBuilder,
    ) -> Handle<back::ir::Type> {
        let inner = builder
            .module
            .inner_types
            .insert(back::ir::TypeInner::Scalar(scalar), span.clone());
        let low_type = back::ir::Type {
            name: name.clone(),
            inner,
        };
        builder.module.types.insert(low_type, span)
    }

    fn lower_vector(
        &self,
        size: ir::VectorSize,
        scalar: ir::Scalar,
        name: &Option<String>,
        span: Span,
        builder: &mut ModuleBuilder,
    ) -> Handle<back::ir::Type> {
        let low_scalar = self.lower_scalar(scalar, name, span.clone(), builder);
        let low_inner = builder.module.inner_types.insert(
            back::ir::TypeInner::Vector {
                size,
                scalar: low_scalar,
            },
            span.clone(),
        );
        let low_type = back::ir::Type {
            name: name.clone(),
            inner: low_inner,
        };
        builder.module.types.insert(low_type, span)
    }

    fn lower_matrix(
        &self,
        columns: ir::VectorSize,
        rows: ir::VectorSize,
        scalar: ir::Scalar,
        name: &Option<String>,
        span: Span,
        builder: &mut ModuleBuilder,
    ) -> Handle<back::ir::Type> {
        use back::ir::MatrixComponent as Mc;
        let element = self.lower_vector(rows, scalar, &None, span.clone(), builder);
        let low_inner = back::ir::TypeInner::Matrix {
            indexing: Mc::Column,
            layout: Mc::Column,
            size: columns,
            element,
        };
        let low_inner_handle = builder.module.inner_types.insert(low_inner, span.clone());
        let low_type = back::ir::Type {
            name: name.clone(),
            inner: low_inner_handle,
        };
        builder.module.types.insert(low_type, span)
    }
}
