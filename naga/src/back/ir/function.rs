/*! Lowering functinos from Naga IR to backend IR. */

use crate::arena::Handle;
use crate::span::Span;
use crate::{back, ir};
use back::builder::ModuleBuilder;

use alloc::vec;
use alloc::vec::Vec;

impl<'m> back::ir::builder::ModuleBuilder<'m> {
    pub fn lower_function(&mut self, handle: Handle<ir::Function>, function: &ir::Function, builder: &mut ModuleBuilder) {
        let span = self.input.functions.get_span(handle);

        let back_function = back::ir::Function {
            name: function.name.clone(),
            arguments: function.arguments.iter().map(|arg| todo!()).collect(),
            result: todo!(),
            entry_point_info: None,
            function_type: todo!(),
        };

        let lowered_handle = builder.lowered.functions.append(back_function, span);
        self.lowered_functions.insert(handle, lowered_handle);
    }

    pub fn lower_entry_point(
        &self,
        entry_point: &ir::EntryPoint,
        builder: &mut ModuleBuilder,
    ) {
        let stage_info = match entry_point.stage {
            nt::ShaderStage::Vertex => todo!(),
            nt::ShaderStage::Task => todo!(),
            nt::ShaderStage::Mesh => todo!(),
            nt::ShaderStage::Fragment => todo!(),
            nt::ShaderStage::Compute => back::ir::EntryPointStageInfo::Compute {
                workgroup_size: entry_point.workgroup_size,
            },
            nt::ShaderStage::RayGeneration => todo!(),
            nt::ShaderStage::Miss => todo!(),
            nt::ShaderStage::AnyHit => todo!(),
            nt::ShaderStage::ClosestHit => todo!(),
        };

        let function_name = match entry_point.function.name {
            Some(ref name) => name.clone(),
            None => entry_point.name.clone(),
        };

        let entry_point_info = back::ir::EntryPointInfo {
            name: entry_point.name.clone(),
            stage: stage_info,
        };
        
        let arguments = self.entry_point_arguments(entry_point, builder);
        let back_function = back::ir::Function {
            name: Some(function_name),
            arguments,
            entry_point_info: Some(entry_point_info),
            result: todo!(),
            function_type: todo!(),
        };

        // Entry points have no spans: wgpu#10272
        let span = crate::Span::default();

        builder.lowered.functions.append(back_function, span);
    }

    fn entry_point_arguments(
        &self,
        entry_point: &ir::EntryPoint,
        builder: &mut ModuleBuilder,
    ) -> Vec<back::ir::Argument> {
        match self.options.entry_points.io {
            back::ir::option::ShaderStageIoStyle::Arguments => {
                self.entry_point_arguments_with_attributes(entry_point, builder)
            }
            back::ir::option::ShaderStageIoStyle::Globals => {
                self.entry_point_arguments_as_globals(entry_point, builder)
            }
        }
    }

    fn entry_point_arguments_with_attributes(
        &self,
        entry_point: &ir::EntryPoint,
        builder: &mut back::ir::Module,
    ) -> Vec<back::ir::Argument> {
        todo!()
    }

    fn entry_point_arguments_as_globals(
        &self,
        entry_point: &ir::EntryPoint,
        builder: &mut back::ir::Module,
    ) -> Vec<back::ir::Argument> {
        entry_point.function.arguments.iter().map(|argument| {
            let Some(ref binding) = argument.binding else {
                validation_failure!("Every entry point argument should have binding information");
            };

            let attributes = match *binding {
                ir::Binding::BuiltIn(builtin) => {
                    vec![back::ir::Attribute::BuiltIn(builtin)]
                }
                ir::Binding::Location {
                    location,
                    interpolation,
                    sampling,
                    blend_src,
                    per_primitive,
                } => {
                    vec![back::ir::Attribute::UserDefined {
                        location,
                        interpolation,
                        sampling,
                        blend_src,
                        per_primitive,
                    }]
                }
            };

            let ty = todo!();

            back::ir::Argument {
                name: argument.name.clone(),
                ty,
                attributes,
            }
        }).collect()
    }
}
