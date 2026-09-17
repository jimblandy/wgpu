/*! A builder type for backend IR modules. */

use crate::{back, ir, valid};
use crate::FastHashMap;
use crate::arena::Handle;

pub struct ModuleBuilder<'m> {
    /// The Naga IR module we're lowering.
    pub input: &'m ir::Module,
    pub info: &'m valid::ModuleInfo,

    /// Options controlling how `input` should be lowered.
    pub options: back::ir::option::Options,

    /// The lowered module under construction.
    pub lowered: back::ir::Module,

    /// A map from Naga IR types to their backend IR equivalents.
    pub lowered_types: FastHashMap<Handle<ir::Type>, Handle<back::ir::Type>>,

    /// A map from Naga IR types to the backend type used to store
    /// them in memory, if that is different from their counterpart in
    /// `lowered_types`.
    pub store_types: FastHashMap<Handle<ir::Type>, StoreType>,

    /// A map from Naga IR functions to their backend IR renderings.
    pub lowered_functions: FastHashMap<Handle<ir::Function>, Handle<back::ir::Function>>,
}

/// The backend type used to store values of some original Naga IR type.
///
/// This says how lowering should render loads and stores of the
/// original type, and form pointers to fields or elements of the
/// original type in memory.
#[derive(Debug)]
pub enum StoreType {
    /// The original type is a matrix, which the backend represents as
    /// a struct with a member for each column.
    ///
    /// See [`TypeOptions::replace_cx2_matrix_with_struct`][o].
    ///
    /// [o]: back::ir::option::TypeOptions::replace_cx2_matrix_with_struct
    StructOfColumns(Handle<back::ir::Type>),
}

impl<'m> ModuleBuilder<'m> {
    pub fn new(module: &'m ir::Module, info: &'m valid::ModuleInfo, options: back::ir::option::Options) -> ModuleBuilder<'m> {
        let mut builder = Self {
            input: module,
            info,
            options,
            lowered: back::ir::Module::default(),
            lowered_types: Default::default(),
            store_types: Default::default(),
            lowered_functions: Default::default(),
        };

        builder.lowered_types.reserve(module.types.len());
        builder.lowered_functions.reserve(module.functions.len());

        builder
    }
}
