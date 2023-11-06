use super::Loaded;
use crate::{ScalarKind, TypeInner};
use crate::front::wgsl::Scalar;

/// What sort of automatic conversion is needed.
#[derive(Clone, Copy, Debug)]
pub enum Conversion {
    /// The source and destination types are identical.
    Trivial,

    /// The source can be cast to the destination type with [`Expression::As`].
    ///
    /// [`Expression::As`]: crate::Expression::As
    ScalarCast(Scalar),

    /// The source cannot be converted to the destination.
    None,
}

/// Determine whether WGSL automatic conversions can convert  `from` to `to`.
///
/// If so, return the scalar type of `to`.
pub(crate) fn find_automatic_conversion(
    from: Loaded<&TypeInner>,
    to: Loaded<&TypeInner>,
    module: &crate::Module,
) -> Option<Scalar> {
    todo!();
}

pub fn convert_loaded(
    from: Loaded<&TypeInner>,
    to: Loaded<&TypeInner>,
    module: &crate::Module,
) -> Conversion {
    match (from, to) {
        (Loaded::Concrete(from), Loaded::Concrete(to)) => {
            // The only automatic conversion that applies to concrete
            // types is the Load Rule, but since `from` and `to` are
            // `Loaded<...>` we know that's already been applied, if
            // possible.
            if from.equivalent(to, &module.types) {
                Conversion::Trivial
            } else {
                Conversion::None
            }
        }
        (Loaded::Abstract(from), Loaded::Concrete(to)) => {
            convert_inner_abstract_to_concrete(from, to, module)
        }
        (Loaded::Abstract(from), Loaded::Abstract(to)) => {
            convert_inner_abstract_to_abstract(from, to, module)
        }
        (Loaded::Concrete(_), Loaded::Abstract(_)) => Conversion::None,
    }
}

fn convert_inner_abstract_to_concrete(
    from: &TypeInner,
    to: &TypeInner,
    module: &crate::Module,
) -> Conversion {
    convert_inner_with(from, to, module, convert_scalar_abstract_to_concrete)
}

fn convert_inner_abstract_to_abstract(
    from: &TypeInner,
    to: &TypeInner,
    module: &crate::Module,
) -> Conversion {
    convert_inner_with(from, to, module, convert_scalar_abstract_to_abstract)
}

fn convert_inner_with(
    from: &TypeInner,
    to: &TypeInner,
    module: &crate::Module,
    mut scalars_convertible: impl FnMut(Scalar, Scalar) -> Conversion,
) -> Conversion {
    use TypeInner as Ti;

    match (*from, *to) {
        (
            Ti::Scalar {
                kind: from_kind,
                width: 8,
            },
            Ti::Scalar {
                kind: to_kind,
                width: to_width,
            },
        ) => scalars_convertible(
            Scalar { kind: from_kind, width: 8},
            Scalar { kind: to_kind, width: to_width }
        ),
        (
            Ti::Vector {
                size: from_size,
                kind: from_kind,
                width: 8,
            },
            Ti::Vector {
                size: to_size,
                kind: to_kind,
                width: to_width,
            },
        ) => {
            if from_size != to_size {
                Conversion::None
            } else {
                scalars_convertible(
                    Scalar { kind: from_kind, width: 8 },
                    Scalar { kind: to_kind, width: to_width },
                )
            }
        }
        (
            Ti::Matrix {
                width: 8,
                columns: from_columns,
                rows: from_rows,
            },
            Ti::Matrix {
                columns: to_columns,
                rows: to_rows,
                width: to_width,
            },
        ) => {
            if from_columns != to_columns || from_rows != to_rows {
                Conversion::None
            } else {
                scalars_convertible(
                    Scalar { kind: ScalarKind::Float, width: 8 },
                    Scalar { kind: ScalarKind::Float, width: to_width },
                )
            }
        }
        (
            Ti::Array {
                base: from_base,
                size: from_size,
                stride: from_stride,
            },
            Ti::Array {
                base: to_base,
                size: to_size,
                stride: to_stride,
            },
        ) => {
            if from_size == to_size && from_stride == to_stride {
                let from_inner = &module.types[from_base].inner;
                let to_inner = &module.types[to_base].inner;
                convert_inner_with(from_inner, to_inner, module, scalars_convertible)
            } else {
                Conversion::None
            }
        }
        _ => Conversion::None,
    }
}

/// Return true if WGSL's automatic conversions permit changing a scalar,
/// vector, matrix, or nested array of such whose leaves are abstract scalars of
/// kind `from` to the same type with concrete leaves of kind `to`.
fn convert_scalar_abstract_to_concrete(from: Scalar, to: Scalar) -> Conversion {
    use ScalarKind as Sk;
    match (from.kind, to.kind) {
        // There are no automatic conversions from unsigned or bools.
        (Sk::Uint | Sk::Bool, _) => Conversion::None,

        // There are no automatic conversions to bools.
        (_, Sk::Bool) => Conversion::None,

        // There are no conversions from floats to ints.
        (Sk::Float, Sk::Sint | Sk::Uint) => Conversion::None,

        // Integer-to-integer and integer-to-float conversions are okay.
        (Sk::Sint, Sk::Uint) | (Sk::Sint, Sk::Float) => Conversion::ScalarCast(to),

        // Conversions can freely change width. (Abstract types are
        // very wide, so these are always lossy.)
        (Sk::Sint, Sk::Sint) | (Sk::Float, Sk::Float) => {
            if from.width == to.width {
                Conversion::Trivial
            } else {
                Conversion::ScalarCast(to)
            }
        }
    }
}

fn convert_scalar_abstract_to_abstract(from: Scalar, to: Scalar) -> Conversion {
    use ScalarKind as Sk;
    match (from.kind, to.kind) {
        // Ints convert to floats.
        (Sk::Sint, Sk::Float) => Conversion::ScalarCast(to),

        // Self-conversions are okay.
        (Sk::Sint, Sk::Sint) | (Sk::Float, Sk::Float) => {
            if from.width == to.width {
                Conversion::Trivial
            } else {
                Conversion::ScalarCast(to)
            }
        }

        // Nothing else is allowed.
        _ => Conversion::None,
    }
}
