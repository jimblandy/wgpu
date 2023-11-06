use std::num::NonZeroU32;

use crate::front::wgsl::parse::ast;
use crate::{Handle, Span};

use crate::front::wgsl::error::Error;
use crate::front::wgsl::lower::{ExpressionContext, Loaded, Lowerer, Typed};
use crate::front::wgsl::Scalar;

/// A cooked form of `ast::ConstructorType` that uses Naga types whenever
/// possible.
enum Constructor<T> {
    /// A vector construction whose component type is inferred from the
    /// argument: `vec3(1.0)`.
    PartialVector { size: crate::VectorSize },

    /// A matrix construction whose component type is inferred from the
    /// argument: `mat2x2(1,2,3,4)`.
    PartialMatrix {
        columns: crate::VectorSize,
        rows: crate::VectorSize,
    },

    /// An array whose component type and size are inferred from the arguments:
    /// `array(3,4,5)`.
    PartialArray,

    /// A known Naga type.
    ///
    /// When we match on this type, we need to see the `TypeInner` here, but at
    /// the point that we build this value we'll still need mutable access to
    /// the module later. To avoid borrowing from the module, the type parameter
    /// `T` is `Handle<Type>` initially. Then we use `borrow_inner` to produce a
    /// version holding a tuple `(Handle<Type>, &TypeInner)`.
    ///
    /// This always corresponds to a WGSL concrete type: only Partial
    /// constructors can produce abstract types.
    Type(T),
}

impl Constructor<Handle<crate::Type>> {
    /// Return an equivalent `Constructor` value that includes borrowed
    /// `TypeInner` values alongside any type handles.
    ///
    /// The returned form is more convenient to match on, since the patterns
    /// can actually see what the handle refers to.
    fn borrow_inner(
        self,
        module: &crate::Module,
    ) -> Constructor<(Handle<crate::Type>, &crate::TypeInner)> {
        match self {
            Constructor::PartialVector { size } => Constructor::PartialVector { size },
            Constructor::PartialMatrix { columns, rows } => {
                Constructor::PartialMatrix { columns, rows }
            }
            Constructor::PartialArray => Constructor::PartialArray,
            Constructor::Type(handle) => Constructor::Type((handle, &module.types[handle].inner)),
        }
    }
}

impl Constructor<(Handle<crate::Type>, &crate::TypeInner)> {
    fn to_error_string(&self, ctx: &ExpressionContext) -> String {
        match *self {
            Self::PartialVector { size } => {
                format!("vec{}<?>", size as u32,)
            }
            Self::PartialMatrix { columns, rows } => {
                format!("mat{}x{}<?>", columns as u32, rows as u32,)
            }
            Self::PartialArray => "array<?, ?>".to_string(),
            Self::Type((handle, _inner)) => ctx.format_type(handle),
        }
    }
}

enum Components<'a> {
    None,
    One {
        component: Typed<Handle<crate::Expression>>,
        span: Span,
        ty_inner: &'a crate::TypeInner,
    },
    Many {
        components: Vec<Typed<Handle<crate::Expression>>>,
        spans: Vec<Span>,
    },
}

impl<'source, 'temp> Lowerer<'source, 'temp> {
    /// Generate Naga IR for a type constructor expression.
    ///
    /// The `constructor` value represents the head of the constructor
    /// expression, which is at least a hint of which type is being built; if
    /// it's one of the `Partial` variants, we need to consider the argument
    /// types as well.
    ///
    /// This is used for [`Construct`] expressions, but also for [`Call`]
    /// expressions, once we've determined that the "callable" (in WGSL spec
    /// terms) is actually a type.
    ///
    /// [`Construct`]: ast::Expression::Construct
    /// [`Call`]: ast::Expression::Call
    pub fn construct(
        &mut self,
        span: Span,
        constructor: &ast::ConstructorType<'source>,
        ty_span: Span,
        components: &[Handle<ast::Expression<'source>>],
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Loaded<Handle<crate::Expression>>, Error<'source>> {
        let constructor_h = self.constructor(constructor, ctx)?;

        let components = match *components {
            [] => Components::None,
            [component] => {
                let span = ctx.ast_expressions.get_span(component);
                let component = self.expression(component, ctx)?;
                let ty_inner = super::resolve_inner!(ctx, component.handle());

                Components::One {
                    component,
                    span,
                    ty_inner,
                }
            }
            ref ast_components @ [_, _, ..] => {
                let components = ast_components
                    .iter()
                    .map(|&expr| self.expression(expr, ctx))
                    .collect::<Result<_, _>>()?;
                let spans = ast_components
                    .iter()
                    .map(|&expr| ctx.ast_expressions.get_span(expr))
                    .collect();

                Components::Many { components, spans }
            }
        };

        // Even though we computed `constructor_h` above, wait until now to borrow
        // a reference to the `TypeInner`, so that the component-handling code
        // above can have mutable access to the type arena.
        let constructor = constructor_h.borrow_inner(ctx.module);

        // Build a Naga expression for this construction.
        //
        // Construction operations never operate on or produce references. We
        // applied `Lowerer::expression` above to apply the Load Rule to
        // everything.
        //
        // Whenever `constructor` is `Constructor::Type`, the result is a
        // concrete type. Otherwise, we need to look at the components to
        // determine whether the result is abstract or concrete.
        let expr = match (components, constructor) {
            // Empty constructor
            (Components::None, dst_ty) => match dst_ty {
                Constructor::Type((result_ty, _)) => {
                    let zero =
                        ctx.append_expression(crate::Expression::ZeroValue(result_ty), span)?;
                    return Ok(Typed::Loaded(zero));
                }
                Constructor::PartialVector { .. }
                | Constructor::PartialMatrix { .. }
                | Constructor::PartialArray => {
                    // We have no arguments from which to infer the result type, so
                    // partial constructors aren't acceptable here.
                    return Err(Error::TypeNotInferrable(ty_span));
                }
            },

            // Scalar constructor & conversion (scalar -> scalar)
            (
                Components::One {
                    component,
                    ty_inner: &crate::TypeInner::Scalar { .. },
                    ..
                },
                Constructor::Type((_, &crate::TypeInner::Scalar { kind, width })),
            ) => cast_to_concrete(component, kind, width),

            // Vector conversion (vector -> vector)
            (
                Components::One {
                    component,
                    ty_inner: &crate::TypeInner::Vector { size: src_size, .. },
                    ..
                },
                Constructor::Type((
                    _,
                    &crate::TypeInner::Vector {
                        size: dst_size,
                        kind: dst_kind,
                        width: dst_width,
                    },
                )),
            ) if dst_size == src_size => cast_to_concrete(component, dst_kind, dst_width),

            // Vector conversion (vector -> vector) - partial
            (
                Components::One {
                    component,
                    ty_inner: &crate::TypeInner::Vector { size: src_size, .. },
                    ..
                },
                Constructor::PartialVector { size: dst_size },
            ) if dst_size == src_size => {
                // This is a trivial conversion: the sizes match, and a Partial
                // constructor doesn't specify a scalar type, so nothing can
                // possibly happen.
                return Ok(component);
            }

            // Matrix conversion (matrix -> matrix)
            (
                Components::One {
                    component,
                    ty_inner:
                        &crate::TypeInner::Matrix {
                            columns: src_columns,
                            rows: src_rows,
                            ..
                        },
                    ..
                },
                Constructor::Type((
                    _,
                    &crate::TypeInner::Matrix {
                        columns: dst_columns,
                        rows: dst_rows,
                        width: dst_width,
                    },
                )),
            ) if dst_columns == src_columns && dst_rows == src_rows => {
                cast_to_concrete(component, crate::ScalarKind::Float, dst_width)
            }

            // Matrix conversion (matrix -> matrix) - partial
            (
                Components::One {
                    component,
                    ty_inner:
                        &crate::TypeInner::Matrix {
                            columns: src_columns,
                            rows: src_rows,
                            ..
                        },
                    ..
                },
                Constructor::PartialMatrix {
                    columns: dst_columns,
                    rows: dst_rows,
                },
            ) if dst_columns == src_columns && dst_rows == src_rows => {
                // This is a trivial conversion: the sizes match, and a Partial
                // constructor doesn't specify a scalar type, so nothing can
                // possibly happen.
                return Ok(component);
            }

            // Vector constructor (splat) - infer type
            (
                Components::One {
                    component,
                    ty_inner: &crate::TypeInner::Scalar { .. },
                    ..
                },
                Constructor::PartialVector { size },
            ) => {
                // Preserve scalar component abstractness.
                component.map(|handle| crate::Expression::Splat {
                    size,
                    value: handle,
                })
            }

            // Vector constructor (splat)
            (
                Components::One {
                    component,
                    ty_inner:
                        &crate::TypeInner::Scalar {
                            kind: src_kind,
                            width: src_width,
                            ..
                        },
                    ..
                },
                Constructor::Type((
                    _,
                    &crate::TypeInner::Vector {
                        size,
                        kind: dst_kind,
                        width: dst_width,
                    },
                )),
            ) if dst_kind == src_kind || dst_width == src_width => {
                let component = ctx.cast_abstract_to_scalar(component, dst_kind, dst_width)?;
                Typed::Loaded(crate::Expression::Splat {
                    size,
                    value: component,
                })
            }

            // Vector constructor (by elements), partial
            (Components::Many { components, spans }, Constructor::PartialVector { size }) => {
                let scalar = component_scalar_from_constructor_args(&components, ctx).map_err(
                    |index| Error::InvalidConstructorComponentType(spans[index], index as i32),
                )?;
                let inner = scalar.to_inner_vector(size);
                let ty = ctx.ensure_type_exists(inner);
                crate::Expression::Compose { ty, components }
            }

            // Vector constructor (by elements), full type given
            (
                Components::Many { components, .. },
                Constructor::Type((ty, &crate::TypeInner::Vector { .. })),
            ) => crate::Expression::Compose { ty, components },

            // Matrix constructor (by elements)
            (
                Components::Many { components, spans },
                Constructor::PartialMatrix { columns, rows },
            )
            | (
                Components::Many { components, spans },
                Constructor::Type((_, &crate::TypeInner::Matrix { columns, rows, .. })),
            ) if components.len() == columns as usize * rows as usize => {
                let scalar = component_scalar_from_constructor_args(&components, ctx).map_err(
                    |index| Error::InvalidConstructorComponentType(spans[index], index as i32),
                )?;
                let vec_ty = ctx.ensure_type_exists(scalar.to_inner_vector(rows));

                let components = components
                    .chunks(rows as usize)
                    .map(|vec_components| {
                        ctx.append_expression(
                            crate::Expression::Compose {
                                ty: vec_ty,
                                components: Vec::from(vec_components),
                            },
                            Default::default(),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let ty = ctx.ensure_type_exists(crate::TypeInner::Matrix {
                    columns,
                    rows,
                    width: scalar.width,
                });
                crate::Expression::Compose { ty, components }
            }

            // Matrix constructor (by columns)
            (
                Components::Many { components, spans },
                Constructor::PartialMatrix { columns, rows },
            )
            | (
                Components::Many { components, spans },
                Constructor::Type((_, &crate::TypeInner::Matrix { columns, rows, .. })),
            ) => {
                let scalar = component_scalar_from_constructor_args(&components, ctx).map_err(
                    |index| Error::InvalidConstructorComponentType(spans[index], index as i32),
                )?;
                let ty = ctx.ensure_type_exists(crate::TypeInner::Matrix {
                    columns,
                    rows,
                    width: scalar.width,
                });
                crate::Expression::Compose { ty, components }
            }

            // Array constructor - infer type
            (components, Constructor::PartialArray) => {
                let components = components.into_components_vec();

                let base = ctx.register_type(components[0])?;

                let inner = crate::TypeInner::Array {
                    base,
                    size: crate::ArraySize::Constant(
                        NonZeroU32::new(u32::try_from(components.len()).unwrap()).unwrap(),
                    ),
                    stride: {
                        self.layouter.update(ctx.module.to_ctx()).unwrap();
                        self.layouter[base].to_stride()
                    },
                };
                let ty = ctx.ensure_type_exists(inner);

                crate::Expression::Compose { ty, components }
            }

            // Array or Struct constructor
            (
                components,
                Constructor::Type((
                    ty,
                    &crate::TypeInner::Array { .. } | &crate::TypeInner::Struct { .. },
                )),
            ) => {
                let components = components.into_components_vec();
                crate::Expression::Compose { ty, components }
            }

            // ERRORS

            // Bad conversion (type cast)
            (Components::One { span, ty_inner, .. }, constructor) => {
                let from_type = ctx.format_typeinner(ty_inner);
                return Err(Error::BadTypeCast {
                    span,
                    from_type,
                    to_type: constructor.to_error_string(ctx),
                });
            }

            // Too many parameters for scalar constructor
            (
                Components::Many { spans, .. },
                Constructor::Type((_, &crate::TypeInner::Scalar { .. })),
            ) => {
                let span = spans[1].until(spans.last().unwrap());
                return Err(Error::UnexpectedComponents(span));
            }

            // Other types can't be constructed
            _ => return Err(Error::TypeNotConstructible(ty_span)),
        };

        let expr = ctx.append_expression(expr, span)?;
        Ok(expr)
    }

    /// Build a [`Constructor`] for a WGSL construction expression.
    ///
    /// If `constructor` conveys enough information to determine which Naga [`Type`]
    /// we're actually building (i.e., it's not a partial constructor), then
    /// ensure the `Type` exists in [`ctx.module`], and return
    /// [`Constructor::Type`].
    ///
    /// Otherwise, return the [`Constructor`] partial variant corresponding to
    /// `constructor`.
    ///
    /// [`Type`]: crate::Type
    /// [`ctx.module`]: ExpressionContext::module
    fn constructor<'out>(
        &mut self,
        constructor: &ast::ConstructorType<'source>,
        ctx: &mut ExpressionContext<'source, '_, 'out>,
    ) -> Result<Constructor<Handle<crate::Type>>, Error<'source>> {
        let handle = match *constructor {
            ast::ConstructorType::Scalar(scalar) => {
                let ty = ctx.ensure_type_exists(scalar.to_inner_scalar());
                Constructor::Type(ty)
            }
            ast::ConstructorType::PartialVector { size } => Constructor::PartialVector { size },
            ast::ConstructorType::Vector { size, scalar } => {
                let ty = ctx.ensure_type_exists(scalar.to_inner_vector(size));
                Constructor::Type(ty)
            }
            ast::ConstructorType::PartialMatrix { columns, rows } => {
                Constructor::PartialMatrix { columns, rows }
            }
            ast::ConstructorType::Matrix {
                rows,
                columns,
                width,
            } => {
                let ty = ctx.ensure_type_exists(crate::TypeInner::Matrix {
                    columns,
                    rows,
                    width,
                });
                Constructor::Type(ty)
            }
            ast::ConstructorType::PartialArray => Constructor::PartialArray,
            ast::ConstructorType::Array { base, size } => {
                let base = self.resolve_ast_type(base, &mut ctx.as_global())?;
                let size = self.array_size(size, &mut ctx.as_global())?;

                self.layouter.update(ctx.module.to_ctx()).unwrap();
                let stride = self.layouter[base].to_stride();

                let ty = ctx.ensure_type_exists(crate::TypeInner::Array { base, size, stride });
                Constructor::Type(ty)
            }
            ast::ConstructorType::Type(ty) => Constructor::Type(ty),
        };

        Ok(handle)
    }
}

/// Compute a vector or matrix's scalar type from those of its
/// constructor arguments.
///
/// Given `components`, the arguments given to a vector or matrix
/// constructor, return the scalar type of the vector or matrix's
/// elements.
///
/// The `components` slice must not be empty. All elements' types must
/// have been resolved.
///
/// If `components` are definitely not acceptable as arguments to such
/// constructors, return `Err(i)`, where `i` is the index in
/// `components` of some problematic argument.
///
/// This function doesn't fully type-check the arguments, so it may
/// return `Ok` even when the Naga validator will reject the resulting
/// construction expression later.
fn component_scalar_from_constructor_args(
    components: &[Handle<crate::Expression>],
    ctx: &mut ExpressionContext<'_, '_, '_>,
) -> Result<Scalar, usize> {
    // Since we don't yet implement abstract types, we can settle for
    // just inspecting the first element.
    let first = components[0];
    ctx.grow_types(first).map_err(|_| 0_usize)?;
    let inner = ctx.typifier()[first].inner_with(&ctx.module.types);
    match Scalar::from_inner(inner) {
        Some(scalar) => Ok(scalar),
        None => Err(0),
    }
}

/// Convert `expr` to a concrete type whose leaves are the given scalar type.
///
/// The Load Rule must have already been applied to `expr`.
fn cast_to_concrete(
    expr: Typed<Handle<crate::Expression>>,
    kind: crate::ScalarKind,
    width: crate::Bytes,
) -> Typed<crate::Expression> {
    match expr {
        // Conveniently, since the `As` expression specifies both kind
        // and width, it will take care of converting any abstract
        // operand to the right concrete type.
        Typed::Abstract(handle) | Typed::Loaded(handle) => Typed::Loaded(crate::Expression::As {
            expr: handle,
            kind,
            convert: Some(width),
        }),
        Typed::Reference(_) => unreachable!(),
    }
}

/// Convert arguments for vector or matrix ('linear') construction.
///
/// Apply WGSL`s [overload resolution] rules for a vector or matrix constructor.
/// The given `components` may be [`Scalar`]s, [`Vector`]s, or a mix. Mutate
/// `components` in place to refer to the converted values. (If no conversions
/// are needed, leave `components` unchanged.)
///
/// This is not appropriate for array constructor arguments. WGSL's constraints
/// on array construction are tighter in some ways (no automatic splat) and
/// looser in others (elements can be any creation-fixed-footprint type), so we
/// handle that elsewhere.
///
/// The Load Rule must have been previously applied to all components.
///
/// [`Scalar`]: crate::TypeInner::Scalar
/// [`Vector`]: crate::TypeInner::Vector
/// [overload resolution]: https://gpuweb.github.io/gpuweb/wgsl/#overload-resolution-section
fn find_consensus_type<'source>(
    constructor: &Constructor<(Handle<crate::Type>, &crate::TypeInner)>,
    components: &[Typed<Handle<crate::Expression>>],
    spans: &[Span],
    ctx: &mut ExpressionContext<'source, '_, '_>,
) -> Result<Typed<(crate::ScalarKind, crate::Bytes)>, Error<'source>> {
    // Track the most general type we've seen so far, and the index of the last
    // component that influenced it, for error reporting.
    let mut best: Option<(Typed<(crate::ScalarKind, crate::Bytes)>, usize)> = None;

    for (i, component) in components.iter().enumerate() {
        let scalar_type = component.try_map(|handle| {
            let inner = super::resolve_inner!(ctx, handle);
            leaf_scalar(inner, ctx.module)
                .ok_or_else(|| Error::UnexpectedComponents(spans[i].clone()))
        })?;
        best = match best {
            None => Some((scalar_type, i)),
            Some((prev_best, prev_best_index)) => {
                match join_scalar_types(prev_best, scalar_type) {
                    Some(new) => {
                        // If considering this component changed our join type,
                        // then adopt the new join type, and cite this component
                        // in any subsequent error messages.
                        let index = if prev_best != new { i } else { prev_best_index };
                        Some((new, index))
                    }
                    None => {
                        return Err(Error::IrreconcilableComponents {
                            r#type: constructor.to_error_string(ctx),
                            this: spans[prev_best_index],
                            that: spans[i],
                        })
                    }
                }
            }
        };
    }

    // This `unwrap` is okay because `components` should never be empty.
    Ok(best.unwrap().0)
}

/// Return the least upper bound of the scalar types `left` and `right`.
///
/// The expressions in question should have had the Load Rule applied to them:
/// `left` and `right` must not be WGSL reference types.
fn join_scalar_types(
    left: Typed<(crate::ScalarKind, crate::Bytes)>,
    right: Typed<(crate::ScalarKind, crate::Bytes)>,
) -> Option<Typed<(crate::ScalarKind, crate::Bytes)>> {
    use crate::ScalarKind as Sk;
    match (left, right) {
        (Typed::Abstract(left), Typed::Abstract(right)) => {
            let kind = match (left.0, right.0) {
                // Ints coerce to floats, but not vice-versa.
                (Sk::Float, _) | (_, Sk::Float) => Sk::Float,
                (Sk::Sint, Sk::Sint) => Sk::Sint,
                // The WGSL front end should never produce abstract unsigned
                // ints or booleans.
                (Sk::Uint | Sk::Bool, _) | (_, Sk::Uint | Sk::Bool) => unreachable!(),
            };
            // All our abstract types are 64 bits long.
            Some(Typed::Abstract((kind, 8)))
        }
        (Typed::Abstract(r#abstract), Typed::Loaded(concrete))
        | (Typed::Loaded(concrete), Typed::Abstract(r#abstract)) => {
            // If we can perform the conversion at all, it should always result
            // in the type of the concrete operand.
            let ok = match (r#abstract.0, concrete.0) {
                // Abstract floats coerce to floats, but not ints.
                (Sk::Float, Sk::Float) => true,
                (Sk::Float, Sk::Sint | Sk::Uint) => false,
                // Abstract ints coerce to floats or ints.
                (Sk::Sint, Sk::Float | Sk::Sint | Sk::Uint) => true,
                // Nothing converts to bool.
                (_, Sk::Bool) => false,
                // The WGSL front end should never produce abstract unsigned
                // ints or booleans.
                (Sk::Uint | Sk::Bool, _) => unreachable!(),
            };

            ok.then_some(Typed::Loaded(concrete))
        }
        (Typed::Loaded(left), Typed::Loaded(right)) => {
            // Concrete types must match. There are only automatic coercions
            // from abstract types.
            (left == right).then_some(Typed::Loaded(left))
        }
        (Typed::Reference(_), _) | (_, Typed::Reference(_)) => unreachable!(),
    }
}

/// Return the scalar kind and width of the leaves of `ty`.
///
/// If `ty` argument is a scalar, vector, or matrix, return the scalar kind and
/// width of its leaves. Otherwise, return `None`.
fn leaf_scalar(
    inner: &crate::TypeInner,
    module: &crate::Module,
) -> Option<(crate::ScalarKind, crate::Bytes)> {
    use crate::TypeInner as Ti;
    match *inner {
        Ti::Scalar { kind, width } | Ti::Vector { kind, width, .. } => Some((kind, width)),
        Ti::Matrix { width, .. } => Some((crate::ScalarKind::Float, width)),
        _ => None,
    }
}
