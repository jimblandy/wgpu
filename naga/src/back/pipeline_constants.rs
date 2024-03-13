use super::PipelineConstants;
use crate::{
    proc::{ConstantEvaluator, ConstantEvaluatorError},
    valid::{Capabilities, ModuleInfo, ValidationError, ValidationFlags, Validator},
    Constant, Expression, Handle, Literal, Module, Override, Scalar, Span, TypeInner, WithSpan,
};
use bit_set::BitSet;
use std::borrow::Cow;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum PipelineConstantError {
    #[error("Missing value for pipeline-overridable constant with identifier string: '{0}'")]
    MissingValue(String),
    #[error("Source f64 value needs to be finite (NaNs and Inifinites are not allowed) for number destinations")]
    SrcNeedsToBeFinite,
    #[error("Source f64 value doesn't fit in destination")]
    DstRangeTooSmall,
    #[error(transparent)]
    ConstantEvaluatorError(#[from] ConstantEvaluatorError),
    #[error(transparent)]
    ValidationError(#[from] WithSpan<ValidationError>),
}

/// Replace all overrides in `module` with constants.
///
/// If no changes are needed, this just returns `Cow::Borrowed`
/// references to `module` and `module_info`. Otherwise, it clones
/// `module`, edits its [`global_expressions`] arena to contain only
/// fully-evaluated expressions, and returns `Cow::Owned` values
/// holding the simplified module and its validation results.
///
/// In either case, the module returned has an empty `overrides`
/// arena, and the `global_expressions` arena contains only
/// fully-evaluated expressions.
///
/// [`global_expressions`]: Module::global_expressions
pub(super) fn process_overrides<'a>(
    module: &'a Module,
    module_info: &'a ModuleInfo,
    pipeline_constants: &PipelineConstants,
) -> Result<(Cow<'a, Module>, Cow<'a, ModuleInfo>), PipelineConstantError> {
    if module.overrides.is_empty() {
        return Ok((Cow::Borrowed(module), Cow::Borrowed(module_info)));
    }

    let mut module = module.clone();

    // A map from override handles to the handles of the constants
    // we've replaced them with.
    let mut override_map = Vec::with_capacity(module.overrides.len());

    // A map from `module`'s original global expression handles to
    // handles in the new, simplified global expression arena.
    let mut adjusted_global_expressions = Vec::with_capacity(module.global_expressions.len());

    // The set of constants whose initializer handles we've already
    // updated to refer to the newly built global expression arena.
    //
    // All constants in `module` must have their `init` handles
    // updated to point into the new, simplified global expression
    // arena. Some of these we can most easily handle as a side effect
    // during the simplification process, but we must handle the rest
    // in a final fixup pass, guided by `adjusted_global_expressions`. We
    // add their handles to this set, so that the final fixup step can
    // leave them alone.
    let mut adjusted_constant_initializers = BitSet::with_capacity(module.constants.len());

    let mut global_expression_kind_tracker = crate::proc::ExpressionKindTracker::new();

    // An iterator through the original overrides table, consumed in
    // approximate tandem with the global expressions.
    let mut override_iter = module.overrides.drain();

    // Do two things in tandem:
    //
    // - Rebuild the global expression arena from scratch, fully
    //   evaluating all expressions, and replacing each `Override`
    //   expression in `module.global_expressions` with a `Constant`
    //   expression.
    //
    // - Build a new `Constant` in `module.constants` to take the
    //   place of each `Override`.
    //
    // Build a map from old global expression handles to their
    // fully-evaluated counterparts in `adjusted_global_expressions` as we
    // go.
    //
    // Why in tandem? Overrides refer to expressions, and expressions
    // refer to overrides, so we can't disentangle the two into
    // separate phases. However, we can take advantage of the fact
    // that the overrides and expressions must form a DAG, and work
    // our way from the leaves to the roots, replacing and evaluating
    // as we go.
    //
    // Although the two loops are nested, this is really two
    // alternating phases: we adjust and evaluate constant expressions
    // until we hit an `Override` expression, at which point we switch
    // to building `Constant`s for `Overrides` until we've handled the
    // one used by the expression. Then we switch back to processing
    // expressions. Because we know they form a DAG, we know the
    // `Override` expressions we encounter can only have initializers
    // referring to global expressions we've already simplified.
    for (old_h, expr, span) in module.global_expressions.drain() {
        let mut expr = match expr {
            Expression::Override(h) => {
                let c_h = if let Some(new_h) = override_map.get(h.index()) {
                    *new_h
                } else {
                    let mut new_h = None;
                    for entry in override_iter.by_ref() {
                        let stop = entry.0 == h;
                        new_h = Some(process_override(
                            entry,
                            pipeline_constants,
                            &mut module,
                            &mut override_map,
                            &adjusted_global_expressions,
                            &mut adjusted_constant_initializers,
                            &mut global_expression_kind_tracker,
                        )?);
                        if stop {
                            break;
                        }
                    }
                    new_h.unwrap()
                };
                Expression::Constant(c_h)
            }
            Expression::Constant(c_h) => {
                adjusted_constant_initializers.insert(c_h.index());
                module.constants[c_h].init = adjusted_global_expressions[c_h.index()];
                expr
            }
            expr => expr,
        };
        let mut evaluator = ConstantEvaluator::for_wgsl_module(
            &mut module,
            &mut global_expression_kind_tracker,
            false,
        );
        adjust_expr(&adjusted_global_expressions, &mut expr);
        let h = evaluator.try_eval_and_append(expr, span)?;
        debug_assert_eq!(old_h.index(), adjusted_global_expressions.len());
        adjusted_global_expressions.push(h);
    }

    // Finish processing any overrides we didn't visit in the loop above.
    for entry in override_iter {
        process_override(
            entry,
            pipeline_constants,
            &mut module,
            &mut override_map,
            &adjusted_global_expressions,
            &mut adjusted_constant_initializers,
            &mut global_expression_kind_tracker,
        )?;
    }

    // Update the initialization expression handles of all `Constant`s
    // and `GlobalVariable`s. Skip `Constant`s we'd already updated en
    // passant.
    for (_, c) in module
        .constants
        .iter_mut()
        .filter(|&(c_h, _)| !adjusted_constant_initializers.contains(c_h.index()))
    {
        c.init = adjusted_global_expressions[c.init.index()];
    }

    for (_, v) in module.global_variables.iter_mut() {
        if let Some(ref mut init) = v.init {
            *init = adjusted_global_expressions[init.index()];
        }
    }

    // Now that the global expression arena has changed, we need to
    // recompute those expressions' types. For the time being, do a
    // full re-validation.
    let mut validator = Validator::new(ValidationFlags::all(), Capabilities::all());
    let module_info = validator.validate_no_overrides(&module)?;

    Ok((Cow::Owned(module), Cow::Owned(module_info)))
}

/// Add a [`Constant`] to `module` for the override `old_h`.
///
/// Add the new `Constant` to `override_map` and `adjusted_constant_initializers`.
fn process_override(
    (old_h, override_, span): (Handle<Override>, Override, Span),
    pipeline_constants: &PipelineConstants,
    module: &mut Module,
    override_map: &mut Vec<Handle<Constant>>,
    adjusted_global_expressions: &[Handle<Expression>],
    adjusted_constant_initializers: &mut BitSet,
    global_expression_kind_tracker: &mut crate::proc::ExpressionKindTracker,
) -> Result<Handle<Constant>, PipelineConstantError> {
    // Determine which key to use for `override_` in `pipeline_constants`.
    let key = if let Some(id) = override_.id {
        Cow::Owned(id.to_string())
    } else if let Some(ref name) = override_.name {
        Cow::Borrowed(name)
    } else {
        unreachable!();
    };

    // Generate a global expression for `override_`'s value, either
    // from the provided `pipeline_constants` table or its initializer
    // in the module.
    let init = if let Some(value) = pipeline_constants.get::<str>(&key) {
        let literal = match module.types[override_.ty].inner {
            TypeInner::Scalar(scalar) => map_value_to_literal(*value, scalar)?,
            _ => unreachable!(),
        };
        let expr = module
            .global_expressions
            .append(Expression::Literal(literal), Span::UNDEFINED);
        global_expression_kind_tracker.insert(expr, crate::proc::ExpressionKind::Const);
        expr
    } else if let Some(init) = override_.init {
        adjusted_global_expressions[init.index()]
    } else {
        return Err(PipelineConstantError::MissingValue(key.to_string()));
    };

    // Generate a new `Constant` to represent the override's value.
    let constant = Constant {
        name: override_.name,
        ty: override_.ty,
        init,
    };
    let h = module.constants.append(constant, span);
    debug_assert_eq!(old_h.index(), override_map.len());
    override_map.push(h);
    adjusted_constant_initializers.insert(h.index());
    Ok(h)
}

/// Replace every expression handle in `expr` with its counterpart
/// given by `new_pos`.
fn adjust_expr(new_pos: &[Handle<Expression>], expr: &mut Expression) {
    let adjust = |expr: &mut Handle<Expression>| {
        *expr = new_pos[expr.index()];
    };
    match *expr {
        Expression::Compose {
            ref mut components, ..
        } => {
            for c in components.iter_mut() {
                adjust(c);
            }
        }
        Expression::Access {
            ref mut base,
            ref mut index,
        } => {
            adjust(base);
            adjust(index);
        }
        Expression::AccessIndex { ref mut base, .. } => {
            adjust(base);
        }
        Expression::Splat { ref mut value, .. } => {
            adjust(value);
        }
        Expression::Swizzle { ref mut vector, .. } => {
            adjust(vector);
        }
        Expression::Load { ref mut pointer } => {
            adjust(pointer);
        }
        Expression::ImageSample {
            ref mut image,
            ref mut sampler,
            ref mut coordinate,
            ref mut array_index,
            ref mut offset,
            ref mut level,
            ref mut depth_ref,
            ..
        } => {
            adjust(image);
            adjust(sampler);
            adjust(coordinate);
            if let Some(e) = array_index.as_mut() {
                adjust(e);
            }
            if let Some(e) = offset.as_mut() {
                adjust(e);
            }
            match *level {
                crate::SampleLevel::Exact(ref mut expr)
                | crate::SampleLevel::Bias(ref mut expr) => {
                    adjust(expr);
                }
                crate::SampleLevel::Gradient {
                    ref mut x,
                    ref mut y,
                } => {
                    adjust(x);
                    adjust(y);
                }
                _ => {}
            }
            if let Some(e) = depth_ref.as_mut() {
                adjust(e);
            }
        }
        Expression::ImageLoad {
            ref mut image,
            ref mut coordinate,
            ref mut array_index,
            ref mut sample,
            ref mut level,
        } => {
            adjust(image);
            adjust(coordinate);
            if let Some(e) = array_index.as_mut() {
                adjust(e);
            }
            if let Some(e) = sample.as_mut() {
                adjust(e);
            }
            if let Some(e) = level.as_mut() {
                adjust(e);
            }
        }
        Expression::ImageQuery {
            ref mut image,
            ref mut query,
        } => {
            adjust(image);
            match *query {
                crate::ImageQuery::Size { ref mut level } => {
                    if let Some(e) = level.as_mut() {
                        adjust(e);
                    }
                }
                _ => {}
            }
        }
        Expression::Unary { ref mut expr, .. } => {
            adjust(expr);
        }
        Expression::Binary {
            ref mut left,
            ref mut right,
            ..
        } => {
            adjust(left);
            adjust(right);
        }
        Expression::Select {
            ref mut condition,
            ref mut accept,
            ref mut reject,
        } => {
            adjust(condition);
            adjust(accept);
            adjust(reject);
        }
        Expression::Derivative { ref mut expr, .. } => {
            adjust(expr);
        }
        Expression::Relational {
            ref mut argument, ..
        } => {
            adjust(argument);
        }
        Expression::Math {
            ref mut arg,
            ref mut arg1,
            ref mut arg2,
            ref mut arg3,
            ..
        } => {
            adjust(arg);
            if let Some(e) = arg1.as_mut() {
                adjust(e);
            }
            if let Some(e) = arg2.as_mut() {
                adjust(e);
            }
            if let Some(e) = arg3.as_mut() {
                adjust(e);
            }
        }
        Expression::As { ref mut expr, .. } => {
            adjust(expr);
        }
        Expression::ArrayLength(ref mut expr) => {
            adjust(expr);
        }
        Expression::RayQueryGetIntersection { ref mut query, .. } => {
            adjust(query);
        }
        Expression::Literal(_)
        | Expression::FunctionArgument(_)
        | Expression::GlobalVariable(_)
        | Expression::LocalVariable(_)
        | Expression::CallResult(_)
        | Expression::RayQueryProceedResult
        | Expression::Constant(_)
        | Expression::Override(_)
        | Expression::ZeroValue(_)
        | Expression::AtomicResult { .. }
        | Expression::WorkGroupUniformLoadResult { .. } => {}
    }
}

fn map_value_to_literal(value: f64, scalar: Scalar) -> Result<Literal, PipelineConstantError> {
    // note that in rust 0.0 == -0.0
    match scalar {
        Scalar::BOOL => {
            // https://webidl.spec.whatwg.org/#js-boolean
            let value = value != 0.0 && !value.is_nan();
            Ok(Literal::Bool(value))
        }
        Scalar::I32 => {
            // https://webidl.spec.whatwg.org/#js-long
            if !value.is_finite() {
                return Err(PipelineConstantError::SrcNeedsToBeFinite);
            }

            let value = value.trunc();
            if value < f64::from(i32::MIN) || value > f64::from(i32::MAX) {
                return Err(PipelineConstantError::DstRangeTooSmall);
            }

            let value = value as i32;
            Ok(Literal::I32(value))
        }
        Scalar::U32 => {
            // https://webidl.spec.whatwg.org/#js-unsigned-long
            if !value.is_finite() {
                return Err(PipelineConstantError::SrcNeedsToBeFinite);
            }

            let value = value.trunc();
            if value < f64::from(u32::MIN) || value > f64::from(u32::MAX) {
                return Err(PipelineConstantError::DstRangeTooSmall);
            }

            let value = value as u32;
            Ok(Literal::U32(value))
        }
        Scalar::F32 => {
            // https://webidl.spec.whatwg.org/#js-float
            if !value.is_finite() {
                return Err(PipelineConstantError::SrcNeedsToBeFinite);
            }

            let value = value as f32;
            if !value.is_finite() {
                return Err(PipelineConstantError::DstRangeTooSmall);
            }

            Ok(Literal::F32(value))
        }
        Scalar::F64 => {
            // https://webidl.spec.whatwg.org/#js-double
            if !value.is_finite() {
                return Err(PipelineConstantError::SrcNeedsToBeFinite);
            }

            Ok(Literal::F64(value))
        }
        _ => unreachable!(),
    }
}

#[test]
fn test_map_value_to_literal() {
    let bool_test_cases = [
        (0.0, false),
        (-0.0, false),
        (f64::NAN, false),
        (1.0, true),
        (f64::INFINITY, true),
        (f64::NEG_INFINITY, true),
    ];
    for (value, out) in bool_test_cases {
        let res = Ok(Literal::Bool(out));
        assert_eq!(map_value_to_literal(value, Scalar::BOOL), res);
    }

    for scalar in [Scalar::I32, Scalar::U32, Scalar::F32, Scalar::F64] {
        for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            let res = Err(PipelineConstantError::SrcNeedsToBeFinite);
            assert_eq!(map_value_to_literal(value, scalar), res);
        }
    }

    // i32
    assert_eq!(
        map_value_to_literal(f64::from(i32::MIN), Scalar::I32),
        Ok(Literal::I32(i32::MIN))
    );
    assert_eq!(
        map_value_to_literal(f64::from(i32::MAX), Scalar::I32),
        Ok(Literal::I32(i32::MAX))
    );
    assert_eq!(
        map_value_to_literal(f64::from(i32::MIN) - 1.0, Scalar::I32),
        Err(PipelineConstantError::DstRangeTooSmall)
    );
    assert_eq!(
        map_value_to_literal(f64::from(i32::MAX) + 1.0, Scalar::I32),
        Err(PipelineConstantError::DstRangeTooSmall)
    );

    // u32
    assert_eq!(
        map_value_to_literal(f64::from(u32::MIN), Scalar::U32),
        Ok(Literal::U32(u32::MIN))
    );
    assert_eq!(
        map_value_to_literal(f64::from(u32::MAX), Scalar::U32),
        Ok(Literal::U32(u32::MAX))
    );
    assert_eq!(
        map_value_to_literal(f64::from(u32::MIN) - 1.0, Scalar::U32),
        Err(PipelineConstantError::DstRangeTooSmall)
    );
    assert_eq!(
        map_value_to_literal(f64::from(u32::MAX) + 1.0, Scalar::U32),
        Err(PipelineConstantError::DstRangeTooSmall)
    );

    // f32
    assert_eq!(
        map_value_to_literal(f64::from(f32::MIN), Scalar::F32),
        Ok(Literal::F32(f32::MIN))
    );
    assert_eq!(
        map_value_to_literal(f64::from(f32::MAX), Scalar::F32),
        Ok(Literal::F32(f32::MAX))
    );
    assert_eq!(
        map_value_to_literal(-f64::from_bits(0x47efffffefffffff), Scalar::F32),
        Ok(Literal::F32(f32::MIN))
    );
    assert_eq!(
        map_value_to_literal(f64::from_bits(0x47efffffefffffff), Scalar::F32),
        Ok(Literal::F32(f32::MAX))
    );
    assert_eq!(
        map_value_to_literal(-f64::from_bits(0x47effffff0000000), Scalar::F32),
        Err(PipelineConstantError::DstRangeTooSmall)
    );
    assert_eq!(
        map_value_to_literal(f64::from_bits(0x47effffff0000000), Scalar::F32),
        Err(PipelineConstantError::DstRangeTooSmall)
    );

    // f64
    assert_eq!(
        map_value_to_literal(f64::MIN, Scalar::F64),
        Ok(Literal::F64(f64::MIN))
    );
    assert_eq!(
        map_value_to_literal(f64::MAX, Scalar::F64),
        Ok(Literal::F64(f64::MAX))
    );
}
