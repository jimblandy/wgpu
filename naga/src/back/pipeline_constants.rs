use super::PipelineConstants;
use crate::{Arena, Constant, Expression, Literal, Module, Scalar, Span, TypeInner};
use std::borrow::Cow;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum PipelineConstantError {
    #[error("Missing value for pipeline-overridable constant with identifier string: '{0}'")]
    MissingValue(String),
    #[error("Source f64 value needs to be finite (NaNs and Inifinites are not allowed) for number destinations")]
    SrcNeedsToBeFinite,
    #[error("Source f64 value doesn't fit in destination")]
    DstRangeTooSmall,
}

pub(super) fn process_overrides<'a>(
    module: &'a Module,
    pipeline_constants: &PipelineConstants,
) -> Result<Cow<'a, Module>, PipelineConstantError> {
    if module.overrides.is_empty() {
        return Ok(Cow::Borrowed(module));
    }

    let mut module = module.clone();
    let overrides = std::mem::replace(&mut module.overrides, Arena::new());

    for (_handle, override_, span) in overrides.drain() {
        let key = if let Some(id) = override_.id {
            Cow::Owned(id.to_string())
        } else if let Some(ref name) = override_.name {
            Cow::Borrowed(name)
        } else {
            unreachable!();
        };
        let init = if let Some(value) = pipeline_constants.get::<str>(&key) {
            let literal = match module.types[override_.ty].inner {
                TypeInner::Scalar(scalar) => map_value_to_literal(*value, scalar)?,
                _ => unreachable!(),
            };
            module
                .const_expressions
                .append(Expression::Literal(literal), Span::UNDEFINED)
        } else if let Some(init) = override_.init {
            init
        } else {
            return Err(PipelineConstantError::MissingValue(key.to_string()));
        };
        let constant = Constant {
            name: override_.name,
            ty: override_.ty,
            init,
        };
        module.constants.append(constant, span);
    }

    Ok(Cow::Owned(module))
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

            let value = value.abs().floor() * value.signum();
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

            let value = value.abs().floor() * value.signum();
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
