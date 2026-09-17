/*! HLSL shader models. */

use crate::ir;

/// A HLSL shader model version.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
#[cfg_attr(feature = "deserialize", derive(serde::Deserialize))]
pub enum ShaderModel {
    V5_0,
    V5_1,
    V6_0,
    V6_1,
    V6_2,
    V6_3,
    V6_4,
    V6_5,
    V6_6,
    V6_7,
    V6_8,
    V6_9,
}

impl ShaderModel {
    pub const fn to_str(self) -> &'static str {
        match self {
            Self::V5_0 => "5_0",
            Self::V5_1 => "5_1",
            Self::V6_0 => "6_0",
            Self::V6_1 => "6_1",
            Self::V6_2 => "6_2",
            Self::V6_3 => "6_3",
            Self::V6_4 => "6_4",
            Self::V6_5 => "6_5",
            Self::V6_6 => "6_6",
            Self::V6_7 => "6_7",
            Self::V6_8 => "6_8",
            Self::V6_9 => "6_9",
        }
    }
}

pub const fn shader_stage_to_hlsl_str(st: ir::ShaderStage) -> &'static str {
    use ir::ShaderStage as Ss;
    match st {
        Ss::Vertex => "vs",
        Ss::Fragment => "ps",
        Ss::Compute => "cs",
        Ss::Task => "as",
        Ss::Mesh => "ms",
        Ss::RayGeneration | Ss::AnyHit | Ss::ClosestHit | Ss::Miss => "lib",
    }
}
