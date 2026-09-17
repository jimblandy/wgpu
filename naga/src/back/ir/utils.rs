/*! Utility functions for HLSL generation. */

use crate::ir;
use bit_set::BitSet;

/// Compute a bit set of all user-defined input locations accepted by
/// `entry_point` in `module`.
///
/// In Naga IR, it is fine for a vertex shader to produce user-defined
/// outputs that the corresponding fragment shader does not accept; they
/// are ignored. In HLSL, however, such unused outputs are an error. Thus,
/// in order to generate valid HLSL for a vertex shader, we need to know
/// which fragment shader the render pipeline will pair it with, and trim
/// out any user-defined outputs the fragment shader isn't expecting.
///
/// The HLSL backend options include [a map] giving the set of user-defined
/// output locations that each vertex shader should be allowed to produce.
/// These sets are most usefully the user-defined inputs expected by some
/// fragment shader entry point. This function constructs such a set from
/// such an entry point.
///
/// [a map]: crate::back::hlsl2::Options::user_output_masks
pub fn compute_user_input_mask(module: &ir::Module, entry_point: &ir::EntryPoint) -> BitSet {
    let mut input_mask = BitSet::new();
    let mut visit = |binding: &ir::Binding| {
        if let &ir::Binding::Location { location, .. } = binding {
            input_mask.insert(location as usize);
        }
    };

    for (index, arg) in entry_point.function.arguments.iter().enumerate() {
        if let Some(ref binding) = arg.binding {
            visit(binding);
        } else if let ir::TypeInner::Struct { ref members, .. } = module.types[arg.ty].inner {
            for member in members {
                if let Some(ref binding) = member.binding {
                    visit(binding);
                }
            }
        }
    }

    input_mask
}
