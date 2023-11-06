use std::num::NonZeroU32;

use crate::front::wgsl::Scalar;
use crate::front::wgsl::error::{Error, ExpectedToken, InvalidAssignmentType};
use crate::front::wgsl::index::Index;
use crate::front::wgsl::parse::number::Number;
use crate::front::wgsl::parse::{ast, conv};
use crate::front::Typifier;
use crate::proc::{
    ensure_block_returns, Alignment, ConstantEvaluator, Emitter, Layouter, ResolveContext,
    TypeResolution,
};
use crate::{Arena, FastHashMap, FastIndexMap, Handle, Span};

mod construction;
mod convert;

/// Resolves the inner type of a given expression.
///
/// Expects a &mut [`ExpressionContext`] and a [`Handle<Expression>`].
///
/// Returns a &[`crate::TypeInner`].
///
/// Ideally, we would simply have a function that takes a `&mut ExpressionContext`
/// and returns a `&TypeResolution`. Unfortunately, this leads the borrow checker
/// to conclude that the mutable borrow lasts for as long as we are using the
/// `&TypeResolution`, so we can't use the `ExpressionContext` for anything else -
/// like, say, resolving another operand's type. Using a macro that expands to
/// two separate calls, only the first of which needs a `&mut`,
/// lets the borrow checker see that the mutable borrow is over.
macro_rules! resolve_inner {
    ($ctx:ident, $expr:expr) => {{
        $ctx.grow_types($expr)?;
        $ctx.typifier()[$expr].inner_with(&$ctx.module.types)
    }};
}
pub(super) use resolve_inner;

/// Resolves the inner types of two given expressions.
///
/// Expects a &mut [`ExpressionContext`] and two [`Handle<Expression>`]s.
///
/// Returns a tuple containing two &[`crate::TypeInner`].
///
/// See the documentation of [`resolve_inner!`] for why this macro is necessary.
#[allow(unused_macros)]
macro_rules! resolve_inner_binary {
    ($ctx:ident, $left:expr, $right:expr) => {{
        $ctx.grow_types($left)?;
        $ctx.grow_types($right)?;
        (
            $ctx.typifier()[$left].inner_with(&$ctx.module.types),
            $ctx.typifier()[$right].inner_with(&$ctx.module.types),
        )
    }};
}

/// Resolves the type of a given expression.
///
/// Expects a &mut [`ExpressionContext`] and a [`Handle<Expression>`].
///
/// Returns a &[`TypeResolution`].
///
/// See the documentation of [`resolve_inner!`] for why this macro is necessary.
macro_rules! resolve {
    ($ctx:ident, $expr:expr) => {{
        $ctx.grow_types($expr)?;
        &$ctx.typifier()[$expr]
    }};
}

/// State for constructing a `crate::Module`.
pub struct GlobalContext<'source, 'temp, 'out> {
    /// The `TranslationUnit`'s expressions arena.
    ast_expressions: &'temp Arena<ast::Expression<'source>>,

    /// The `TranslationUnit`'s types arena.
    types: &'temp Arena<ast::Type<'source>>,

    // Naga IR values.
    /// The map from the names of module-scope declarations to the Naga IR
    /// `Handle`s we have built for them, owned by `Lowerer::lower`.
    globals: &'temp mut FastHashMap<&'source str, LoweredGlobalDecl>,

    /// The module we're constructing.
    module: &'out mut crate::Module,

    const_typifier: &'temp mut Typifier,
}

impl<'source> GlobalContext<'source, '_, '_> {
    fn as_const(&mut self) -> ExpressionContext<'source, '_, '_> {
        ExpressionContext {
            ast_expressions: self.ast_expressions,
            globals: self.globals,
            types: self.types,
            module: self.module,
            const_typifier: self.const_typifier,
            expr_type: ExpressionContextType::Constant,
        }
    }

    fn ensure_type_exists(&mut self, inner: crate::TypeInner) -> Handle<crate::Type> {
        self.module
            .types
            .insert(crate::Type { inner, name: None }, Span::UNDEFINED)
    }
}

/// State for lowering a statement within a function.
pub struct StatementContext<'source, 'temp, 'out> {
    // WGSL AST values.
    /// A reference to [`TranslationUnit::expressions`] for the translation unit
    /// we're lowering.
    ///
    /// [`TranslationUnit::expressions`]: ast::TranslationUnit::expressions
    ast_expressions: &'temp Arena<ast::Expression<'source>>,

    /// A reference to [`TranslationUnit::types`] for the translation unit
    /// we're lowering.
    ///
    /// [`TranslationUnit::types`]: ast::TranslationUnit::types
    types: &'temp Arena<ast::Type<'source>>,

    // Naga IR values.
    /// The map from the names of module-scope declarations to the Naga IR
    /// `Handle`s we have built for them, owned by `Lowerer::lower`.
    globals: &'temp mut FastHashMap<&'source str, LoweredGlobalDecl>,

    /// A map from each `ast::Local` handle to the Naga expression
    /// we've built for it:
    ///
    /// - WGSL function arguments become Naga [`FunctionArgument`] expressions.
    ///
    /// - WGSL `var` declarations become Naga [`LocalVariable`] expressions.
    ///
    /// - WGSL `let` declararations become arbitrary Naga expressions.
    ///
    /// This always borrows the `local_table` local variable in
    /// [`Lowerer::function`].
    ///
    /// [`LocalVariable`]: crate::Expression::LocalVariable
    /// [`FunctionArgument`]: crate::Expression::FunctionArgument
    local_table: &'temp mut FastHashMap<Handle<ast::Local>, Typed<Handle<crate::Expression>>>,

    const_typifier: &'temp mut Typifier,
    typifier: &'temp mut Typifier,
    function: &'out mut crate::Function,
    /// Stores the names of expressions that are assigned in `let` statement
    /// Also stores the spans of the names, for use in errors.
    named_expressions: &'out mut FastIndexMap<Handle<crate::Expression>, (String, Span)>,
    module: &'out mut crate::Module,

    /// Which `Expression`s in `self.naga_expressions` are const expressions, in
    /// the WGSL sense.
    ///
    /// According to the WGSL spec, a const expression must not refer to any
    /// `let` declarations, even if those declarations' initializers are
    /// themselves const expressions. So this tracker is not simply concerned
    /// with the form of the expressions; it is also tracking whether WGSL says
    /// we should consider them to be const. See the use of `force_non_const` in
    /// the code for lowering `let` bindings.
    expression_constness: &'temp mut crate::proc::ExpressionConstnessTracker,
}

impl<'a, 'temp> StatementContext<'a, 'temp, '_> {
    fn as_expression<'t>(
        &'t mut self,
        block: &'t mut crate::Block,
        emitter: &'t mut Emitter,
    ) -> ExpressionContext<'a, 't, '_>
    where
        'temp: 't,
    {
        ExpressionContext {
            globals: self.globals,
            types: self.types,
            ast_expressions: self.ast_expressions,
            const_typifier: self.const_typifier,
            module: self.module,
            expr_type: ExpressionContextType::Runtime(RuntimeExpressionContext {
                local_table: self.local_table,
                function: self.function,
                block,
                emitter,
                typifier: self.typifier,
                expression_constness: self.expression_constness,
            }),
        }
    }

    fn as_global(&mut self) -> GlobalContext<'a, '_, '_> {
        GlobalContext {
            ast_expressions: self.ast_expressions,
            globals: self.globals,
            types: self.types,
            module: self.module,
            const_typifier: self.const_typifier,
        }
    }

    fn invalid_assignment_type(&self, expr: Handle<crate::Expression>) -> InvalidAssignmentType {
        if let Some(&(_, span)) = self.named_expressions.get(&expr) {
            InvalidAssignmentType::ImmutableBinding(span)
        } else {
            match self.function.expressions[expr] {
                crate::Expression::Swizzle { .. } => InvalidAssignmentType::Swizzle,
                crate::Expression::Access { base, .. } => self.invalid_assignment_type(base),
                crate::Expression::AccessIndex { base, .. } => self.invalid_assignment_type(base),
                _ => InvalidAssignmentType::Other,
            }
        }
    }
}

pub struct RuntimeExpressionContext<'temp, 'out> {
    /// A map from [`ast::Local`] handles to the Naga expressions we've built for them.
    ///
    /// This is always [`StatementContext::local_table`] for the
    /// enclosing statement; see that documentation for details.
    local_table: &'temp FastHashMap<Handle<ast::Local>, Typed<Handle<crate::Expression>>>,

    function: &'out mut crate::Function,
    block: &'temp mut crate::Block,
    emitter: &'temp mut Emitter,
    typifier: &'temp mut Typifier,

    /// Which `Expression`s in `self.naga_expressions` are const expressions, in
    /// the WGSL sense.
    ///
    /// See [`StatementContext::expression_constness`] for details.
    expression_constness: &'temp mut crate::proc::ExpressionConstnessTracker,
}

/// The type of Naga IR expression we are lowering an [`ast::Expression`] to.
pub enum ExpressionContextType<'temp, 'out> {
    /// We are lowering to an arbitrary runtime expression, to be
    /// included in a function's body.
    ///
    /// The given [`RuntimeExpressionContext`] holds information about local
    /// variables, arguments, and other definitions available only to runtime
    /// expressions, not constant or override expressions.
    Runtime(RuntimeExpressionContext<'temp, 'out>),

    /// We are lowering to a constant expression, to be included in the module's
    /// constant expression arena.
    ///
    /// Everything constant expressions are allowed to refer to is
    /// available in the [`ExpressionContext`], so this variant
    /// carries no further information.
    Constant,
}

/// State for lowering an [`ast::Expression`] to Naga IR.
///
/// [`ExpressionContext`]s come in two kinds, distinguished by
/// the value of the [`expr_type`] field:
///
/// - A [`Runtime`] context contributes [`naga::Expression`]s to a [`naga::Function`]'s
///   runtime expression arena.
///
/// - A [`Constant`] context contributes [`naga::Expression`]s to a [`naga::Module`]'s
///   constant expression arena.
///
/// [`ExpressionContext`]s are constructed in restricted ways:
///
/// - To get a [`Runtime`] [`ExpressionContext`], call
///   [`StatementContext::as_expression`].
///
/// - To get a [`Constant`] [`ExpressionContext`], call
///   [`GlobalContext::as_const`].
///
/// - You can demote a [`Runtime`] context to a [`Constant`] context
///   by calling [`as_const`], but there's no way to go in the other
///   direction, producing a runtime context from a constant one. This
///   is because runtime expressions can refer to constant
///   expressions, via [`Expression::Constant`], but constant
///   expressions can't refer to a function's expressions.
///
/// Not to be confused with `wgsl::parse::ExpressionContext`, which is
/// for parsing the `ast::Expression` in the first place.
///
/// [`expr_type`]: ExpressionContext::expr_type
/// [`Runtime`]: ExpressionContextType::Runtime
/// [`naga::Expression`]: crate::Expression
/// [`naga::Function`]: crate::Function
/// [`Constant`]: ExpressionContextType::Constant
/// [`naga::Module`]: crate::Module
/// [`as_const`]: ExpressionContext::as_const
/// [`Expression::Constant`]: crate::Expression::Constant
pub struct ExpressionContext<'source, 'temp, 'out> {
    // WGSL AST values.
    ast_expressions: &'temp Arena<ast::Expression<'source>>,
    types: &'temp Arena<ast::Type<'source>>,

    // Naga IR values.
    /// The map from the names of module-scope declarations to the Naga IR
    /// `Handle`s we have built for them, owned by `Lowerer::lower`.
    globals: &'temp mut FastHashMap<&'source str, LoweredGlobalDecl>,

    /// The IR [`Module`] we're constructing.
    ///
    /// [`Module`]: crate::Module
    module: &'out mut crate::Module,

    /// Type judgments for [`module::const_expressions`].
    ///
    /// [`module::const_expressions`]: crate::Module::const_expressions
    const_typifier: &'temp mut Typifier,

    /// Whether we are lowering a constant expression or a general
    /// runtime expression, and the data needed in each case.
    expr_type: ExpressionContextType<'temp, 'out>,
}

impl<'source, 'temp, 'out> ExpressionContext<'source, 'temp, 'out> {
    fn as_const(&mut self) -> ExpressionContext<'source, '_, '_> {
        ExpressionContext {
            globals: self.globals,
            types: self.types,
            ast_expressions: self.ast_expressions,
            const_typifier: self.const_typifier,
            module: self.module,
            expr_type: ExpressionContextType::Constant,
        }
    }

    fn as_global(&mut self) -> GlobalContext<'source, '_, '_> {
        GlobalContext {
            ast_expressions: self.ast_expressions,
            globals: self.globals,
            types: self.types,
            module: self.module,
            const_typifier: self.const_typifier,
        }
    }

    fn as_globalctx(&self) -> crate::proc::GlobalCtx<'_> {
        crate::proc::GlobalCtx {
            types: &self.module.types,
            constants: &self.module.constants,
            const_expressions: &self.module.const_expressions,
        }
    }

    fn as_const_evaluator(&mut self) -> ConstantEvaluator {
        match self.expr_type {
            ExpressionContextType::Runtime(ref mut rctx) => ConstantEvaluator::for_wgsl_function(
                self.module,
                &mut rctx.function.expressions,
                rctx.expression_constness,
                rctx.emitter,
                rctx.block,
            ),
            ExpressionContextType::Constant => ConstantEvaluator::for_wgsl_module(self.module),
        }
    }

    fn append_expression(
        &mut self,
        expr: crate::Expression,
        span: Span,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let mut eval = self.as_const_evaluator();
        match eval.try_eval_and_append(&expr, span) {
            Ok(evaluated_expr) => Ok(evaluated_expr),

            // `expr` is not a constant expression. This is fine as
            // long as we're not building `Module::const_expressions`.
            Err(err) => match self.expr_type {
                ExpressionContextType::Runtime(ref mut rctx) => {
                    Ok(rctx.function.expressions.append(expr, span))
                }
                ExpressionContextType::Constant => Err(Error::ConstantEvaluatorError(err, span)),
            },
        }
    }

    fn is_const(&self, expr: Handle<crate::Expression>) -> bool {
        match self.expr_type {
            ExpressionContextType::Runtime(rctx) => rctx.expression_constness.is_const(expr),
            ExpressionContextType::Constant => true,
        }
    }

    fn const_access(&self, handle: Handle<crate::Expression>) -> Option<u32> {
        match self.expr_type {
            ExpressionContextType::Runtime(ref ctx) => {
                if !ctx.expression_constness.is_const(handle) {
                    return None;
                }

                self.module
                    .to_ctx()
                    .eval_expr_to_u32_from(handle, &ctx.function.expressions)
                    .ok()
            }
            ExpressionContextType::Constant => self.module.to_ctx().eval_expr_to_u32(handle).ok(),
        }
    }

    fn get_expression_span(&self, handle: Handle<crate::Expression>) -> Span {
        match self.expr_type {
            ExpressionContextType::Runtime(ref ctx) => ctx.function.expressions.get_span(handle),
            ExpressionContextType::Constant => self.module.const_expressions.get_span(handle),
        }
    }

    fn typifier(&self) -> &Typifier {
        match self.expr_type {
            ExpressionContextType::Runtime(ref ctx) => ctx.typifier,
            ExpressionContextType::Constant => self.const_typifier,
        }
    }

    fn runtime_expression_ctx(
        &mut self,
        span: Span,
    ) -> Result<&mut RuntimeExpressionContext<'temp, 'out>, Error<'source>> {
        match self.expr_type {
            ExpressionContextType::Runtime(ref mut ctx) => Ok(ctx),
            ExpressionContextType::Constant => Err(Error::UnexpectedOperationInConstContext(span)),
        }
    }

    fn gather_component(
        &mut self,
        expr: Handle<crate::Expression>,
        component_span: Span,
        gather_span: Span,
    ) -> Result<crate::SwizzleComponent, Error<'source>> {
        match self.expr_type {
            ExpressionContextType::Runtime(ref rctx) => {
                if !rctx.expression_constness.is_const(expr) {
                    return Err(Error::ExpectedConstExprConcreteIntegerScalar(
                        component_span,
                    ));
                }

                let index = self
                    .module
                    .to_ctx()
                    .eval_expr_to_u32_from(expr, &rctx.function.expressions)
                    .map_err(|err| match err {
                        crate::proc::U32EvalError::NonConst => {
                            Error::ExpectedConstExprConcreteIntegerScalar(component_span)
                        }
                        crate::proc::U32EvalError::Negative => {
                            Error::ExpectedNonNegative(component_span)
                        }
                    })?;
                crate::SwizzleComponent::XYZW
                    .get(index as usize)
                    .copied()
                    .ok_or(Error::InvalidGatherComponent(component_span))
            }
            // This means a `gather` operation appeared in a constant expression.
            // This error refers to the `gather` itself, not its "component" argument.
            ExpressionContextType::Constant => {
                Err(Error::UnexpectedOperationInConstContext(gather_span))
            }
        }
    }

    /// Determine the type of `expr`, and add it to the module's arena.
    ///
    /// If you just need a `TypeInner` for `expr`'s type, use the
    /// [`resolve_inner!`] macro instead. This function should only be
    /// used when the type of `expr` needs to appear in the module's
    /// final `Arena<Type>` --- for example, if you're creating a
    /// [`LocalVariable`] whose type is inferred from its initializer.
    ///
    /// [`LocalVariable`]: crate::LocalVariable
    fn register_type(
        &mut self,
        expr: Handle<crate::Expression>,
    ) -> Result<Handle<crate::Type>, Error<'source>> {
        // Ensure that we have a `TypeResolution` for `expr`.
        self.grow_types(expr)?;

        // This is equivalent to calling ExpressionContext::typifier(),
        // except that this lets the borrow checker see that it's okay
        // to also borrow self.module.types mutably below.
        let typifier = match self.expr_type {
            ExpressionContextType::Runtime(ref ctx) => ctx.typifier,
            ExpressionContextType::Constant => &*self.const_typifier,
        };

        // Promote the `TypeResolution` to a full `Type` in the arena.
        Ok(typifier.register_type(expr, &mut self.module.types))
    }

    /// Resolve the types of all expressions up through `handle`.
    ///
    /// Ensure that [`self.typifier`] has a [`TypeResolution`] for
    /// every expression in [`self.function.expressions`].
    ///
    /// This does not add types to any arena. The [`Typifier`]
    /// documentation explains the steps we take to avoid filling
    /// arenas with intermediate types.
    ///
    /// This function takes `&mut self`, so it can't conveniently
    /// return a shared reference to the resulting `TypeResolution`:
    /// the shared reference would extend the mutable borrow, and you
    /// wouldn't be able to use `self` for anything else. Instead, you
    /// should use [`register_type`] or one of [`resolve!`],
    /// [`resolve_inner!`] or [`resolve_inner_binary!`].
    ///
    /// [`self.typifier`]: ExpressionContext::typifier
    /// [`register_type`]: Self::register_type
    /// [`Typifier`]: Typifier
    fn grow_types(
        &mut self,
        handle: Handle<crate::Expression>,
    ) -> Result<&mut Self, Error<'source>> {
        let empty_arena = Arena::new();
        let resolve_ctx;
        let typifier;
        let expressions;
        match self.expr_type {
            ExpressionContextType::Runtime(ref mut ctx) => {
                resolve_ctx = ResolveContext::with_locals(
                    self.module,
                    &ctx.function.local_variables,
                    &ctx.function.arguments,
                );
                typifier = &mut *ctx.typifier;
                expressions = &ctx.function.expressions;
            }
            ExpressionContextType::Constant => {
                resolve_ctx = ResolveContext::with_locals(self.module, &empty_arena, &[]);
                typifier = self.const_typifier;
                expressions = &self.module.const_expressions;
            }
        };
        typifier
            .grow(handle, expressions, &resolve_ctx)
            .map_err(Error::InvalidResolve)?;

        Ok(self)
    }

    fn image_data(
        &mut self,
        image: Handle<crate::Expression>,
        span: Span,
    ) -> Result<(crate::ImageClass, bool), Error<'source>> {
        match *resolve_inner!(self, image) {
            crate::TypeInner::Image { class, arrayed, .. } => Ok((class, arrayed)),
            _ => Err(Error::BadTexture(span)),
        }
    }

    fn prepare_args<'b>(
        &mut self,
        args: &'b [Handle<ast::Expression<'source>>],
        min_args: u32,
        span: Span,
    ) -> ArgumentContext<'b, 'source> {
        ArgumentContext {
            args: args.iter(),
            min_args,
            args_used: 0,
            total_args: args.len() as u32,
            span,
        }
    }

    /// Insert splats, if needed by the non-'*' operations.
    ///
    /// See the "Binary arithmetic expressions with mixed scalar and vector operands"
    /// table in the WebGPU Shading Language specification for relevant operators.
    ///
    /// Multiply is not handled here as backends are expected to handle vec*scalar
    /// operations, so inserting splats into the IR increases size needlessly.
    fn binary_op_splat(
        &mut self,
        op: crate::BinaryOperator,
        left: &mut Loaded<Handle<crate::Expression>>,
        right: &mut Loaded<Handle<crate::Expression>>,
    ) -> Result<(), Error<'source>> {
        if matches!(
            op,
            crate::BinaryOperator::Add
                | crate::BinaryOperator::Subtract
                | crate::BinaryOperator::Divide
                | crate::BinaryOperator::Modulo
        ) {
            todo!()
            /*
                match resolve_inner_binary!(self, *left, *right) {
                    (&crate::TypeInner::Vector { size, .. }, &crate::TypeInner::Scalar { .. }) => {
                        *right = self.append_expression(
                            crate::Expression::Splat {
                                size,
                                value: *right,
                            },
                            self.get_expression_span(*right),
                        )?;
                    }
                    (&crate::TypeInner::Scalar { .. }, &crate::TypeInner::Vector { size, .. }) => {
                        *left = self.append_expression(
                            crate::Expression::Splat { size, value: *left },
                            self.get_expression_span(*left),
                        )?;
                    }
                    _ => {}
            }
                */
        }

        Ok(())
    }

    /// Add a single expression to the expression table that is not covered by `self.emitter`.
    ///
    /// This is useful for `CallResult` and `AtomicResult` expressions, which should not be covered by
    /// `Emit` statements.
    fn interrupt_emitter(
        &mut self,
        expression: crate::Expression,
        span: Span,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        match self.expr_type {
            ExpressionContextType::Runtime(ref mut rctx) => {
                rctx.block
                    .extend(rctx.emitter.finish(&rctx.function.expressions));
            }
            ExpressionContextType::Constant => {}
        }
        let result = self.append_expression(expression, span);
        match self.expr_type {
            ExpressionContextType::Runtime(ref mut rctx) => {
                rctx.emitter.start(&rctx.function.expressions);
            }
            ExpressionContextType::Constant => {}
        }
        result
    }

    /// Apply the WGSL Load Rule to `expr`.
    ///
    /// If `expr` is has type `ref<SC, T, A>`, perform a load to produce a value of type
    /// `T`. Otherwise, return `expr` unchanged.
    fn apply_load_rule(
        &mut self,
        expr: Typed<Handle<crate::Expression>>,
    ) -> Result<Loaded<Handle<crate::Expression>>, Error<'source>> {
        match expr {
            Typed::Reference(pointer) => {
                let span = self.get_expression_span(pointer);
                let load = crate::Expression::Load { pointer };
                // Since the value was stored, it can't be abstract.
                Ok(Loaded::Concrete(self.append_expression(load, span)?))
            }
            Typed::Loaded(plain) => Ok(plain),
        }
    }

    /// Try to convert `expr` to `target` via WGSL's automatic conversions.
    ///
    /// If no conversions are necessary, return `Ok(Some(expr))`.
    ///
    /// If the automatic conversions cannot convert `expr` to
    /// `target`, return `Ok(None)`. (Only the caller has enough
    /// information to produce an appropriately detailed error.)
    ///
    /// Note that, even though the WGSL spec includes the Load Rule in
    /// the set of automatic conversions, this function assumes the
    /// Load Rule has already been applied (as indicated by the type
    /// of `expr`).
    fn apply_automatic_conversions(
        &mut self,
        expr: Loaded<Handle<crate::Expression>>,
        target: Loaded<TypeResolution>,
    ) -> Result<Option<Loaded<Handle<crate::Expression>>>, Error<'source>> {
        let types = &self.module.types;
        let expr_inner = expr.try_map(|handle| Ok(resolve_inner!(self, handle)))?;
        let target_inner = target.map(|res| res.inner_with(&self.module.types));
        match convert::convert_loaded(expr_inner, target_inner, self.module) {
            convert::Conversion::Trivial => return Ok(Some(expr)),
            convert::Conversion::ScalarCast(Scalar { kind, width }) => {
                let cast = self.cast_scalar_components(expr.handle(), kind, width)?;
                // The resulting expression has the abstractness of `target`.
                Ok(Some(target_inner.map(|_| cast)))
            }
            convert::Conversion::None => Ok(None),
        }
    }

    /// Compute the [concretization] of `expr`.
    ///
    /// The concretization of `expr` is the best-ranked conversion of `expr` to
    /// some concrete type.
    ///
    /// [concretization]: https://gpuweb.github.io/gpuweb/wgsl/#concretization
    fn concretize(
        &mut self,
        expr: Loaded<Handle<crate::Expression>>,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        match expr {
            Loaded::Concrete(concrete) => Ok(concrete),
            Loaded::Abstract(r#abstract) => {
                if !self.is_const(r#abstract) {
                    return Err(Error::Internal("abstract values must always be const"));
                }
                todo!()
            }
        }
    }

    /// Convert the constant `expr`'s scalar components to the given kind and width.
    ///
    /// This function is meant to be used to implement WGSL automatic
    /// conversions. Those conversion rules never affect concrete values, only
    /// abstract values. Since only constant expressions can be abstract values,
    /// we assume `expr` must be a constant expression, and use the constant
    /// evaluator to do the conversion.
    ///
    /// If `expr` is a vector, matrix, or array, the conversion applies to its
    /// leaves.
    fn cast_scalar_components(
        &mut self,
        expr: Handle<crate::Expression>,
        kind: crate::ScalarKind,
        width: crate::Bytes,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let span = self.get_expression_span(expr);
        let converted = self
            .as_const_evaluator()
            .cast(expr, kind, width, span)
            .map_err(|err| Error::ConstantEvaluatorError(err, span))?;
        Ok(converted)
    }

    fn format_typeinner(&self, inner: &crate::TypeInner) -> String {
        inner.to_wgsl(&self.module.to_ctx())
    }

    fn format_type(&self, handle: Handle<crate::Type>) -> String {
        let ty = &self.module.types[handle];
        match ty.name {
            Some(ref name) => name.clone(),
            None => self.format_typeinner(&ty.inner),
        }
    }

    fn format_type_resolution(&self, resolution: &TypeResolution) -> String {
        match *resolution {
            TypeResolution::Handle(handle) => self.format_type(handle),
            TypeResolution::Value(ref inner) => self.format_typeinner(inner),
        }
    }

    fn ensure_type_exists(&mut self, inner: crate::TypeInner) -> Handle<crate::Type> {
        self.as_global().ensure_type_exists(inner)
    }
}

struct ArgumentContext<'ctx, 'source> {
    args: std::slice::Iter<'ctx, Handle<ast::Expression<'source>>>,
    min_args: u32,
    args_used: u32,
    total_args: u32,
    span: Span,
}

impl<'source> ArgumentContext<'_, 'source> {
    pub fn finish(self) -> Result<(), Error<'source>> {
        if self.args.len() == 0 {
            Ok(())
        } else {
            Err(Error::WrongArgumentCount {
                found: self.total_args,
                expected: self.min_args..self.args_used + 1,
                span: self.span,
            })
        }
    }

    pub fn next(&mut self) -> Result<Handle<ast::Expression<'source>>, Error<'source>> {
        match self.args.next().copied() {
            Some(arg) => {
                self.args_used += 1;
                Ok(arg)
            }
            None => Err(Error::WrongArgumentCount {
                found: self.total_args,
                expected: self.min_args..self.args_used + 1,
                span: self.span,
            }),
        }
    }
}

/// WGSL reference/pointer distinction.
///
/// Naga and WGSL types are very close, but Naga lacks WGSL's `ref` types, which
/// we need to know to apply the Load Rule. This enum carries some WGSL or Naga
/// datum along with enough information to determine its corresponding WGSL
/// type.
///
/// The `T` type parameter can be any expression-like thing:
///
/// - `Typed<Handle<crate::Type>>` can represent a full WGSL type. For example,
///   given some Naga `Pointer` type `ptr`, a WGSL reference type is a
///   `Typed::Reference(ptr)` whereas a WGSL pointer type is a
///   `Typed::Plain(ptr)`.
///
/// - `Typed<crate::Expression>` or `Typed<Handle<crate::Expression>>` can
///   represent references similarly.
///
/// Use the `map` and `try_map` methods to convert from one expression
/// representation to another.
///
/// [`Expression`]: crate::Expression
#[derive(Debug, Copy, Clone)]
enum Typed<T> {
    /// A WGSL reference.
    Reference(T),

    /// A WGSL plain type.
    Loaded(Loaded<T>),
}

impl<T> Typed<T> {
    fn map<U>(self, mut f: impl FnMut(T) -> U) -> Typed<U> {
        match self {
            Self::Reference(v) => Typed::Reference(f(v)),
            Self::Loaded(v) => Typed::Loaded(v.map(f)),
        }
    }

    fn try_map<U, E>(self, mut f: impl FnMut(T) -> Result<U, E>) -> Result<Typed<U>, E> {
        Ok(match self {
            Self::Reference(v) => Typed::Reference(f(v)?),
            Self::Loaded(v) => Typed::Loaded(v.try_map(f)?),
        })
    }
}

/// WGSL concrete/abstract distinction
///
/// Naga and WGSL types are very close, but Naga lacks WGSL's
/// `AbstractInt` and `AbstractFloat` types, which we need to know to
/// apply automatic conversions. This enum carries some WGSL or Naga
/// datum along with enough information to determine its corresponding
/// WGSL type.
///
/// Unlike `Typed`, this enum excludes WGSL reference types. If you
/// have a `Loaded`, you know it is either a plain type or a WGSL
/// pointer.
///
/// The `T` type parameter can be any expression-like thing:
///
/// - `Loaded<Handle<crate::Type>>` can represent a WGSL concrete or
///   abstract type. For example, given some Naga `Scalar` type
///   `scalar`, a WGSL abstract type is a `Loaded::Abstract(scalar)`
///   whereas a WGSL concrete scalar type is a
///   `Loaded::Concrete(scalar)`.
///
/// - `Typed<crate::Expression>` or `Typed<Handle<crate::Expression>>` can
///   represent abstract and concrete values similarly.
///
/// Use the `map` and `try_map` methods to convert from one expression
/// representation to another.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Loaded<T> {
    Concrete(T),
    Abstract(T),
}

impl<T> Loaded<T> {
    fn map<U>(self, mut f: impl FnMut(T) -> U) -> Loaded<U> {
        match self {
            Self::Concrete(v) => Loaded::Concrete(f(v)),
            Self::Abstract(v) => Loaded::Abstract(f(v)),
        }
    }

    fn try_map<U, E>(self, mut f: impl FnMut(T) -> Result<U, E>) -> Result<Loaded<U>, E> {
        Ok(match self {
            Self::Concrete(expr) => Loaded::Concrete(f(expr)?),
            Self::Abstract(expr) => Loaded::Abstract(f(expr)?),
        })
    }
}

// We would like to delete all uses of these functions before bringing
// the PR out of draft. But in the mean time, using these allows the
// code to compile again.
impl<T> Loaded<T> {
    fn todo(self) -> T {
        match self {
            Self::Concrete(handle) => handle,
            Self::Abstract(handle) => handle,
        }
    }

    fn new_todo(value: T) -> Self {
        Self::Concrete(value)
    }

    fn container_todo<I, O>(wrapped: I) -> O
    where I: IntoIterator<Item=Self>,
          O: FromIterator<T>,
    {
        wrapped
            .into_iter()
            .map(Self::todo)
            .collect()
    }
}

impl Loaded<Handle<crate::Expression>> {
    fn handle(self) -> Handle<crate::Expression> {
        match self {
            Loaded::Concrete(handle) => handle,
            Loaded::Abstract(handle) => handle,
        }
    }
}

impl Loaded<&crate::TypeInner> {
    fn to_wgsl(&self, gctx: &crate::proc::GlobalCtx) -> String {
        use crate::ScalarKind as Sk;
        use crate::TypeInner as Ti;

        fn abstract_scalar(kind: Sk, width: crate::Bytes) -> &'static str {
            match (kind, width) {
                (Sk::Sint, 8) => "AbstractInt",
                (Sk::Float, 8) => "AbstractFloat",
                // The above ought to cover all abstract types.
                _ => unreachable!(),
            }
        }

        match *self {
            Loaded::Concrete(inner) => inner.to_wgsl(gctx),
            Loaded::Abstract(inner) => {
                match *inner {
                    Ti::Scalar { kind, width } => abstract_scalar(kind, width).to_string(),
                    Ti::Vector { size, kind, width } => {
                        format!("vec{}<{}>", size as u8, abstract_scalar(kind, width))
                    }
                    Ti::Matrix {
                        columns,
                        rows,
                        width,
                    } => {
                        format!(
                            "mat{}x{}<{}>",
                            columns as u8,
                            rows as u8,
                            abstract_scalar(Sk::Float, width)
                        )
                    }
                    Ti::Array { base, size, stride } => {
                        let base_wgsl = Loaded::Abstract(base).to_wgsl(gctx);
                        return match size {
                            crate::ArraySize::Constant(size) => {
                                format!("array<{}, {}>", base_wgsl, size)
                            }
                            crate::ArraySize::Dynamic => {
                                format!("array<{}>", base_wgsl)
                            }
                        };
                    }
                    // The above ought to cover all abstract types.
                    _ => unreachable!(),
                }
            }
        }
    }
}

impl Loaded<Handle<crate::Type>> {
    fn to_wgsl(&self, gctx: &crate::proc::GlobalCtx) -> String {
        self.map(|handle| &gctx.types[handle].inner).to_wgsl(gctx)
    }
}

impl Loaded<&'_ TypeResolution> {
    fn to_wgsl(&self, gctx: &crate::proc::GlobalCtx) -> String {
        self.map(|resolution| resolution.inner_with(gctx.types))
            .to_wgsl(gctx)
    }
}
/// A single vector component or swizzle.
///
/// This represents the things that can appear after the `.` in a vector access
/// expression: either a single component name, or a series of them,
/// representing a swizzle.
enum Components {
    Single(u32),
    Swizzle {
        size: crate::VectorSize,
        pattern: [crate::SwizzleComponent; 4],
    },
}

impl Components {
    const fn letter_component(letter: char) -> Option<crate::SwizzleComponent> {
        use crate::SwizzleComponent as Sc;
        match letter {
            'x' | 'r' => Some(Sc::X),
            'y' | 'g' => Some(Sc::Y),
            'z' | 'b' => Some(Sc::Z),
            'w' | 'a' => Some(Sc::W),
            _ => None,
        }
    }

    fn single_component(name: &str, name_span: Span) -> Result<u32, Error> {
        let ch = name.chars().next().ok_or(Error::BadAccessor(name_span))?;
        match Self::letter_component(ch) {
            Some(sc) => Ok(sc as u32),
            None => Err(Error::BadAccessor(name_span)),
        }
    }

    /// Construct a `Components` value from a 'member' name, like `"wzy"` or `"x"`.
    ///
    /// Use `name_span` for reporting errors in parsing the component string.
    fn new(name: &str, name_span: Span) -> Result<Self, Error> {
        let size = match name.len() {
            1 => return Ok(Components::Single(Self::single_component(name, name_span)?)),
            2 => crate::VectorSize::Bi,
            3 => crate::VectorSize::Tri,
            4 => crate::VectorSize::Quad,
            _ => return Err(Error::BadAccessor(name_span)),
        };

        let mut pattern = [crate::SwizzleComponent::X; 4];
        for (comp, ch) in pattern.iter_mut().zip(name.chars()) {
            *comp = Self::letter_component(ch).ok_or(Error::BadAccessor(name_span))?;
        }

        Ok(Components::Swizzle { size, pattern })
    }
}

/// An `ast::GlobalDecl` for which we have built the Naga IR equivalent.
enum LoweredGlobalDecl {
    Function(Handle<crate::Function>),
    Var(Handle<crate::GlobalVariable>),
    Const(Handle<crate::Constant>),
    Type(Handle<crate::Type>),
    EntryPoint,
}

enum Texture {
    Gather,
    GatherCompare,

    Sample,
    SampleBias,
    SampleCompare,
    SampleCompareLevel,
    SampleGrad,
    SampleLevel,
    // SampleBaseClampToEdge,
}

impl Texture {
    pub fn map(word: &str) -> Option<Self> {
        Some(match word {
            "textureGather" => Self::Gather,
            "textureGatherCompare" => Self::GatherCompare,

            "textureSample" => Self::Sample,
            "textureSampleBias" => Self::SampleBias,
            "textureSampleCompare" => Self::SampleCompare,
            "textureSampleCompareLevel" => Self::SampleCompareLevel,
            "textureSampleGrad" => Self::SampleGrad,
            "textureSampleLevel" => Self::SampleLevel,
            // "textureSampleBaseClampToEdge" => Some(Self::SampleBaseClampToEdge),
            _ => return None,
        })
    }

    pub const fn min_argument_count(&self) -> u32 {
        match *self {
            Self::Gather => 3,
            Self::GatherCompare => 4,

            Self::Sample => 3,
            Self::SampleBias => 5,
            Self::SampleCompare => 5,
            Self::SampleCompareLevel => 5,
            Self::SampleGrad => 6,
            Self::SampleLevel => 5,
            // Self::SampleBaseClampToEdge => 3,
        }
    }
}

pub struct Lowerer<'source, 'temp> {
    index: &'temp Index<'source>,
    layouter: Layouter,
}

impl<'source, 'temp> Lowerer<'source, 'temp> {
    pub fn new(index: &'temp Index<'source>) -> Self {
        Self {
            index,
            layouter: Layouter::default(),
        }
    }

    pub fn lower(
        &mut self,
        tu: &'temp ast::TranslationUnit<'source>,
    ) -> Result<crate::Module, Error<'source>> {
        let mut module = crate::Module::default();

        let mut ctx = GlobalContext {
            ast_expressions: &tu.expressions,
            globals: &mut FastHashMap::default(),
            types: &tu.types,
            module: &mut module,
            const_typifier: &mut Typifier::new(),
        };

        for decl_handle in self.index.visit_ordered() {
            let span = tu.decls.get_span(decl_handle);
            let decl = &tu.decls[decl_handle];

            match decl.kind {
                ast::GlobalDeclKind::Fn(ref f) => {
                    let lowered_decl = self.function(f, span, &mut ctx)?;
                    ctx.globals.insert(f.name.name, lowered_decl);
                }
                ast::GlobalDeclKind::Var(ref v) => {
                    let mut ectx = ctx.as_const();
                    let explicit_ty = self.resolve_ast_type(v.ty, &mut ctx)?;

                    let init_handle = v
                        .init
                        .map(|init| {
                            let mut init = self.expression(init, &mut ectx)?;

                            // Can the initializer be converted to the explicit type?
                            let explicit_res = Loaded::Concrete(TypeResolution::Handle(explicit_ty));
                            match ectx.apply_automatic_conversions(init, explicit_res)? {
                                Some(converted) => {
                                    // Since the explicit type is always concrete, we know
                                    // the initializer's type is now concrete.
                                    Ok(converted.handle())
                                }
                                None => {
                                    let init_ty = init.try_map(|handle| Ok(resolve_inner!(ectx, handle)))?;
                                    let ctx = ectx.as_globalctx();
                                    Err(Error::InitializationTypeMismatch {
                                        name: v.name.span,
                                        expected: explicit_ty.to_wgsl(&ctx),
                                        got: init_ty.to_wgsl(&ctx),
                                    })
                                }
                            }
                        })
                        .transpose()?;

                    let binding = if let Some(ref binding) = v.binding {
                        Some(crate::ResourceBinding {
                            group: self.const_u32(binding.group, &mut ctx.as_const())?.0,
                            binding: self.const_u32(binding.binding, &mut ctx.as_const())?.0,
                        })
                    } else {
                        None
                    };

                    let handle = ctx.module.global_variables.append(
                        crate::GlobalVariable {
                            name: Some(v.name.name.to_string()),
                            space: v.space,
                            binding,
                            ty: explicit_ty,
                            init: init_handle,
                        },
                        span,
                    );

                    ctx.globals
                        .insert(v.name.name, LoweredGlobalDecl::Var(handle));
                }
                ast::GlobalDeclKind::Const(ref c) => {
                    let mut ectx = ctx.as_const();
                    let mut init = self.expression(c.init, &mut ectx)?;

                    // If the declaration has an explicit type, try converting.
                    // Otherwise, use `init`'s type, which may be abstract.
                    if let Some(explicit_ty) = c.ty {
                        let explicit_ty =
                            self.resolve_ast_type(explicit_ty, &mut ectx.as_global())?;

                        // Can the initializer be converted to the explicit type?
                        let explicit_res = Loaded::Concrete(TypeResolution::Handle(explicit_ty));
                        match ectx.apply_automatic_conversions(init, explicit_res)? {
                            Some(converted) => {
                                init = converted;
                            }
                            None => {
                                let init_ty = init.try_map(|handle| Ok(resolve!(ectx, handle)))?;
                                let ctx = ectx.as_globalctx();
                                return Err(Error::InitializationTypeMismatch {
                                    name: c.name.span,
                                    expected: explicit_ty.to_wgsl(&ctx),
                                    got: init_ty.to_wgsl(&ctx),
                                })
                            }
                        }
                    }

                    match init {
                        // If the constant has a concrete type, we can put it in
                        // the module's arena.
                        Loaded::Concrete(init) => {
                            let ty = ectx.register_type(init)?;
                            let handle = ctx.module.constants.append(
                                crate::Constant {
                                    name: Some(c.name.name.to_string()),
                                    r#override: crate::Override::None,
                                    ty,
                                    init,
                                },
                                span,
                            );

                            ctx.globals
                                .insert(c.name.name, LoweredGlobalDecl::Const(handle));
                        }
                        // We can't put abstract-typed constants in the module
                        // arena, because the IR has no representation of
                        // abstractness, and the validator will reject the
                        // 64-bit types.
                        //
                        // Plan: add LoweredGlobalDecl::AbstractConst.
                        // Abstract-typed global constants would just be omitted
                        // from the IR.
                        Loaded::Abstract(_) => {
                            todo!();
                        }
                    }
                }
                ast::GlobalDeclKind::Struct(ref s) => {
                    let handle = self.r#struct(s, span, &mut ctx)?;
                    ctx.globals
                        .insert(s.name.name, LoweredGlobalDecl::Type(handle));
                }
                ast::GlobalDeclKind::Type(ref alias) => {
                    let ty = self.resolve_ast_type(alias.ty, &mut ctx)?;
                    ctx.globals
                        .insert(alias.name.name, LoweredGlobalDecl::Type(ty));
                }
            }
        }

        Ok(module)
    }

    fn function(
        &mut self,
        f: &ast::Function<'source>,
        span: Span,
        ctx: &mut GlobalContext<'source, '_, '_>,
    ) -> Result<LoweredGlobalDecl, Error<'source>> {
        let mut local_table = FastHashMap::default();
        let mut expressions = Arena::new();
        let mut named_expressions = FastIndexMap::default();

        let arguments = f
            .arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let ty = self.resolve_ast_type(arg.ty, ctx)?;
                let expr = expressions
                    .append(crate::Expression::FunctionArgument(i as u32), arg.name.span);
                local_table.insert(arg.handle, Typed::Loaded(Loaded::Concrete(expr)));
                named_expressions.insert(expr, (arg.name.name.to_string(), arg.name.span));

                Ok(crate::FunctionArgument {
                    name: Some(arg.name.name.to_string()),
                    ty,
                    binding: self.binding(&arg.binding, ty, ctx)?,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let result = f
            .result
            .as_ref()
            .map(|res| {
                let ty = self.resolve_ast_type(res.ty, ctx)?;
                Ok(crate::FunctionResult {
                    ty,
                    binding: self.binding(&res.binding, ty, ctx)?,
                })
            })
            .transpose()?;

        let mut function = crate::Function {
            name: Some(f.name.name.to_string()),
            arguments,
            result,
            local_variables: Arena::new(),
            expressions,
            named_expressions: crate::NamedExpressions::default(),
            body: crate::Block::default(),
        };

        let mut typifier = Typifier::default();
        let mut stmt_ctx = StatementContext {
            local_table: &mut local_table,
            globals: ctx.globals,
            ast_expressions: ctx.ast_expressions,
            const_typifier: ctx.const_typifier,
            typifier: &mut typifier,
            function: &mut function,
            named_expressions: &mut named_expressions,
            types: ctx.types,
            module: ctx.module,
            expression_constness: &mut crate::proc::ExpressionConstnessTracker::new(),
        };
        let mut body = self.block(&f.body, false, &mut stmt_ctx)?;
        ensure_block_returns(&mut body);

        function.body = body;
        function.named_expressions = named_expressions
            .into_iter()
            .map(|(key, (name, _))| (key, name))
            .collect();

        if let Some(ref entry) = f.entry_point {
            let workgroup_size = if let Some(workgroup_size) = entry.workgroup_size {
                // TODO: replace with try_map once stabilized
                let mut workgroup_size_out = [1; 3];
                for (i, size) in workgroup_size.into_iter().enumerate() {
                    if let Some(size_expr) = size {
                        workgroup_size_out[i] = self.const_u32(size_expr, &mut ctx.as_const())?.0;
                    }
                }
                workgroup_size_out
            } else {
                [0; 3]
            };

            ctx.module.entry_points.push(crate::EntryPoint {
                name: f.name.name.to_string(),
                stage: entry.stage,
                early_depth_test: entry.early_depth_test,
                workgroup_size,
                function,
            });
            Ok(LoweredGlobalDecl::EntryPoint)
        } else {
            let handle = ctx.module.functions.append(function, span);
            Ok(LoweredGlobalDecl::Function(handle))
        }
    }

    fn block(
        &mut self,
        b: &ast::Block<'source>,
        is_inside_loop: bool,
        ctx: &mut StatementContext<'source, '_, '_>,
    ) -> Result<crate::Block, Error<'source>> {
        let mut block = crate::Block::default();

        for stmt in b.stmts.iter() {
            self.statement(stmt, &mut block, is_inside_loop, ctx)?;
        }

        Ok(block)
    }

    fn statement(
        &mut self,
        stmt: &ast::Statement<'source>,
        block: &mut crate::Block,
        is_inside_loop: bool,
        ctx: &mut StatementContext<'source, '_, '_>,
    ) -> Result<(), Error<'source>> {
        let out = match stmt.kind {
            ast::StatementKind::Block(ref block) => {
                let block = self.block(block, is_inside_loop, ctx)?;
                crate::Statement::Block(block)
            }
            ast::StatementKind::LocalDecl(ref decl) => match *decl {
                ast::LocalDecl::Let(ref l) => {
                    let mut emitter = Emitter::default();
                    emitter.start(&ctx.function.expressions);

                    let mut ectx = ctx.as_expression(block, &mut emitter);

                    // `let` bindings must be constructible types or pointers,
                    // so references are forbidden: we must apply the Load Rule.
                    let init = self.expression(l.init, &mut ectx)?;

                    // If the declaration has an explicit type, try converting,
                    // adjusting `init` and `ty` accordingly.
                    let init = if let Some(explicit_ty) = l.ty {
                        let explicit_ty =
                            self.resolve_ast_type(explicit_ty, &mut ctx.as_global())?;

                        // Can the initializer be converted to the explicit type?
                        let explicit_res = Loaded::Concrete(TypeResolution::Handle(explicit_ty));
                        match ectx.apply_automatic_conversions(init, explicit_res)? {
                            Some(converted) => {
                                converted
                            }
                            None => {
                                let init_ty = init.try_map(|handle| Ok(resolve!(ectx, handle)))?;
                                let ctx = ectx.as_globalctx();
                                return Err(Error::InitializationTypeMismatch {
                                    name: l.name.span,
                                    expected: explicit_ty.to_wgsl(&ctx),
                                    got: init_ty.to_wgsl(&ctx),
                                });
                            }
                        }
                    } else {
                        // The value of a `let` binding is the concretized value
                        // of the initializer.
                        Loaded::Concrete(ectx.concretize(init)?)
                    };

                    let init_handle = init.handle();
                    ctx.named_expressions
                        .insert(init_handle, (l.name.name.to_string(), l.name.span));
                    ctx.local_table.insert(l.handle, Typed::Loaded(init));
                    block.extend(emitter.finish(&ctx.function.expressions));

                    // The WGSL spec says that any expression that refers to a
                    // `let`-bound variable is not a const expression. This
                    // affects when errors must be reported, so we can't even
                    // treat suitable `let` bindings as constant as an
                    // optimization.
                    ctx.expression_constness.force_non_const(init_handle);

                    return Ok(());
                }
                ast::LocalDecl::Var(ref v) => {
                    let mut emitter = Emitter::default();
                    emitter.start(&ctx.function.expressions);

                    let mut ectx = ctx.as_expression(block, &mut emitter);

                    let init = v.init
                        .map(|init| {
                            self.expression(init, &mut ectx)
                        })
                        .transpose()?;

                    let explicit_ty = v.ty
                        .map(|ty| {
                            self.resolve_ast_type(ty, &mut ectx.as_global())
                        })
                        .transpose()?;

                    let var_ty: Handle<crate::Type>;
                    let var_init: Option<Handle<crate::Expression>>;
                    match (explicit_ty, init) {
                        (Some(explicit_ty), Some(init)) => {
                            var_ty = explicit_ty;
                            let explicit_res = Loaded::Concrete(TypeResolution::Handle(explicit_ty));
                            match ectx.apply_automatic_conversions(init, explicit_res)? {
                                Some(converted) => {
                                    // Since explicit_ty was written out, it
                                    // must not be abstract, so `converted`
                                    // should always be `Loaded::Concrete`.
                                    var_init = Some(converted.handle());
                                },
                                None => {
                                    let init_ty = init.try_map(|handle| Ok(resolve!(ectx, handle)))?;
                                    let ctx = ectx.as_globalctx();
                                    return Err(Error::InitializationTypeMismatch {
                                        name: v.name.span,
                                        expected: explicit_ty.to_wgsl(&ctx),
                                        got: init_ty.to_wgsl(&ctx),
                                    });
                                }
                            }
                        }
                        (Some(explicit_ty), None) => {
                            var_ty = explicit_ty;
                            var_init = None;
                        },
                        (None, Some(init)) => {
                            let concretized = ectx.concretize(init)?;
                            var_init = Some(concretized);
                            var_ty = ectx.register_type(concretized)?;
                        }
                        (None, None) => {
                            return Err(Error::MissingType(v.name.span));
                        }
                    };

                    let (const_initializer, initializer) = {
                        match var_init {
                            Some(var_init) => {
                                // It's not correct to hoist the initializer up
                                // to the top of the function if:
                                // - the initialization is inside a loop, and should
                                //   take place on every iteration, or
                                // - the initialization is not a constant
                                //   expression, so its value depends on the
                                //   state at the point of initialization.
                                if is_inside_loop || !ctx.expression_constness.is_const(var_init) {
                                    (None, Some(var_init))
                                } else {
                                    (Some(var_init), None)
                                }
                            }
                            None => (None, None),
                        }
                    };

                    let var = ctx.function.local_variables.append(
                        crate::LocalVariable {
                            name: Some(v.name.name.to_string()),
                            ty: var_ty,
                            init: const_initializer,
                        },
                        stmt.span,
                    );

                    let var_expr = ctx.as_expression(block, &mut emitter).interrupt_emitter(
                        crate::Expression::LocalVariable(var),
                        Span::UNDEFINED,
                    )?;
                    block.extend(emitter.finish(&ctx.function.expressions));
                    ctx.local_table.insert(v.handle, Typed::Reference(var_expr));

                    match initializer {
                        Some(initializer) => crate::Statement::Store {
                            pointer: var_expr,
                            value: initializer,
                        },
                        None => return Ok(()),
                    }
                }
            },
            ast::StatementKind::If {
                condition,
                ref accept,
                ref reject,
            } => {
                let mut emitter = Emitter::default();
                emitter.start(&ctx.function.expressions);

                let condition =
                    self.expression(condition, &mut ctx.as_expression(block, &mut emitter))?;
                block.extend(emitter.finish(&ctx.function.expressions));

                let accept = self.block(accept, is_inside_loop, ctx)?;
                let reject = self.block(reject, is_inside_loop, ctx)?;

                crate::Statement::If {
                    condition: condition.todo(),
                    accept,
                    reject,
                }
            }
            ast::StatementKind::Switch {
                selector,
                ref cases,
            } => {
                let mut emitter = Emitter::default();
                emitter.start(&ctx.function.expressions);

                let mut ectx = ctx.as_expression(block, &mut emitter);
                let selector = self.expression(selector, &mut ectx)?;
                let selector = selector.todo();

                let uint =
                    resolve_inner!(ectx, selector).scalar_kind() == Some(crate::ScalarKind::Uint);
                block.extend(emitter.finish(&ctx.function.expressions));

                let cases = cases
                    .iter()
                    .map(|case| {
                        Ok(crate::SwitchCase {
                            value: match case.value {
                                ast::SwitchValue::Expr(expr) => {
                                    let span = ctx.ast_expressions.get_span(expr);
                                    let expr =
                                        self.expression(expr, &mut ctx.as_global().as_const())?;
                                    match ctx.module.to_ctx().eval_expr_to_literal(expr.todo()) {
                                        Some(crate::Literal::I32(value)) if !uint => {
                                            crate::SwitchValue::I32(value)
                                        }
                                        Some(crate::Literal::U32(value)) if uint => {
                                            crate::SwitchValue::U32(value)
                                        }
                                        _ => {
                                            return Err(Error::InvalidSwitchValue { uint, span });
                                        }
                                    }
                                }
                                ast::SwitchValue::Default => crate::SwitchValue::Default,
                            },
                            body: self.block(&case.body, is_inside_loop, ctx)?,
                            fall_through: case.fall_through,
                        })
                    })
                    .collect::<Result<_, _>>()?;

                crate::Statement::Switch { selector, cases }
            }
            ast::StatementKind::Loop {
                ref body,
                ref continuing,
                break_if,
            } => {
                let body = self.block(body, true, ctx)?;
                let mut continuing = self.block(continuing, true, ctx)?;

                let mut emitter = Emitter::default();
                emitter.start(&ctx.function.expressions);
                let break_if = break_if
                    .map(|expr| self.expression(expr, &mut ctx.as_expression(block, &mut emitter)))
                    .transpose()?
                    .map(Loaded::todo);
                continuing.extend(emitter.finish(&ctx.function.expressions));

                crate::Statement::Loop {
                    body,
                    continuing,
                    break_if,
                }
            }
            ast::StatementKind::Break => crate::Statement::Break,
            ast::StatementKind::Continue => crate::Statement::Continue,
            ast::StatementKind::Return { value } => {
                let mut emitter = Emitter::default();
                emitter.start(&ctx.function.expressions);

                let value = value
                    .map(|expr| self.expression(expr, &mut ctx.as_expression(block, &mut emitter)))
                    .transpose()?
                    .map(Loaded::todo);
                block.extend(emitter.finish(&ctx.function.expressions));

                crate::Statement::Return { value }
            }
            ast::StatementKind::Kill => crate::Statement::Kill,
            ast::StatementKind::Call {
                ref function,
                ref arguments,
            } => {
                let mut emitter = Emitter::default();
                emitter.start(&ctx.function.expressions);

                let _ = self.call(
                    stmt.span,
                    function,
                    arguments,
                    &mut ctx.as_expression(block, &mut emitter),
                )?;
                block.extend(emitter.finish(&ctx.function.expressions));
                return Ok(());
            }
            ast::StatementKind::Assign {
                target: ast_target,
                op,
                value,
            } => {
                let mut emitter = Emitter::default();
                emitter.start(&ctx.function.expressions);
                let ectx = ctx.as_expression(block, &mut emitter);

                let target = self.expression_for_reference(ast_target, &mut ectx)?;

                // `target` must be a WGSL reference. Select appropriate errors
                // for everything else.
                let target_handle = match target {
                    Typed::Reference(handle) => handle,
                    Typed::Loaded(Loaded::Concrete(handle)) => {
                        let ty = ctx.invalid_assignment_type(handle);
                        return Err(Error::InvalidAssignment {
                            span: ctx.ast_expressions.get_span(ast_target),
                            ty,
                        });
                    }
                    Typed::Loaded(Loaded::Abstract(_)) => {
                        return Err(Error::InvalidAssignment {
                            span: ctx.ast_expressions.get_span(ast_target),
                            ty: InvalidAssignmentType::Other,
                        });
                    }
                };

                let value = self.expression(value, &mut ectx)?;

                let new_value;
                match op {
                    Some(op) => {
                        let mut left = ectx.apply_load_rule(target)?;
                        let expr = self.binary(op, left, value, &mut ectx)?;
                        let expr = match expr {
                            Loaded::Concrete(handle) => handle,
                            // All binary operators return concrete results if
                            // either operand is concrete, and `target` is
                            // storable, so it must be concrete.
                            Loaded::Abstract(_) => unreachable!(),
                        };
                        new_value = ectx.append_expression(expr, stmt.span)?;
                    }
                    None => {
                        // If `value` is abstract, it must be convertable to
                        // `target`'s type via WGSL's automatic conversions.
                        let target_type = ectx.register_type(target_handle)?;
                        let target_res = Loaded::Concrete(TypeResolution::Handle(target_type));
                        match ectx.apply_automatic_conversions(value, target_res)? {
                            Some(converted) => {
                                // Since `target` is concrete, we know the new
                                // value must be concrete.
                                new_value = converted.handle();
                            }
                            None => {
                                let value_type = value.try_map(|handle| Ok(resolve!(ectx, handle)))?;
                                let gctx = ectx.as_globalctx();
                                return Err(Error::InvalidAssignment {
                                    span: stmt.span,
                                    ty: InvalidAssignmentType::Type {
                                        to_type: target_type.to_wgsl(&gctx),
                                        from_type: value_type.to_wgsl(&gctx),
                                    },
                                });
                            }
                        }
                    }
                }

                block.extend(emitter.finish(&ctx.function.expressions));
                crate::Statement::Store {
                    pointer: target_handle,
                    value: new_value,
                }
            }
            ast::StatementKind::Increment(value) | ast::StatementKind::Decrement(value) => {
                let mut emitter = Emitter::default();
                emitter.start(&ctx.function.expressions);

                let op = match stmt.kind {
                    ast::StatementKind::Increment(_) => crate::BinaryOperator::Add,
                    ast::StatementKind::Decrement(_) => crate::BinaryOperator::Subtract,
                    _ => unreachable!(),
                };

                let value_span = ctx.ast_expressions.get_span(value);
                let target = self
                    .expression_for_reference(value, &mut ctx.as_expression(block, &mut emitter))?;
                let target_handle = match target {
                    Typed::Reference(handle) => handle,
                    Typed::Loaded(_) => return Err(Error::BadIncrDecrReferenceType(value_span)),
                };

                let mut ectx = ctx.as_expression(block, &mut emitter);
                let (kind, width) = match *resolve_inner!(ectx, target_handle) {
                    crate::TypeInner::ValuePointer {
                        size: None,
                        kind,
                        width,
                        ..
                    } => (kind, width),
                    crate::TypeInner::Pointer { base, .. } => match ectx.module.types[base].inner {
                        crate::TypeInner::Scalar { kind, width } => (kind, width),
                        _ => return Err(Error::BadIncrDecrReferenceType(value_span)),
                    },
                    _ => return Err(Error::BadIncrDecrReferenceType(value_span)),
                };
                let literal = match kind {
                    crate::ScalarKind::Sint | crate::ScalarKind::Uint => {
                        crate::Literal::one(kind, width)
                            .ok_or(Error::BadIncrDecrReferenceType(value_span))?
                    }
                    _ => return Err(Error::BadIncrDecrReferenceType(value_span)),
                };

                let right =
                    ectx.interrupt_emitter(crate::Expression::Literal(literal), Span::UNDEFINED)?;
                let rctx = ectx.runtime_expression_ctx(stmt.span)?;
                let left = rctx.function.expressions.append(
                    crate::Expression::Load {
                        pointer: target_handle,
                    },
                    value_span,
                );
                let value = rctx
                    .function
                    .expressions
                    .append(crate::Expression::Binary { op, left, right }, stmt.span);

                block.extend(emitter.finish(&ctx.function.expressions));
                crate::Statement::Store {
                    pointer: target_handle,
                    value,
                }
            }
            ast::StatementKind::Ignore(expr) => {
                let mut emitter = Emitter::default();
                emitter.start(&ctx.function.expressions);

                let _ = self.expression(expr, &mut ctx.as_expression(block, &mut emitter))?;
                block.extend(emitter.finish(&ctx.function.expressions));
                return Ok(());
            }
        };

        block.push(out, stmt.span);

        Ok(())
    }

    /// Lower `expr` and apply the WGSL Load Rule.
    fn expression(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Loaded<Handle<crate::Expression>>, Error<'source>> {
        let expr = self.expression_for_reference(expr, ctx)?;
        ctx.apply_load_rule(expr)
    }

    /// Lower `expr`, but do not apply the Load Rule.
    ///
    /// WGSL says that the Load Rule should only be applied when needed to
    /// satisfy a type rule. Contexts like the target of an assignment or the
    /// operand of the `&` operator should use this function: these context
    /// require a reference, so the Load Rule would be inappropriate.
    fn expression_for_reference(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Typed<Handle<crate::Expression>>, Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);
        let expr = &ctx.ast_expressions[expr];

        let expr: Typed<crate::Expression> = match *expr {
            ast::Expression::Literal(literal) => {
                return self.literal(literal, span, ctx);
            }
            ast::Expression::Ident(ast::IdentExpr::Local(local)) => {
                let rctx = ctx.runtime_expression_ctx(span)?;
                return Ok(rctx.local_table[&local]);
            }
            ast::Expression::Ident(ast::IdentExpr::Unresolved(name)) => {
                let global = ctx
                    .globals
                    .get(name)
                    .ok_or(Error::UnknownIdent(span, name))?;

                let expr = match *global {
                    LoweredGlobalDecl::Var(handle) => {
                        let global_expr = crate::Expression::GlobalVariable(handle);
                        // Uses of global variables in the `Handle` address
                        // space don't produce references.
                        if ctx.module.global_variables[handle].space == crate::AddressSpace::Handle
                        {
                            Typed::Loaded(Loaded::Concrete(global_expr))
                        } else {
                            Typed::Reference(global_expr)
                        }
                    }
                    LoweredGlobalDecl::Const(constant) => {
                        Typed::Loaded(Loaded::Concrete(crate::Expression::Constant(constant)))
                    }
                    _ => {
                        return Err(Error::Unexpected(span, ExpectedToken::Variable));
                    }
                };

                return expr.try_map(|handle| ctx.interrupt_emitter(handle, span));
            }
            ast::Expression::Construct {
                ref ty,
                ty_span,
                ref components,
            } => {
                return self
                    .construct(span, ty, ty_span, components, ctx)
                    .map(Typed::Loaded);
            }
            ast::Expression::Unary { op, expr } => {
                // All unary operators that accept abstract types at
                // all simply preserve the type.
                let expr = self.expression(expr, ctx)?;
                Typed::Loaded(expr.map(|expr| crate::Expression::Unary { op, expr }))
            }
            ast::Expression::Binary { op, left, right } => {
                // There are no binary operators that operate on references, so
                // apply the Load Rule to both operands.
                let left = self.expression(left, ctx)?;
                let right = self.expression(right, ctx)?;

                Typed::Loaded(self.binary(op, left, right, ctx)?)
            }
            ast::Expression::AddrOf(expr) => {
                // Lower the operand, but do not apply the Load Rule. WGSL says
                // that the Load Rule should only be applied when needed to
                // satisfy a type rule. The `&` operator requires a reference as
                // its operand, so the Load Rule is never necessary.
                let reference = self.expression_for_reference(expr, ctx)?;
                match reference {
                    Typed::Reference(expr) => {
                        // No code is generated. The underlying Naga pointer
                        // used to be a WGSL reference, but now it's a WGSL
                        // pointer. Abstract types aren't storable, so it's
                        // definitely concrete.
                        return Ok(Typed::Loaded(Loaded::Concrete(expr)));
                    }
                    Typed::Loaded(_) => {
                        return Err(Error::NotReference("the operand of the `&` operator", span));
                    }
                }
            }
            ast::Expression::Deref(expr) => {
                // The `*` operator does not accept references as operands,
                // so we must apply the Load Rule as needed.
                let pointer = self.expression(expr, ctx)?;

                match pointer {
                    Loaded::Concrete(pointer) => {
                        if resolve_inner!(ctx, pointer).pointer_space().is_none() {
                            return Err(Error::NotPointer(span));
                        }
                        // No code is actually generated. The underlying Naga
                        // pointer used to be a WGSL pointer, but now it's a
                        // WGSL reference. Abstract types aren't storable, so
                        // it's definitely concrete.
                        return Ok(Typed::Reference(pointer));
                    }
                    Loaded::Abstract(_) => return Err(Error::NotPointer(span)),
                }
            }
            ast::Expression::Call {
                ref function,
                ref arguments,
            } => {
                let handle = self
                    .call(span, function, arguments, ctx)?
                    .ok_or(Error::FunctionReturnsVoid(function.span))?;
                // Calls can't return references or abstract types.
                return Ok(Typed::Loaded(Loaded::Concrete(handle)));
            }
            ast::Expression::Index { base, index } => {
                let lowered_base = self.expression_for_reference(base, ctx)?;
                let index = self.expression(index, ctx)?;
                let index = index.todo();

                // WGSL subscripting can't be applied to pointers.
                if let Typed::Loaded(Loaded::Concrete(base_handle)) = lowered_base {
                    if resolve_inner!(ctx, base_handle).pointer_space().is_some() {
                        return Err(Error::Pointer(
                            "the value indexed by a `[]` subscripting expression",
                            ctx.ast_expressions.get_span(base),
                        ));
                    }
                }

                lowered_base.map(|base| match ctx.const_access(index) {
                    Some(index) => crate::Expression::AccessIndex { base, index },
                    None => crate::Expression::Access { base, index },
                })
            }
            ast::Expression::Member { base, ref field } => self.member(base, field, ctx)?,
            ast::Expression::Bitcast { expr, to, ty_span } => {
                let expr = self.expression(expr, ctx)?;
                let expr = expr.todo();
                let to_resolved = self.resolve_ast_type(to, &mut ctx.as_global())?;

                let kind = match ctx.module.types[to_resolved].inner {
                    crate::TypeInner::Scalar { kind, .. } => kind,
                    crate::TypeInner::Vector { kind, .. } => kind,
                    _ => {
                        let ty = resolve!(ctx, expr);
                        return Err(Error::BadTypeCast {
                            from_type: ctx.format_type_resolution(ty),
                            span: ty_span,
                            to_type: ctx.format_type(to_resolved),
                        });
                    }
                };

                Typed::Loaded(Loaded::Concrete(crate::Expression::As {
                    expr,
                    kind,
                    convert: None,
                }))
            }
        };

        expr.try_map(|handle| ctx.append_expression(handle, span))
    }

    /// Generate Naga IR for call expressions and statements, and type
    /// constructor expressions.
    ///
    /// The "function" being called is simply an `Ident` that we know refers to
    /// some module-scope definition.
    ///
    /// - If it is the name of a type, then the expression is a type constructor
    ///   expression: either constructing a value from components, a conversion
    ///   expression, or a zero value expression.
    ///
    /// - If it is the name of a function, then we're generating a [`Call`]
    ///   statement. We may be in the midst of generating code for an
    ///   expression, in which case we must generate an `Emit` statement to
    ///   force evaluation of the IR expressions we've generated so far, add the
    ///   `Call` statement to the current block, and then resume generating
    ///   expressions.
    ///
    /// [`Call`]: crate::Statement::Call
    fn call(
        &mut self,
        span: Span,
        function: &ast::Ident<'source>,
        arguments: &[Handle<ast::Expression<'source>>],
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Option<Handle<crate::Expression>>, Error<'source>> {
        match ctx.globals.get(function.name) {
            Some(&LoweredGlobalDecl::Type(ty)) => {
                let handle = self.construct(
                    span,
                    &ast::ConstructorType::Type(ty),
                    function.span,
                    arguments,
                    ctx,
                )?;
                Ok(Some(handle))
            }
            Some(&LoweredGlobalDecl::Const(_) | &LoweredGlobalDecl::Var(_)) => {
                Err(Error::Unexpected(function.span, ExpectedToken::Function))
            }
            Some(&LoweredGlobalDecl::EntryPoint) => Err(Error::CalledEntryPoint(function.span)),
            Some(&LoweredGlobalDecl::Function(function)) => {
                let arguments = arguments
                    .iter()
                    .map(|&arg| self.expression(arg, ctx))
                    .collect::<Result<Vec<_>, _>>()?;

                let has_result = ctx.module.functions[function].result.is_some();
                let rctx = ctx.runtime_expression_ctx(span)?;
                // we need to always do this before a fn call since all arguments need to be emitted before the fn call
                rctx.block
                    .extend(rctx.emitter.finish(&rctx.function.expressions));
                let result = has_result.then(|| {
                    rctx.function
                        .expressions
                        .append(crate::Expression::CallResult(function), span)
                });
                rctx.emitter.start(&rctx.function.expressions);
                rctx.block.push(
                    crate::Statement::Call {
                        function,
                        arguments,
                        result,
                    },
                    span,
                );

                Ok(result)
            }
            None => {
                let span = function.span;
                let expr = if let Some(fun) = conv::map_relational_fun(function.name) {
                    let mut args = ctx.prepare_args(arguments, 1, span);
                    let argument = self.expression(args.next()?, ctx)?;
                    args.finish()?;

                    // Check for no-op all(bool) and any(bool):
                    let argument_unmodified = matches!(
                        fun,
                        crate::RelationalFunction::All | crate::RelationalFunction::Any
                    ) && {
                        matches!(
                            resolve_inner!(ctx, argument),
                            &crate::TypeInner::Scalar {
                                kind: crate::ScalarKind::Bool,
                                ..
                            }
                        )
                    };

                    if argument_unmodified {
                        return Ok(Some(argument));
                    } else {
                        crate::Expression::Relational { fun, argument }
                    }
                } else if let Some((axis, ctrl)) = conv::map_derivative(function.name) {
                    let mut args = ctx.prepare_args(arguments, 1, span);
                    let expr = self.expression(args.next()?, ctx)?;
                    args.finish()?;

                    crate::Expression::Derivative { axis, ctrl, expr }
                } else if let Some(fun) = conv::map_standard_fun(function.name) {
                    let expected = fun.argument_count() as _;
                    let mut args = ctx.prepare_args(arguments, expected, span);

                    let arg = self.expression(args.next()?, ctx)?;
                    let arg1 = args
                        .next()
                        .map(|x| self.expression(x, ctx))
                        .ok()
                        .transpose()?;
                    let arg2 = args
                        .next()
                        .map(|x| self.expression(x, ctx))
                        .ok()
                        .transpose()?;
                    let arg3 = args
                        .next()
                        .map(|x| self.expression(x, ctx))
                        .ok()
                        .transpose()?;

                    args.finish()?;

                    if fun == crate::MathFunction::Modf || fun == crate::MathFunction::Frexp {
                        if let Some((size, width)) = match *resolve_inner!(ctx, arg) {
                            crate::TypeInner::Scalar { width, .. } => Some((None, width)),
                            crate::TypeInner::Vector { size, width, .. } => {
                                Some((Some(size), width))
                            }
                            _ => None,
                        } {
                            ctx.module.generate_predeclared_type(
                                if fun == crate::MathFunction::Modf {
                                    crate::PredeclaredType::ModfResult { size, width }
                                } else {
                                    crate::PredeclaredType::FrexpResult { size, width }
                                },
                            );
                        }
                    }

                    crate::Expression::Math {
                        fun,
                        arg,
                        arg1,
                        arg2,
                        arg3,
                    }
                } else if let Some(fun) = Texture::map(function.name) {
                    self.texture_sample_helper(fun, arguments, span, ctx)?
                } else {
                    match function.name {
                        "select" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);

                            let reject = self.expression(args.next()?, ctx)?;
                            let accept = self.expression(args.next()?, ctx)?;
                            let condition = self.expression(args.next()?, ctx)?;

                            args.finish()?;

                            crate::Expression::Select {
                                reject,
                                accept,
                                condition,
                            }
                        }
                        "arrayLength" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let expr = self.expression(args.next()?, ctx)?;
                            args.finish()?;

                            crate::Expression::ArrayLength(expr)
                        }
                        "atomicLoad" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let pointer = self.atomic_pointer(args.next()?, ctx)?;
                            args.finish()?;

                            crate::Expression::Load { pointer }
                        }
                        "atomicStore" => {
                            let mut args = ctx.prepare_args(arguments, 2, span);
                            let pointer = self.atomic_pointer(args.next()?, ctx)?;
                            let value = self.expression(args.next()?, ctx)?;
                            args.finish()?;

                            let rctx = ctx.runtime_expression_ctx(span)?;
                            rctx.block
                                .extend(rctx.emitter.finish(&rctx.function.expressions));
                            rctx.emitter.start(&rctx.function.expressions);
                            rctx.block
                                .push(crate::Statement::Store { pointer, value }, span);
                            return Ok(None);
                        }
                        "atomicAdd" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Add,
                                arguments,
                                ctx,
                            )?))
                        }
                        "atomicSub" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Subtract,
                                arguments,
                                ctx,
                            )?))
                        }
                        "atomicAnd" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::And,
                                arguments,
                                ctx,
                            )?))
                        }
                        "atomicOr" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::InclusiveOr,
                                arguments,
                                ctx,
                            )?))
                        }
                        "atomicXor" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::ExclusiveOr,
                                arguments,
                                ctx,
                            )?))
                        }
                        "atomicMin" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Min,
                                arguments,
                                ctx,
                            )?))
                        }
                        "atomicMax" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Max,
                                arguments,
                                ctx,
                            )?))
                        }
                        "atomicExchange" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Exchange { compare: None },
                                arguments,
                                ctx,
                            )?))
                        }
                        "atomicCompareExchangeWeak" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);

                            let pointer = self.atomic_pointer(args.next()?, ctx)?;

                            let compare = self.expression(args.next()?, ctx)?;

                            let value = args.next()?;
                            let value_span = ctx.ast_expressions.get_span(value);
                            let value = self.expression(value, ctx)?;

                            args.finish()?;

                            let expression = match *resolve_inner!(ctx, value) {
                                crate::TypeInner::Scalar { kind, width } => {
                                    crate::Expression::AtomicResult {
                                        ty: ctx.module.generate_predeclared_type(
                                            crate::PredeclaredType::AtomicCompareExchangeWeakResult {
                                                kind,
                                                width,
                                            },
                                        ),
                                        comparison: true,
                                    }
                                }
                                _ => return Err(Error::InvalidAtomicOperandType(value_span)),
                            };

                            let result = ctx.interrupt_emitter(expression, span)?;
                            let rctx = ctx.runtime_expression_ctx(span)?;
                            rctx.block.push(
                                crate::Statement::Atomic {
                                    pointer,
                                    fun: crate::AtomicFunction::Exchange {
                                        compare: Some(compare),
                                    },
                                    value,
                                    result,
                                },
                                span,
                            );
                            return Ok(Some(result));
                        }
                        "storageBarrier" => {
                            ctx.prepare_args(arguments, 0, span).finish()?;

                            let rctx = ctx.runtime_expression_ctx(span)?;
                            rctx.block
                                .push(crate::Statement::Barrier(crate::Barrier::STORAGE), span);
                            return Ok(None);
                        }
                        "workgroupBarrier" => {
                            ctx.prepare_args(arguments, 0, span).finish()?;

                            let rctx = ctx.runtime_expression_ctx(span)?;
                            rctx.block
                                .push(crate::Statement::Barrier(crate::Barrier::WORK_GROUP), span);
                            return Ok(None);
                        }
                        "workgroupUniformLoad" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let expr = args.next()?;
                            args.finish()?;

                            let pointer = self.expression(expr, ctx)?;
                            let result_ty = match *resolve_inner!(ctx, pointer) {
                                crate::TypeInner::Pointer {
                                    base,
                                    space: crate::AddressSpace::WorkGroup,
                                } => base,
                                ref other => {
                                    log::error!("Type {other:?} passed to workgroupUniformLoad");
                                    let span = ctx.ast_expressions.get_span(expr);
                                    return Err(Error::InvalidWorkGroupUniformLoad(span));
                                }
                            };
                            let result = ctx.interrupt_emitter(
                                crate::Expression::WorkGroupUniformLoadResult { ty: result_ty },
                                span,
                            )?;
                            let rctx = ctx.runtime_expression_ctx(span)?;
                            rctx.block.push(
                                crate::Statement::WorkGroupUniformLoad { pointer, result },
                                span,
                            );

                            return Ok(Some(result));
                        }
                        "textureStore" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);

                            let image = args.next()?;
                            let image_span = ctx.ast_expressions.get_span(image);
                            let image = self.expression(image, ctx)?;

                            let coordinate = self.expression(args.next()?, ctx)?;

                            let (_, arrayed) = ctx.image_data(image, image_span)?;
                            let array_index = arrayed
                                .then(|| {
                                    args.min_args += 1;
                                    self.expression(args.next()?, ctx)
                                })
                                .transpose()?;

                            let value = self.expression(args.next()?, ctx)?;

                            args.finish()?;

                            let rctx = ctx.runtime_expression_ctx(span)?;
                            rctx.block
                                .extend(rctx.emitter.finish(&rctx.function.expressions));
                            rctx.emitter.start(&rctx.function.expressions);
                            let stmt = crate::Statement::ImageStore {
                                image,
                                coordinate,
                                array_index,
                                value,
                            };
                            rctx.block.push(stmt, span);
                            return Ok(None);
                        }
                        "textureLoad" => {
                            let mut args = ctx.prepare_args(arguments, 2, span);

                            let image = args.next()?;
                            let image_span = ctx.ast_expressions.get_span(image);
                            let image = self.expression(image, ctx)?;

                            let coordinate = self.expression(args.next()?, ctx)?;

                            let (class, arrayed) = ctx.image_data(image, image_span)?;
                            let array_index = arrayed
                                .then(|| {
                                    args.min_args += 1;
                                    self.expression(args.next()?, ctx)
                                })
                                .transpose()?;

                            let level = class
                                .is_mipmapped()
                                .then(|| {
                                    args.min_args += 1;
                                    self.expression(args.next()?, ctx)
                                })
                                .transpose()?;

                            let sample = class
                                .is_multisampled()
                                .then(|| self.expression(args.next()?, ctx))
                                .transpose()?;

                            args.finish()?;

                            crate::Expression::ImageLoad {
                                image,
                                coordinate,
                                array_index,
                                level,
                                sample,
                            }
                        }
                        "textureDimensions" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let image = self.expression(args.next()?, ctx)?;
                            let level = args
                                .next()
                                .map(|arg| self.expression(arg, ctx))
                                .ok()
                                .transpose()?;
                            args.finish()?;

                            crate::Expression::ImageQuery {
                                image,
                                query: crate::ImageQuery::Size { level },
                            }
                        }
                        "textureNumLevels" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let image = self.expression(args.next()?, ctx)?;
                            args.finish()?;

                            crate::Expression::ImageQuery {
                                image,
                                query: crate::ImageQuery::NumLevels,
                            }
                        }
                        "textureNumLayers" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let image = self.expression(args.next()?, ctx)?;
                            args.finish()?;

                            crate::Expression::ImageQuery {
                                image,
                                query: crate::ImageQuery::NumLayers,
                            }
                        }
                        "textureNumSamples" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let image = self.expression(args.next()?, ctx)?;
                            args.finish()?;

                            crate::Expression::ImageQuery {
                                image,
                                query: crate::ImageQuery::NumSamples,
                            }
                        }
                        "rayQueryInitialize" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);
                            let query = self.ray_query_pointer(args.next()?, ctx)?;
                            let acceleration_structure = self.expression(args.next()?, ctx)?;
                            let descriptor = self.expression(args.next()?, ctx)?;
                            args.finish()?;

                            let _ = ctx.module.generate_ray_desc_type();
                            let fun = crate::RayQueryFunction::Initialize {
                                acceleration_structure,
                                descriptor,
                            };

                            let rctx = ctx.runtime_expression_ctx(span)?;
                            rctx.block
                                .extend(rctx.emitter.finish(&rctx.function.expressions));
                            rctx.emitter.start(&rctx.function.expressions);
                            rctx.block
                                .push(crate::Statement::RayQuery { query, fun }, span);
                            return Ok(None);
                        }
                        "rayQueryProceed" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let query = self.ray_query_pointer(args.next()?, ctx)?;
                            args.finish()?;

                            let result = ctx.interrupt_emitter(
                                crate::Expression::RayQueryProceedResult,
                                span,
                            )?;
                            let fun = crate::RayQueryFunction::Proceed { result };
                            let rctx = ctx.runtime_expression_ctx(span)?;
                            rctx.block
                                .push(crate::Statement::RayQuery { query, fun }, span);
                            return Ok(Some(result));
                        }
                        "rayQueryGetCommittedIntersection" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let query = self.ray_query_pointer(args.next()?, ctx)?;
                            args.finish()?;

                            let _ = ctx.module.generate_ray_intersection_type();

                            crate::Expression::RayQueryGetIntersection {
                                query,
                                committed: true,
                            }
                        }
                        "RayDesc" => {
                            let ty = ctx.module.generate_ray_desc_type();
                            let handle = self.construct(
                                span,
                                &ast::ConstructorType::Type(ty),
                                function.span,
                                arguments,
                                ctx,
                            )?;
                            return Ok(Some(handle));
                        }
                        _ => return Err(Error::UnknownIdent(function.span, function.name)),
                    }
                };

                let expr = ctx.append_expression(expr, span)?;
                Ok(Some(expr))
            }
        }
    }

    fn binary(
        &mut self,
        op: crate::BinaryOperator,
        left: Loaded<Handle<crate::Expression>>,
        right: Loaded<Handle<crate::Expression>>,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Loaded<crate::Expression>, Error<'source>> {
        use Loaded as Lo;
        ctx.binary_op_splat(op, &mut left, &mut right)?;

        // Concretize abstract operands as necessary.
        let result = if let crate::BinaryOperator::ShiftLeft | crate::BinaryOperator::ShiftRight =
            op
        {
            todo!()
        } else {
            // Aside from the bit shift operators, all binary operators
            // require both operands to be of the same scalar kind.
            match (left, right) {
                // If both operands are concrete, then the result is concrete.
                (Lo::Concrete(left), Lo::Concrete(right)) => {
                    Lo::Concrete(crate::Expression::Binary { op, left, right })
                }

                // If both operands are abstract, then the result is
                // still abstract.
                (Lo::Abstract(left), Lo::Abstract(right)) => {
                    // This arm isn't right: the two operands may have different
                    // types, and need to be reconciled to a single type.
                    // Specifically, AbstractInt coerces to AbstractFloat.
                    //
                    // This should probably find a way to use unify_linear_components,
                    // which handles that case.
                    todo!();
                    Lo::Abstract(crate::Expression::Binary { op, left, right })
                }

                // Otherwise, the abstract operand needs to be converted
                // to the scalar kind of the concrete operand.
                (Lo::Abstract(mut left), Lo::Concrete(right)) => {
                    let right_inner = resolve_inner!(ctx, right);
                    todo!();
                    Lo::Concrete(crate::Expression::Binary { op, left, right })
                }
                (Lo::Concrete(left), Lo::Abstract(mut right)) => {
                    let left_inner = resolve_inner!(ctx, left);
                    todo!();
                    Lo::Concrete(crate::Expression::Binary { op, left, right })
                }
            }
        };

        Ok(result)
    }

    fn member(
        &mut self,
        base_ast: Handle<ast::Expression>,
        field: &ast::Ident<'source>,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Typed<crate::Expression>, Error<'source>> {
        let mut base = self.expression_for_reference(base_ast, ctx)?;

        // Determine what type of composite we're actually accessing a
        // member/members of.
        let temp_composite_inner;
        let composite_inner;
        match base {
            Typed::Reference(base_handle) => {
                // The actual Naga expression had better be a pointer. Follow it
                // to find the composite type.
                match *resolve_inner!(ctx, base_handle) {
                    crate::TypeInner::Pointer {
                        base: composite_ty, ..
                    } => {
                        composite_inner = &ctx.module.types[composite_ty].inner;
                    }
                    crate::TypeInner::ValuePointer {
                        size, kind, width, ..
                    } => {
                        temp_composite_inner = match size {
                            Some(size) => crate::TypeInner::Vector { size, kind, width },
                            None => crate::TypeInner::Scalar { kind, width },
                        };
                        composite_inner = &temp_composite_inner;
                    }
                    ref other => {
                        // If the WGSL expression is a reference, the Naga type
                        // really ought to be a pointer.
                        return Err(Error::Internal(
                            "Typed::Reference expr was not a Naga pointer",
                        ));
                    }
                }
            }
            Typed::Abstract(base_handle) | Typed::Loaded(base_handle) => {
                composite_inner = resolve_inner!(ctx, base_handle);

                // Here, a Naga pointer type represents a WGSL pointer type.
                // WGSL doesn't allow member access on pointers.
                if let crate::TypeInner::Pointer { .. } | crate::TypeInner::ValuePointer { .. } =
                    *composite_inner
                {
                    return Err(Error::Pointer(
                        "the value accessed by a `.member` expression",
                        ctx.ast_expressions.get_span(base_ast),
                    ));
                }
            }
        }

        let access = match *composite_inner {
            crate::TypeInner::Struct { ref members, .. } => {
                let index = members
                    .iter()
                    .position(|m| m.name.as_deref() == Some(field.name))
                    .ok_or(Error::BadAccessor(field.span))? as u32;

                base.map(|base| crate::Expression::AccessIndex { base, index })
            }
            crate::TypeInner::Vector { .. } => {
                match Components::new(field.name, field.span)? {
                    Components::Swizzle { size, pattern } => {
                        // WGSL does not support assignment to swizzles, so the
                        // result of a swizzle is no longer a reference.
                        base = ctx.apply_load_rule(base)?;
                        // We know `base` can't be a `Typed::Reference`; this
                        // `map` preserves the `Abstract`/`Plain` distinction.
                        base.map(|vector| crate::Expression::Swizzle {
                            size,
                            vector,
                            pattern,
                        })
                    }
                    Components::Single(index) => {
                        base.map(|base| crate::Expression::AccessIndex { base, index })
                    }
                }
            }
            _ => return Err(Error::BadAccessor(field.span)),
        };

        Ok(access)
    }

    fn atomic_pointer(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);
        let pointer = self.expression(expr, ctx)?;

        match *resolve_inner!(ctx, pointer) {
            crate::TypeInner::Pointer { base, .. } => match ctx.module.types[base].inner {
                crate::TypeInner::Atomic { .. } => Ok(pointer),
                ref other => {
                    log::error!("Pointer type to {:?} passed to atomic op", other);
                    Err(Error::InvalidAtomicPointer(span))
                }
            },
            ref other => {
                log::error!("Type {:?} passed to atomic op", other);
                Err(Error::InvalidAtomicPointer(span))
            }
        }
    }

    fn atomic_helper(
        &mut self,
        span: Span,
        fun: crate::AtomicFunction,
        args: &[Handle<ast::Expression<'source>>],
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let mut args = ctx.prepare_args(args, 2, span);

        let pointer = self.atomic_pointer(args.next()?, ctx)?;

        let value = args.next()?;
        let value = self.expression(value, ctx)?;
        let ty = ctx.register_type(value)?;

        args.finish()?;

        let result = ctx.interrupt_emitter(
            crate::Expression::AtomicResult {
                ty,
                comparison: false,
            },
            span,
        )?;
        let rctx = ctx.runtime_expression_ctx(span)?;
        rctx.block.push(
            crate::Statement::Atomic {
                pointer,
                fun,
                value,
                result,
            },
            span,
        );
        Ok(result)
    }

    fn texture_sample_helper(
        &mut self,
        fun: Texture,
        args: &[Handle<ast::Expression<'source>>],
        span: Span,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<crate::Expression, Error<'source>> {
        let mut args = ctx.prepare_args(args, fun.min_argument_count(), span);

        fn get_image_and_span<'source>(
            lowerer: &mut Lowerer<'source, '_>,
            args: &mut ArgumentContext<'_, 'source>,
            ctx: &mut ExpressionContext<'source, '_, '_>,
        ) -> Result<(Handle<crate::Expression>, Span), Error<'source>> {
            let image = args.next()?;
            let image_span = ctx.ast_expressions.get_span(image);
            let image = lowerer.expression(image, ctx)?;
            Ok((image, image_span))
        }

        let (image, image_span, gather) = match fun {
            Texture::Gather => {
                let image_or_component = args.next()?;
                let image_or_component_span = ctx.ast_expressions.get_span(image_or_component);
                // Gathers from depth textures don't take an initial `component` argument.
                let lowered_image_or_component = self.expression(image_or_component, ctx)?;

                match *resolve_inner!(ctx, lowered_image_or_component) {
                    crate::TypeInner::Image {
                        class: crate::ImageClass::Depth { .. },
                        ..
                    } => (
                        lowered_image_or_component,
                        image_or_component_span,
                        Some(crate::SwizzleComponent::X),
                    ),
                    _ => {
                        let (image, image_span) = get_image_and_span(self, &mut args, ctx)?;
                        (
                            image,
                            image_span,
                            Some(ctx.gather_component(
                                lowered_image_or_component,
                                image_or_component_span,
                                span,
                            )?),
                        )
                    }
                }
            }
            Texture::GatherCompare => {
                let (image, image_span) = get_image_and_span(self, &mut args, ctx)?;
                (image, image_span, Some(crate::SwizzleComponent::X))
            }

            _ => {
                let (image, image_span) = get_image_and_span(self, &mut args, ctx)?;
                (image, image_span, None)
            }
        };

        let sampler = self.expression(args.next()?, ctx)?;

        let coordinate = self.expression(args.next()?, ctx)?;

        let (_, arrayed) = ctx.image_data(image, image_span)?;
        let array_index = arrayed
            .then(|| self.expression(args.next()?, ctx))
            .transpose()?;

        let (level, depth_ref) = match fun {
            Texture::Gather => (crate::SampleLevel::Zero, None),
            Texture::GatherCompare => {
                let reference = self.expression(args.next()?, ctx)?;
                (crate::SampleLevel::Zero, Some(reference))
            }

            Texture::Sample => (crate::SampleLevel::Auto, None),
            Texture::SampleBias => {
                let bias = self.expression(args.next()?, ctx)?;
                (crate::SampleLevel::Bias(bias), None)
            }
            Texture::SampleCompare => {
                let reference = self.expression(args.next()?, ctx)?;
                (crate::SampleLevel::Auto, Some(reference))
            }
            Texture::SampleCompareLevel => {
                let reference = self.expression(args.next()?, ctx)?;
                (crate::SampleLevel::Zero, Some(reference))
            }
            Texture::SampleGrad => {
                let x = self.expression(args.next()?, ctx)?;
                let y = self.expression(args.next()?, ctx)?;
                (crate::SampleLevel::Gradient { x, y }, None)
            }
            Texture::SampleLevel => {
                let level = self.expression(args.next()?, ctx)?;
                (crate::SampleLevel::Exact(level), None)
            }
        };

        let offset = args
            .next()
            .map(|arg| self.expression(arg, &mut ctx.as_const()))
            .ok()
            .transpose()?;

        args.finish()?;

        Ok(crate::Expression::ImageSample {
            image,
            sampler,
            gather,
            coordinate,
            array_index,
            offset,
            level,
            depth_ref,
        })
    }

    fn r#struct(
        &mut self,
        s: &ast::Struct<'source>,
        span: Span,
        ctx: &mut GlobalContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Type>, Error<'source>> {
        let mut offset = 0;
        let mut struct_alignment = Alignment::ONE;
        let mut members = Vec::with_capacity(s.members.len());

        for member in s.members.iter() {
            let ty = self.resolve_ast_type(member.ty, ctx)?;

            self.layouter.update(ctx.module.to_ctx()).unwrap();

            let member_min_size = self.layouter[ty].size;
            let member_min_alignment = self.layouter[ty].alignment;

            let member_size = if let Some(size_expr) = member.size {
                let (size, span) = self.const_u32(size_expr, &mut ctx.as_const())?;
                if size < member_min_size {
                    return Err(Error::SizeAttributeTooLow(span, member_min_size));
                } else {
                    size
                }
            } else {
                member_min_size
            };

            let member_alignment = if let Some(align_expr) = member.align {
                let (align, span) = self.const_u32(align_expr, &mut ctx.as_const())?;
                if let Some(alignment) = Alignment::new(align) {
                    if alignment < member_min_alignment {
                        return Err(Error::AlignAttributeTooLow(span, member_min_alignment));
                    } else {
                        alignment
                    }
                } else {
                    return Err(Error::NonPowerOfTwoAlignAttribute(span));
                }
            } else {
                member_min_alignment
            };

            let binding = self.binding(&member.binding, ty, ctx)?;

            offset = member_alignment.round_up(offset);
            struct_alignment = struct_alignment.max(member_alignment);

            members.push(crate::StructMember {
                name: Some(member.name.name.to_owned()),
                ty,
                binding,
                offset,
            });

            offset += member_size;
        }

        let size = struct_alignment.round_up(offset);
        let inner = crate::TypeInner::Struct {
            members,
            span: size,
        };

        let handle = ctx.module.types.insert(
            crate::Type {
                name: Some(s.name.name.to_string()),
                inner,
            },
            span,
        );
        Ok(handle)
    }

    fn literal(
        &mut self,
        literal: ast::Literal,
        span: Span,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Typed<Handle<crate::Expression>>, Error<'source>> {
        use crate::Literal as Nl;
        use ast::Literal as Al;
        use Loaded as Lo;

        let literal = match literal {
            Al::Bool(b) => Lo::Concrete(Nl::Bool(b)),
            Al::Number(Number::AbstractInt(i)) => Lo::Abstract(Nl::I64(i)),
            Al::Number(Number::AbstractFloat(f)) => Lo::Abstract(Nl::F64(f)),
            Al::Number(Number::I32(n)) => Lo::Concrete(Nl::I32(n)),
            Al::Number(Number::U32(n)) => Lo::Concrete(Nl::U32(n)),
            Al::Number(Number::F32(n)) => Lo::Concrete(Nl::F32(n)),
        };

        literal
            .try_map(|literal| ctx.append_expression(crate::Expression::Literal(literal), span))
            .map(Typed::Loaded)
    }

    fn const_u32(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<(u32, Span), Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);
        let expr = self.expression(expr, ctx)?;
        let u32_res = Loaded::Concrete(
            TypeResolution::Value(
                crate::TypeInner::Scalar {
                    kind: crate::ScalarKind::Uint,
                    width: 4,
                }
            )
        );

        let converted = ctx.apply_automatic_conversions(expr, u32_res)?
            .ok_or(Error::ExpectedConstExprConcreteIntegerScalar(span))?;

        // Because the conversion to `u32_res` succeeded, we know
        // `converted` is `Concrete`.
        let converted = converted.handle();

        let gctx = ctx.as_globalctx();
        let value = gctx.eval_expr_to_u32(converted)
            .map_err(|err| match err {
                crate::proc::U32EvalError::NonConst => {
                    Error::ExpectedConstExprConcreteIntegerScalar(span)
                }
                crate::proc::U32EvalError::Negative => Error::ExpectedNonNegative(span),
            })?;

        Ok((value, span))
    }

    fn array_size(
        &mut self,
        size: ast::ArraySize<'source>,
        ctx: &mut GlobalContext<'source, '_, '_>,
    ) -> Result<crate::ArraySize, Error<'source>> {
        Ok(match size {
            ast::ArraySize::Constant(expr) => {
                let span = ctx.ast_expressions.get_span(expr);
                let const_expr = self.expression(expr, &mut ctx.as_const())?;
                let len =
                    ctx.module
                        .to_ctx()
                        .eval_expr_to_u32(const_expr)
                        .map_err(|err| match err {
                            crate::proc::U32EvalError::NonConst => {
                                Error::ExpectedConstExprConcreteIntegerScalar(span)
                            }
                            crate::proc::U32EvalError::Negative => {
                                Error::ExpectedPositiveArrayLength(span)
                            }
                        })?;
                let size = NonZeroU32::new(len).ok_or(Error::ExpectedPositiveArrayLength(span))?;
                crate::ArraySize::Constant(size)
            }
            ast::ArraySize::Dynamic => crate::ArraySize::Dynamic,
        })
    }

    /// Return a Naga `Handle<Type>` representing the front-end type `handle`.
    fn resolve_ast_type(
        &mut self,
        handle: Handle<ast::Type<'source>>,
        ctx: &mut GlobalContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Type>, Error<'source>> {
        let inner = match ctx.types[handle] {
            ast::Type::Scalar(scalar) => scalar.to_inner_scalar(),
            ast::Type::Vector { size, scalar } => scalar.to_inner_vector(size),
            ast::Type::Matrix {
                rows,
                columns,
                width,
            } => crate::TypeInner::Matrix {
                columns,
                rows,
                width,
            },
            ast::Type::Atomic(scalar) => scalar.to_inner_atomic(),
            ast::Type::Pointer { base, space } => {
                let base = self.resolve_ast_type(base, ctx)?;
                crate::TypeInner::Pointer { base, space }
            }
            ast::Type::Array { base, size } => {
                let base = self.resolve_ast_type(base, ctx)?;
                let size = self.array_size(size, ctx)?;

                self.layouter.update(ctx.module.to_ctx()).unwrap();
                let stride = self.layouter[base].to_stride();

                crate::TypeInner::Array { base, size, stride }
            }
            ast::Type::Image {
                dim,
                arrayed,
                class,
            } => crate::TypeInner::Image {
                dim,
                arrayed,
                class,
            },
            ast::Type::Sampler { comparison } => crate::TypeInner::Sampler { comparison },
            ast::Type::AccelerationStructure => crate::TypeInner::AccelerationStructure,
            ast::Type::RayQuery => crate::TypeInner::RayQuery,
            ast::Type::BindingArray { base, size } => {
                let base = self.resolve_ast_type(base, ctx)?;
                let size = self.array_size(size, ctx)?;
                crate::TypeInner::BindingArray { base, size }
            }
            ast::Type::RayDesc => {
                return Ok(ctx.module.generate_ray_desc_type());
            }
            ast::Type::RayIntersection => {
                return Ok(ctx.module.generate_ray_intersection_type());
            }
            ast::Type::User(ref ident) => {
                return match ctx.globals.get(ident.name) {
                    Some(&LoweredGlobalDecl::Type(handle)) => Ok(handle),
                    Some(_) => Err(Error::Unexpected(ident.span, ExpectedToken::Type)),
                    None => Err(Error::UnknownType(ident.span)),
                }
            }
        };

        Ok(ctx.ensure_type_exists(inner))
    }

    fn binding(
        &mut self,
        binding: &Option<ast::Binding<'source>>,
        ty: Handle<crate::Type>,
        ctx: &mut GlobalContext<'source, '_, '_>,
    ) -> Result<Option<crate::Binding>, Error<'source>> {
        Ok(match *binding {
            Some(ast::Binding::BuiltIn(b)) => Some(crate::Binding::BuiltIn(b)),
            Some(ast::Binding::Location {
                location,
                second_blend_source,
                interpolation,
                sampling,
            }) => {
                let mut binding = crate::Binding::Location {
                    location: self.const_u32(location, &mut ctx.as_const())?.0,
                    second_blend_source,
                    interpolation,
                    sampling,
                };
                binding.apply_default_interpolation(&ctx.module.types[ty].inner);
                Some(binding)
            }
            None => None,
        })
    }

    fn ray_query_pointer(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        ctx: &mut ExpressionContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);
        let pointer = self.expression(expr, ctx)?;

        match pointer {
            Loaded::Concrete(pointer) => match *resolve_inner!(ctx, pointer) {
                crate::TypeInner::Pointer { base, .. } => match ctx.module.types[base].inner {
                    crate::TypeInner::RayQuery => Ok(pointer),
                    ref other => {
                        log::error!("Pointer type to {:?} passed to ray query op", other);
                        Err(Error::InvalidRayQueryPointer(span))
                    }
                },
                ref other => {
                    log::error!("Type {:?} passed to ray query op", other);
                    Err(Error::InvalidRayQueryPointer(span))
                }
            },
            Loaded::Abstract(pointer) => {
                let inner = resolve_inner!(ctx, pointer);
                log::error!("Type {:?} passed to ray query op", inner);
                Err(Error::InvalidRayQueryPointer(span))
            }
        }
    }
}
