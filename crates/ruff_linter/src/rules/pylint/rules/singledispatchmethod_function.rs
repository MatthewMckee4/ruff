use ruff_macros::{ViolationMetadata, derive_message_formats};
use ruff_python_ast as ast;
use ruff_python_semantic::Scope;
use ruff_python_semantic::analyze::function_type;
use ruff_text_size::Ranged;

use crate::checkers::ast::Checker;
use crate::importer::ImportRequest;
use crate::{Edit, Fix, FixAvailability, Violation};

/// ## What it does
/// Checks for non-method functions decorated with `@singledispatchmethod`.
///
/// ## Why is this bad?
/// The `@singledispatchmethod` decorator is intended for use with methods, not
/// functions.
///
/// Instead, use the `@singledispatch` decorator.
///
/// ## Example
///
/// ```python
/// from functools import singledispatchmethod
///
///
/// @singledispatchmethod
/// def func(arg): ...
/// ```
///
/// Use instead:
///
/// ```python
/// from functools import singledispatch
///
///
/// @singledispatch
/// def func(arg): ...
/// ```
///
/// ## Fix safety
/// This rule's fix is marked as unsafe, as migrating from `@singledispatchmethod` to
/// `@singledispatch` may change the behavior of the code.
#[derive(ViolationMetadata)]
pub(crate) struct SingledispatchmethodFunction;

impl Violation for SingledispatchmethodFunction {
    const FIX_AVAILABILITY: FixAvailability = FixAvailability::Sometimes;

    #[derive_message_formats]
    fn message(&self) -> String {
        "`@singledispatchmethod` decorator should not be used on non-method functions".to_string()
    }

    fn fix_title(&self) -> Option<String> {
        Some("Replace with `@singledispatch`".to_string())
    }
}

/// PLE1520
pub(crate) fn singledispatchmethod_function(checker: &Checker, scope: &Scope) {
    let Some(func) = scope.kind.as_function() else {
        return;
    };

    let ast::StmtFunctionDef {
        name,
        decorator_list,
        ..
    } = func;

    let Some(parent) = checker.semantic().first_non_type_parent_scope(scope) else {
        return;
    };

    let type_ = function_type::classify(
        name,
        decorator_list,
        parent,
        checker.semantic(),
        &checker.settings().pep8_naming.classmethod_decorators,
        &checker.settings().pep8_naming.staticmethod_decorators,
    );
    if !matches!(type_, function_type::FunctionType::Function) {
        return;
    }

    for decorator in decorator_list {
        if checker
            .semantic()
            .resolve_qualified_name(&decorator.expression)
            .is_some_and(|qualified_name| {
                matches!(
                    qualified_name.segments(),
                    ["functools", "singledispatchmethod"]
                )
            })
        {
            let mut diagnostic =
                checker.report_diagnostic(SingledispatchmethodFunction, decorator.range());
            diagnostic.try_set_fix(|| {
                let (import_edit, binding) = checker.importer().get_or_import_symbol(
                    &ImportRequest::import("functools", "singledispatch"),
                    decorator.start(),
                    checker.semantic(),
                )?;
                Ok(Fix::unsafe_edits(
                    Edit::range_replacement(binding, decorator.expression.range()),
                    [import_edit],
                ))
            });
        }
    }
}
