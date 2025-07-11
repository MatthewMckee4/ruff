use ruff_python_ast::Alias;

use ruff_macros::{ViolationMetadata, derive_message_formats};
use ruff_python_stdlib::future::is_feature_name;
use ruff_text_size::Ranged;

use crate::Violation;
use crate::checkers::ast::Checker;

/// ## What it does
/// Checks for `__future__` imports that are not defined in the current Python
/// version.
///
/// ## Why is this bad?
/// Importing undefined or unsupported members from the `__future__` module is
/// a `SyntaxError`.
///
/// ## References
/// - [Python documentation: `__future__`](https://docs.python.org/3/library/__future__.html)
#[derive(ViolationMetadata)]
pub(crate) struct FutureFeatureNotDefined {
    name: String,
}

impl Violation for FutureFeatureNotDefined {
    #[derive_message_formats]
    fn message(&self) -> String {
        let FutureFeatureNotDefined { name } = self;
        format!("Future feature `{name}` is not defined")
    }
}

/// F407
pub(crate) fn future_feature_not_defined(checker: &Checker, alias: &Alias) {
    if is_feature_name(&alias.name) {
        return;
    }

    checker.report_diagnostic(
        FutureFeatureNotDefined {
            name: alias.name.to_string(),
        },
        alias.range(),
    );
}
