use ruff_macros::{ViolationMetadata, derive_message_formats};
use ruff_python_ast::{self as ast};
use ruff_text_size::Ranged;

use crate::Violation;
use crate::checkers::ast::Checker;

/// ## What it does
/// Checks for uses of the `mako` templates.
///
/// ## Why is this bad?
/// Mako templates allow HTML and JavaScript rendering by default, and are
/// inherently open to XSS attacks. Ensure variables in all templates are
/// properly sanitized via the `n`, `h` or `x` flags (depending on context).
/// For example, to HTML escape the variable `data`, use `${ data |h }`.
///
/// ## Example
/// ```python
/// from mako.template import Template
///
/// Template("hello")
/// ```
///
/// Use instead:
/// ```python
/// from mako.template import Template
///
/// Template("hello |h")
/// ```
///
/// ## References
/// - [Mako documentation](https://www.makotemplates.org/)
/// - [OpenStack security: Cross site scripting XSS](https://security.openstack.org/guidelines/dg_cross-site-scripting-xss.html)
/// - [Common Weakness Enumeration: CWE-80](https://cwe.mitre.org/data/definitions/80.html)
#[derive(ViolationMetadata)]
pub(crate) struct MakoTemplates;

impl Violation for MakoTemplates {
    #[derive_message_formats]
    fn message(&self) -> String {
        "Mako templates allow HTML and JavaScript rendering by default and are inherently open to XSS attacks".to_string()
    }
}

/// S702
pub(crate) fn mako_templates(checker: &Checker, call: &ast::ExprCall) {
    if checker
        .semantic()
        .resolve_qualified_name(&call.func)
        .is_some_and(|qualified_name| {
            matches!(qualified_name.segments(), ["mako", "template", "Template"])
        })
    {
        checker.report_diagnostic(MakoTemplates, call.func.range());
    }
}
