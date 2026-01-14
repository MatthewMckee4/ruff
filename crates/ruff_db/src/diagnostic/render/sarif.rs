use std::fmt;

use ruff_source_file::OneIndexed;
use serde::{Serialize, Serializer};

use crate::diagnostic::{Diagnostic, Severity};

use super::FileResolver;

/// Renderer for producing SARIF 2.1.0-compliant JSON output.
///
/// Static Analysis Results Interchange Format (SARIF) is a standard format
/// for static analysis results. For full specification, see:
/// [SARIF 2.1.0](https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html)
pub struct SarifRenderer<'a> {
    resolver: &'a dyn FileResolver,
    tool_name: &'static str,
    tool_information_uri: &'static str,
    tool_version: &'static str,
}

impl<'a> SarifRenderer<'a> {
    pub fn new(
        resolver: &'a dyn FileResolver,
        tool_name: &'static str,
        tool_information_uri: &'static str,
        tool_version: &'static str,
    ) -> Self {
        Self {
            resolver,
            tool_name,
            tool_information_uri,
            tool_version,
        }
    }
}

impl SarifRenderer<'_> {
    pub fn render(&self, f: &mut fmt::Formatter, diagnostics: &[Diagnostic]) -> fmt::Result {
        write!(
            f,
            "{}",
            serde_json::to_string_pretty(&SarifLog {
                diagnostics,
                resolver: self.resolver,
                tool_name: self.tool_name,
                tool_information_uri: self.tool_information_uri,
                tool_version: self.tool_version,
            })
            .unwrap()
        )
    }
}

struct SarifLog<'a> {
    diagnostics: &'a [Diagnostic],
    resolver: &'a dyn FileResolver,
    tool_name: &'static str,
    tool_information_uri: &'static str,
    tool_version: &'static str,
}

impl Serialize for SarifLog<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::SerializeMap;

        let results: Vec<SarifResult> = self
            .diagnostics
            .iter()
            .map(|d| SarifResult::from_diagnostic(d, self.resolver))
            .collect();

        // Collect unique rules from the diagnostics (keyed by rule id for uniqueness and ordering)
        let rules: Vec<SarifRule> = self
            .diagnostics
            .iter()
            .map(|d| (d.secondary_code_or_id().to_string(), SarifRule::from(d)))
            .collect::<std::collections::BTreeMap<_, _>>()
            .into_values()
            .collect();

        let mut map = serializer.serialize_map(Some(3))?;
        map.serialize_entry("$schema", "https://json.schemastore.org/sarif-2.1.0.json")?;
        map.serialize_entry("version", "2.1.0")?;
        map.serialize_entry(
            "runs",
            &[SarifRun {
                tool: SarifTool {
                    driver: SarifDriver {
                        name: self.tool_name,
                        information_uri: self.tool_information_uri,
                        version: self.tool_version,
                        rules,
                    },
                },
                results,
            }],
        )?;
        map.end()
    }
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct SarifRun<'a> {
    tool: SarifTool<'a>,
    results: Vec<SarifResult>,
}

#[derive(Serialize)]
struct SarifTool<'a> {
    driver: SarifDriver<'a>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct SarifDriver<'a> {
    name: &'a str,
    #[serde(rename = "informationUri")]
    information_uri: &'a str,
    version: &'a str,
    rules: Vec<SarifRule>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct SarifRule {
    id: String,
    short_description: SarifMessageOwned,
    #[serde(skip_serializing_if = "Option::is_none")]
    help_uri: Option<String>,
    properties: SarifRuleProperties,
}

impl From<&Diagnostic> for SarifRule {
    fn from(diagnostic: &Diagnostic) -> Self {
        let id = diagnostic.secondary_code_or_id().to_string();
        let severity = match diagnostic.severity() {
            Severity::Info => "note",
            Severity::Warning => "warning",
            Severity::Error => "error",
            Severity::Fatal => "error",
        };

        Self {
            id: id.clone(),
            short_description: SarifMessageOwned {
                text: diagnostic.concise_message().to_string(),
            },
            help_uri: diagnostic.documentation_url().map(ToString::to_string),
            properties: SarifRuleProperties {
                id,
                name: diagnostic.name().to_string(),
                problem_severity: severity,
            },
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct SarifRuleProperties {
    id: String,
    name: String,
    #[serde(rename = "problem.severity")]
    problem_severity: &'static str,
}

/// Represents a single result in a SARIF 2.1.0 report.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SarifResult {
    rule_id: String,
    level: &'static str,
    message: SarifMessageOwned,
    locations: Vec<SarifLocation>,
}

#[derive(Debug, Clone, Serialize)]
struct SarifMessageOwned {
    text: String,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SarifLocation {
    physical_location: SarifPhysicalLocation,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SarifPhysicalLocation {
    artifact_location: SarifArtifactLocation,
    region: SarifRegion,
}

#[derive(Debug, Serialize)]
struct SarifArtifactLocation {
    uri: String,
}

#[derive(Debug, Serialize, Clone, Copy)]
#[serde(rename_all = "camelCase")]
struct SarifRegion {
    start_line: OneIndexed,
    start_column: OneIndexed,
    end_line: OneIndexed,
    end_column: OneIndexed,
}

impl SarifResult {
    fn from_diagnostic(diagnostic: &Diagnostic, resolver: &dyn FileResolver) -> Self {
        let level = match diagnostic.severity() {
            Severity::Info => "note",
            Severity::Warning => "warning",
            Severity::Error | Severity::Fatal => "error",
        };

        let (uri, region) = diagnostic
            .primary_span()
            .map(|span| {
                let file = span.file();
                let path = file.relative_path(resolver);

                // Convert path to URI format
                let uri = format!("file://{}", path.display());

                let region = if resolver.is_notebook(file) {
                    // For notebooks, use default positions
                    SarifRegion {
                        start_line: OneIndexed::MIN,
                        start_column: OneIndexed::MIN,
                        end_line: OneIndexed::MIN,
                        end_column: OneIndexed::MIN,
                    }
                } else {
                    let diagnostic_source = file.diagnostic_source(resolver);
                    let source_code = diagnostic_source.as_source_code();

                    span.range()
                        .map(|range| {
                            let start = source_code.line_column(range.start());
                            let end = source_code.line_column(range.end());
                            SarifRegion {
                                start_line: start.line,
                                start_column: start.column,
                                end_line: end.line,
                                end_column: end.column,
                            }
                        })
                        .unwrap_or(SarifRegion {
                            start_line: OneIndexed::MIN,
                            start_column: OneIndexed::MIN,
                            end_line: OneIndexed::MIN,
                            end_column: OneIndexed::MIN,
                        })
                };

                (uri, region)
            })
            .unwrap_or_else(|| {
                (
                    String::new(),
                    SarifRegion {
                        start_line: OneIndexed::MIN,
                        start_column: OneIndexed::MIN,
                        end_line: OneIndexed::MIN,
                        end_column: OneIndexed::MIN,
                    },
                )
            });

        Self {
            rule_id: diagnostic.secondary_code_or_id().to_string(),
            level,
            message: SarifMessageOwned {
                text: diagnostic.concise_message().to_string(),
            },
            locations: vec![SarifLocation {
                physical_location: SarifPhysicalLocation {
                    artifact_location: SarifArtifactLocation { uri },
                    region,
                },
            }],
        }
    }
}

/// A type that implements `Display` for rendering SARIF diagnostics.
pub struct DisplaySarifDiagnostics<'a> {
    renderer: &'a SarifRenderer<'a>,
    diagnostics: &'a [Diagnostic],
}

impl<'a> DisplaySarifDiagnostics<'a> {
    pub fn new(renderer: &'a SarifRenderer<'a>, diagnostics: &'a [Diagnostic]) -> Self {
        Self {
            renderer,
            diagnostics,
        }
    }
}

impl std::fmt::Display for DisplaySarifDiagnostics<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.renderer.render(f, self.diagnostics)
    }
}

#[cfg(test)]
mod tests {
    use crate::diagnostic::{
        DiagnosticFormat,
        render::tests::{create_diagnostics, create_syntax_error_diagnostics},
    };

    #[test]
    fn output() {
        let (env, diagnostics) = create_diagnostics(DiagnosticFormat::Sarif);
        insta::assert_snapshot!(env.render_diagnostics(&diagnostics));
    }

    #[test]
    fn syntax_errors() {
        let (env, diagnostics) = create_syntax_error_diagnostics(DiagnosticFormat::Sarif);
        insta::assert_snapshot!(env.render_diagnostics(&diagnostics));
    }
}
