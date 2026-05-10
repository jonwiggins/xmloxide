//! W3C Canonical XML conformance test harness.
//!
//! Drives `canonicalize` / `canonicalize_subtree` against the worked examples
//! published in the C14N 1.0 and Exclusive C14N 1.0 specifications. Each
//! input is loaded from `tests/data/c14n_w3c/`, canonicalized in the
//! requested mode, and compared byte-for-byte to the expected output.
//!
//! Sources:
//! - C14N 1.0: <https://www.w3.org/TR/xml-c14n/> §3 ("Examples")
//! - Exclusive C14N 1.0: <https://www.w3.org/TR/xml-exc-c14n/> §2
//!
//! Some published examples require infrastructure outside the c14n module
//! (XPath-defined node-sets, DTD-driven attribute normalization, external
//! entity resolution). Those subsections are not represented here.
//!
//! On byte mismatch the harness reports the first differing byte plus a
//! 200-byte window from each side. Trailing newlines are normalized away
//! because file-save tooling routinely adds them; the spec's canonical
//! forms have no trailing newline.

use xmloxide::serial::c14n::{canonicalize, canonicalize_subtree, C14nOptions};
use xmloxide::tree::{Document, NodeId};

#[derive(Debug, Clone, Copy)]
enum Apex {
    /// Whole-document canonicalization.
    Document,
    /// Subtree rooted at the first element with the given local name.
    SubtreeAt(&'static str),
}

#[derive(Debug)]
struct C14nCase {
    name: &'static str,
    input: &'static str,
    expected: &'static str,
    apex: Apex,
    with_comments: bool,
    exclusive: bool,
    inclusive_prefixes: &'static [&'static str],
    /// Provenance for failure reports.
    source: &'static str,
}

const CASES: &[C14nCase] = &[
    // -------- W3C Canonical XML 1.0 --------
    C14nCase {
        name: "c14n10_3_1_no_comments",
        input: "c14n10_3_1_input.xml",
        expected: "c14n10_3_1_no_comments.xml",
        apex: Apex::Document,
        with_comments: false,
        exclusive: false,
        inclusive_prefixes: &[],
        source: "C14N 1.0 §3.1 (without comments)",
    },
    C14nCase {
        name: "c14n10_3_1_with_comments",
        input: "c14n10_3_1_input.xml",
        expected: "c14n10_3_1_with_comments.xml",
        apex: Apex::Document,
        with_comments: true,
        exclusive: false,
        inclusive_prefixes: &[],
        source: "C14N 1.0 §3.1 (with comments)",
    },
    C14nCase {
        name: "c14n10_3_2_whitespace",
        input: "c14n10_3_2_input.xml",
        expected: "c14n10_3_2_expected.xml",
        apex: Apex::Document,
        with_comments: false,
        exclusive: false,
        inclusive_prefixes: &[],
        source: "C14N 1.0 §3.2 (whitespace in document content)",
    },
    C14nCase {
        // §3.3 exercises start/end tags, attribute sorting, and namespace
        // handling. It also relies on a DTD `<!ATTLIST e9 attr CDATA "default">`
        // which adds an attribute via DTD defaulting. xmloxide's parser may or
        // may not apply that — if it doesn't, this test surfaces it as a
        // missing `attr="default"`.
        name: "c14n10_3_3_tags",
        input: "c14n10_3_3_input.xml",
        expected: "c14n10_3_3_expected.xml",
        apex: Apex::Document,
        with_comments: false,
        exclusive: false,
        inclusive_prefixes: &[],
        source: "C14N 1.0 §3.3 (start/end tags)",
    },
    C14nCase {
        name: "c14n10_3_6_utf8",
        input: "c14n10_3_6_input.xml",
        expected: "c14n10_3_6_expected.xml",
        apex: Apex::Document,
        with_comments: false,
        exclusive: false,
        inclusive_prefixes: &[],
        source: "C14N 1.0 §3.6 (UTF-8 encoding)",
    },
    // -------- W3C Exclusive XML Canonicalization 1.0 --------
    C14nCase {
        name: "excc14n_2_1_standalone",
        input: "excc14n_2_1_standalone_input.xml",
        expected: "excc14n_2_1_standalone_expected.xml",
        apex: Apex::Document,
        with_comments: false,
        exclusive: true,
        inclusive_prefixes: &[],
        source: "Exc-C14N 1.0 §2.1 (standalone)",
    },
    C14nCase {
        // §2.1 enveloped: extracting elem1 from a pdu apex must yield the
        // same canonical form as the standalone case. Subtree extraction.
        name: "excc14n_2_1_enveloped",
        input: "excc14n_2_1_enveloped_input.xml",
        expected: "excc14n_2_1_enveloped_expected.xml",
        apex: Apex::SubtreeAt("elem1"),
        with_comments: false,
        exclusive: true,
        inclusive_prefixes: &[],
        source: "Exc-C14N 1.0 §2.1 (enveloped, subtree at elem1)",
    },
    C14nCase {
        // §2.2 input1: elem2 subtree under different enveloping context.
        name: "excc14n_2_2_input1",
        input: "excc14n_2_2_input1.xml",
        expected: "excc14n_2_2_expected.xml",
        apex: Apex::SubtreeAt("elem2"),
        with_comments: false,
        exclusive: true,
        inclusive_prefixes: &[],
        source: "Exc-C14N 1.0 §2.2 (input1, subtree at elem2)",
    },
    C14nCase {
        // §2.2 input2: same elem2 subtree under a different enveloping
        // context. Spec property: byte-equal output to input1's elem2.
        name: "excc14n_2_2_input2",
        input: "excc14n_2_2_input2.xml",
        expected: "excc14n_2_2_expected.xml",
        apex: Apex::SubtreeAt("elem2"),
        with_comments: false,
        exclusive: true,
        inclusive_prefixes: &[],
        source: "Exc-C14N 1.0 §2.2 (input2, subtree at elem2)",
    },
];

fn fixture_path(name: &str) -> String {
    format!("{}/tests/data/c14n_w3c/{name}", env!("CARGO_MANIFEST_DIR"))
}

fn find_first_local(doc: &Document, root: NodeId, local: &str) -> Option<NodeId> {
    if doc.node_name(root) == Some(local) {
        return Some(root);
    }
    for child in doc.children(root) {
        if let Some(n) = find_first_local(doc, child, local) {
            return Some(n);
        }
    }
    None
}

/// Strip a single trailing newline if present. C14N's spec output has no
/// trailing newline; file-save tooling routinely adds one. Normalizing both
/// sides means we can keep readable fixture files without false positives.
fn trim_trailing_newline(s: &str) -> &str {
    s.strip_suffix('\n').unwrap_or(s)
}

fn first_diff_window(expected: &str, actual: &str) -> String {
    let exp = expected.as_bytes();
    let act = actual.as_bytes();
    let mut at = 0usize;
    while at < exp.len() && at < act.len() && exp[at] == act[at] {
        at += 1;
    }
    let pre = at.saturating_sub(80);
    let exp_end = (at + 80).min(exp.len());
    let act_end = (at + 80).min(act.len());
    format!(
        "  first diff at byte {at}\n  expected window: {:?}\n  actual   window: {:?}",
        String::from_utf8_lossy(&exp[pre..exp_end]),
        String::from_utf8_lossy(&act[pre..act_end]),
    )
}

#[test]
fn w3c_c14n_conformance() {
    let mut failures: Vec<String> = Vec::new();

    for case in CASES {
        let input_path = fixture_path(case.input);
        let expected_path = fixture_path(case.expected);

        let input = match std::fs::read_to_string(&input_path) {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("[{}] read {input_path}: {e}", case.name));
                continue;
            }
        };
        let expected = match std::fs::read_to_string(&expected_path) {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("[{}] read {expected_path}: {e}", case.name));
                continue;
            }
        };

        let doc = match Document::parse_str(&input) {
            Ok(d) => d,
            Err(e) => {
                failures.push(format!(
                    "[{}] parse failed: {e:?}\n  source: {}",
                    case.name, case.source
                ));
                continue;
            }
        };

        let opts = C14nOptions {
            with_comments: case.with_comments,
            exclusive: case.exclusive,
            inclusive_prefixes: case
                .inclusive_prefixes
                .iter()
                .map(|s| (*s).to_string())
                .collect(),
        };

        let actual = match case.apex {
            Apex::Document => canonicalize(&doc, &opts),
            Apex::SubtreeAt(local) => {
                let Some(root) = doc.root_element() else {
                    failures.push(format!("[{}] no root element", case.name));
                    continue;
                };
                let Some(apex) = find_first_local(&doc, root, local) else {
                    failures.push(format!(
                        "[{}] subtree apex <{local}> not found in input",
                        case.name
                    ));
                    continue;
                };
                canonicalize_subtree(&doc, apex, &opts)
            }
        };

        let exp_trim = trim_trailing_newline(&expected);
        let act_trim = trim_trailing_newline(&actual);

        if exp_trim != act_trim {
            failures.push(format!(
                "[{}] BYTE MISMATCH ({} bytes expected, {} bytes actual)\n  source: {}\n{}",
                case.name,
                exp_trim.len(),
                act_trim.len(),
                case.source,
                first_diff_window(exp_trim, act_trim),
            ));
        }
    }

    if !failures.is_empty() {
        let total = CASES.len();
        let failed = failures.len();
        panic!(
            "W3C C14N conformance: {} / {} cases failed\n\n{}\n",
            failed,
            total,
            failures.join("\n\n"),
        );
    }
}

/// Idempotency roundtrip: feeding canonical output back through the parser
/// + canonicalizer must produce byte-identical output. This is the formal
///   statement of "the canonical form is canonical" — a c14n implementation
///   that doesn't satisfy it is by definition non-canonical.
///
/// Runs against every case in `CASES`. If a fixture's expected output
/// re-canonicalizes to anything other than itself, the test fails with
/// the same byte-window diff used by the conformance test above.
#[test]
fn w3c_c14n_idempotency() {
    let mut failures: Vec<String> = Vec::new();

    for case in CASES {
        // The "expected" file already represents canonical output for this
        // case's mode; feeding it back through must reproduce it exactly.
        let Ok(canonical_input) = std::fs::read_to_string(fixture_path(case.expected)) else {
            continue;
        };
        let canonical_input = trim_trailing_newline(&canonical_input).to_string();

        let doc = match Document::parse_str(&canonical_input) {
            Ok(d) => d,
            Err(e) => {
                failures.push(format!(
                    "[{}] re-parse of canonical output failed: {e:?}",
                    case.name,
                ));
                continue;
            }
        };

        let opts = C14nOptions {
            with_comments: case.with_comments,
            exclusive: case.exclusive,
            inclusive_prefixes: case
                .inclusive_prefixes
                .iter()
                .map(|s| (*s).to_string())
                .collect(),
        };

        // Re-canonicalize using the same apex selection as the original
        // case so subtree-apex tests stay well-defined.
        let actual = match case.apex {
            Apex::Document => canonicalize(&doc, &opts),
            Apex::SubtreeAt(local) => {
                let Some(root) = doc.root_element() else {
                    continue;
                };
                let Some(apex) = find_first_local(&doc, root, local) else {
                    // The canonical-output file may have stripped enveloping
                    // ancestors, so the original apex element is the new
                    // root. Fall back to canonicalizing the whole doc.
                    let actual = canonicalize(&doc, &opts);
                    let act_trim = trim_trailing_newline(&actual);
                    if act_trim != canonical_input {
                        failures.push(format!(
                            "[{}] subtree-apex idempotency failed\n  source: {}\n{}",
                            case.name,
                            case.source,
                            first_diff_window(&canonical_input, act_trim),
                        ));
                    }
                    continue;
                };
                canonicalize_subtree(&doc, apex, &opts)
            }
        };

        let act_trim = trim_trailing_newline(&actual);
        if act_trim != canonical_input {
            failures.push(format!(
                "[{}] re-canonicalize differs from input\n  source: {}\n{}",
                case.name,
                case.source,
                first_diff_window(&canonical_input, act_trim),
            ));
        }
    }

    assert!(
        failures.is_empty(),
        "C14N idempotency: {} / {} cases failed\n\n{}\n",
        failures.len(),
        CASES.len(),
        failures.join("\n\n"),
    );
}
