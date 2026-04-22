//! html5lib-tests tree construction conformance suite.
//!
//! Parses `.dat` test files from the html5lib-tests repository and runs them
//! against the xmloxide HTML5 tree builder.
//!
//! To download the test suite, run:
//! ```sh
//! ./scripts/download-html5lib-tests.sh
//! ```
//!
//! Then run these tests with:
//! ```sh
//! cargo test --test html5lib_tree_construction -- --nocapture
//! ```

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fmt::Write as _;
use std::path::Path;

use xmloxide::html5::{parse_html5_with_options, Html5ParseOptions};
use xmloxide::tree::{Document, NodeId, NodeKind};

const SUITE_DIR: &str = "tests/html5lib-tests/tree-construction";

// -------------------------------------------------------------------------
// Test case parsing
// -------------------------------------------------------------------------

#[derive(Debug)]
#[allow(dead_code)]
struct TreeTest {
    data: String,
    expected_errors: usize,
    expected_document: String,
    fragment_context: Option<String>,
    script_on: Option<bool>,
}

fn parse_test_file(content: &str) -> Vec<TreeTest> {
    let mut tests = Vec::new();
    let lines: Vec<&str> = content.split('\n').collect();

    let mut i = 0;
    while i < lines.len() {
        // Find the start of the next test block
        if lines[i] != "#data" {
            i += 1;
            continue;
        }

        let mut data = String::new();
        let mut errors = 0;
        let mut new_errors = 0;
        let mut document = String::new();
        let mut fragment_context: Option<String> = None;
        let mut script_on: Option<bool> = None;
        let mut section = "data";
        i += 1; // skip "#data" line

        while i < lines.len() {
            let line = lines[i];
            match line {
                "#data" => {
                    // Start of next test — don't consume this line
                    break;
                }
                "#errors" => {
                    section = "errors";
                    i += 1;
                    continue;
                }
                "#new-errors" => {
                    section = "new-errors";
                    i += 1;
                    continue;
                }
                "#document-fragment" => {
                    section = "fragment";
                    i += 1;
                    continue;
                }
                "#document" => {
                    section = "document";
                    i += 1;
                    continue;
                }
                "#script-on" => {
                    script_on = Some(true);
                    i += 1;
                    continue;
                }
                "#script-off" => {
                    script_on = Some(false);
                    i += 1;
                    continue;
                }
                _ => {}
            }

            match section {
                "data" => {
                    if !data.is_empty() {
                        data.push('\n');
                    }
                    data.push_str(line);
                }
                "errors" if !line.is_empty() => {
                    errors += 1;
                }
                "new-errors" if !line.is_empty() => {
                    new_errors += 1;
                }
                "fragment" => {
                    fragment_context = Some(line.to_string());
                }
                "document" => {
                    if !document.is_empty() {
                        document.push('\n');
                    }
                    document.push_str(line);
                }
                _ => {}
            }
            i += 1;
        }

        if !document.is_empty() {
            // Trim trailing blank lines from the document (test separator)
            let document = document.trim_end_matches('\n').to_string();
            tests.push(TreeTest {
                data,
                expected_errors: errors + new_errors,
                expected_document: document,
                fragment_context,
                script_on,
            });
        }
    }

    tests
}

// -------------------------------------------------------------------------
// Tree serialization (html5lib-tests format)
// -------------------------------------------------------------------------

fn serialize_tree(doc: &Document) -> String {
    let mut result = String::new();
    let root = doc.root();

    let mut child = doc.node(root).first_child;
    while let Some(child_id) = child {
        serialize_node(doc, child_id, 0, &mut result);
        child = doc.node(child_id).next_sibling;
    }

    result
}

/// Serialize the fragment result: children of the root html element.
/// For foreign contexts, the context element is flattened (its children
/// are serialized instead of the element itself).
fn serialize_fragment(doc: &Document, context: &str) -> String {
    let mut result = String::new();
    let root = doc.root();

    // Find the <html> element (first child of document root)
    let Some(html_id) = doc.node(root).first_child else {
        return result;
    };

    let is_foreign = context.starts_with("svg ") || context.starts_with("math ");

    if is_foreign {
        // Foreign context: the context element is a child of html.
        // Flatten it by serializing its children.
        let ctx_name = context
            .strip_prefix("svg ")
            .or_else(|| context.strip_prefix("math "))
            .unwrap_or(context);
        let mut child = doc.node(html_id).first_child;
        while let Some(child_id) = child {
            if let NodeKind::Element { ref name, .. } = doc.node(child_id).kind {
                if name == ctx_name {
                    // Flatten: serialize children of the context element
                    let mut ctx_child = doc.node(child_id).first_child;
                    while let Some(cc_id) = ctx_child {
                        serialize_node(doc, cc_id, 0, &mut result);
                        ctx_child = doc.node(cc_id).next_sibling;
                    }
                    child = doc.node(child_id).next_sibling;
                    continue;
                }
            }
            serialize_node(doc, child_id, 0, &mut result);
            child = doc.node(child_id).next_sibling;
        }
    } else {
        // HTML context: serialize all children of html.
        let mut child = doc.node(html_id).first_child;
        while let Some(child_id) = child {
            serialize_node(doc, child_id, 0, &mut result);
            child = doc.node(child_id).next_sibling;
        }
    }

    result
}

fn serialize_node(doc: &Document, id: NodeId, depth: usize, out: &mut String) {
    let node = doc.node(id);

    match &node.kind {
        NodeKind::Element {
            name,
            namespace,
            attributes,
            ..
        } => {
            let ns_prefix = match namespace.as_deref() {
                Some("http://www.w3.org/2000/svg") => "svg ",
                Some("http://www.w3.org/1998/Math/MathML") => "math ",
                _ => "",
            };
            write_indent(out, depth);
            let _ = writeln!(out, "<{ns_prefix}{name}>");

            let mut sorted_attrs: Vec<(String, &str)> = attributes
                .iter()
                .map(|a| {
                    let attr_name = match a.prefix.as_deref() {
                        Some("xlink") => format!("xlink {}", a.name),
                        Some("xml") => format!("xml {}", a.name),
                        Some("xmlns") => format!("xmlns {}", a.name),
                        _ => a.name.clone(),
                    };
                    (attr_name, a.value.as_str())
                })
                .collect();
            sorted_attrs.sort_by(|a, b| a.0.cmp(&b.0));

            for (attr_name, attr_value) in &sorted_attrs {
                write_indent(out, depth + 1);
                let _ = writeln!(out, "{attr_name}=\"{attr_value}\"");
            }

            if name == "template" && namespace.is_none() {
                write_indent(out, depth + 1);
                let _ = writeln!(out, "content");
                let mut child = node.first_child;
                while let Some(child_id) = child {
                    serialize_node(doc, child_id, depth + 2, out);
                    child = doc.node(child_id).next_sibling;
                }
            } else {
                let mut child = node.first_child;
                while let Some(child_id) = child {
                    serialize_node(doc, child_id, depth + 1, out);
                    child = doc.node(child_id).next_sibling;
                }
            }
        }
        NodeKind::Text { content } => {
            write_indent(out, depth);
            let _ = writeln!(out, "\"{content}\"");
        }
        NodeKind::Comment { content } => {
            write_indent(out, depth);
            let _ = writeln!(out, "<!-- {content} -->");
        }
        NodeKind::DocumentType {
            name,
            public_id,
            system_id,
            ..
        } => {
            write_indent(out, depth);
            if public_id.is_some() || system_id.is_some() {
                let pub_str = public_id.as_deref().unwrap_or("");
                let sys_str = system_id.as_deref().unwrap_or("");
                let _ = writeln!(out, "<!DOCTYPE {name} \"{pub_str}\" \"{sys_str}\">");
            } else {
                let _ = writeln!(out, "<!DOCTYPE {name}>");
            }
        }
        NodeKind::ProcessingInstruction { target, data } => {
            write_indent(out, depth);
            let d = data.as_deref().unwrap_or("");
            let _ = writeln!(out, "<?{target} {d}>");
        }
        _ => {
            let mut child = node.first_child;
            while let Some(child_id) = child {
                serialize_node(doc, child_id, depth + 1, out);
                child = doc.node(child_id).next_sibling;
            }
        }
    }
}

fn write_indent(out: &mut String, depth: usize) {
    out.push_str("| ");
    for _ in 0..depth {
        out.push_str("  ");
    }
}

// -------------------------------------------------------------------------
// Main test runner
// -------------------------------------------------------------------------

struct TestResults {
    total: u32,
    passed: u32,
    failed: u32,
    skipped: u32,
}

#[test]
fn html5lib_tree_construction_suite() {
    let suite_dir = Path::new(SUITE_DIR);
    if !suite_dir.exists() {
        eprintln!(
            "Skipping html5lib tree construction tests: suite not found at {SUITE_DIR}.\n\
             Run ./scripts/download-html5lib-tests.sh to download."
        );
        return;
    }

    let mut results = TestResults {
        total: 0,
        passed: 0,
        failed: 0,
        skipped: 0,
    };

    let mut test_files: Vec<_> = std::fs::read_dir(suite_dir)
        .unwrap()
        .filter_map(std::result::Result::ok)
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "dat"))
        .map(|e| e.path())
        .collect();
    test_files.sort();

    for path in &test_files {
        let content = std::fs::read_to_string(path).unwrap_or_else(|_| {
            let bytes = std::fs::read(path).unwrap();
            String::from_utf8_lossy(&bytes).to_string()
        });
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let tests = parse_test_file(&content);

        for (i, test) in tests.iter().enumerate() {
            results.total += 1;

            let scripting = test.script_on.unwrap_or(false);

            let options = Html5ParseOptions {
                scripting,
                fragment_context: test.fragment_context.clone(),
            };
            let doc = match parse_html5_with_options(&test.data, &options) {
                Ok(d) => d,
                Err(e) => {
                    results.failed += 1;
                    eprintln!("FAIL [{file_name}#{i}] parse error: {e}");
                    eprintln!("  input: {:?}", test.data);
                    continue;
                }
            };

            let actual = if let Some(ref ctx) = test.fragment_context {
                serialize_fragment(&doc, ctx)
            } else {
                serialize_tree(&doc)
            };
            let actual_trimmed = actual.trim_end();
            let expected_trimmed = test.expected_document.trim_end();

            if actual_trimmed == expected_trimmed {
                results.passed += 1;
            } else {
                results.failed += 1;
                eprintln!("FAIL [{file_name}#{i}]");
                eprintln!("  input:    {:?}", test.data);
                eprintln!("  expected:\n{expected_trimmed}");
                eprintln!("  actual:\n{actual_trimmed}");
            }
        }
    }

    eprintln!();
    eprintln!("html5lib tree construction results:");
    eprintln!(
        "  {}/{} passed ({} failed, {} skipped)",
        results.passed, results.total, results.failed, results.skipped
    );

    let pass_rate = f64::from(results.passed) / f64::from(results.total.max(1)) * 100.0;
    eprintln!("  pass rate: {pass_rate:.1}%");

    assert!(
        results.passed > 0,
        "Expected at least some tree construction tests to pass"
    );
}
