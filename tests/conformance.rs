//! W3C XML Conformance Test Suite harness.
//!
//! Runs the 2000+ tests from the W3C XML Conformance Test Suite
//! against the xmloxide parser and reports results.
//!
//! To download the test suite, run:
//! ```sh
//! ./scripts/download-conformance-suite.sh
//! ```
//!
//! Then run the tests with:
//! ```sh
//! cargo test --test conformance -- --nocapture
//! ```

#![allow(clippy::unwrap_used)]

use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::time::Duration;

use xmloxide::Document;

/// Base path for the conformance test suite.
const SUITE_DIR: &str = "tests/conformance/xmlconf";

/// A single test case from the W3C XML Conformance Test Suite.
#[allow(dead_code)]
#[derive(Debug)]
struct TestCase {
    /// Unique test identifier.
    id: String,
    /// Test type: "not-wf", "valid", "invalid", or "error".
    test_type: String,
    /// Entity handling: "none", "general", "parameter", or "both".
    entities: String,
    /// Path to the test file (relative to suite base dir).
    uri: PathBuf,
    /// Spec sections this test covers.
    sections: String,
    /// Optional recommendation version (e.g., "XML1.1").
    recommendation: Option<String>,
    /// Optional edition (e.g., "5").
    edition: Option<String>,
    /// Optional VERSION attribute (e.g., "1.1").
    version: Option<String>,
    /// Optional NAMESPACE attribute ("yes" or "no").
    namespace: Option<String>,
    /// Description of what the test checks.
    description: String,
}

/// Results of running the conformance suite.
#[derive(Debug, Default)]
struct ConformanceResults {
    total: u32,
    passed: u32,
    failed: u32,
    skipped: u32,
    failures: Vec<FailureInfo>,
}

/// Information about a single test failure.
#[derive(Debug)]
struct FailureInfo {
    id: String,
    test_type: String,
    expected: &'static str,
    actual: &'static str,
    detail: String,
}

impl std::fmt::Display for ConformanceResults {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "\n=== W3C XML Conformance Test Results ===")?;
        writeln!(f, "Total:   {}", self.total)?;
        writeln!(f, "Passed:  {}", self.passed)?;
        writeln!(f, "Failed:  {}", self.failed)?;
        writeln!(f, "Skipped: {}", self.skipped)?;
        if self.total > 0 {
            let pct = f64::from(self.passed) / f64::from(self.total - self.skipped) * 100.0;
            writeln!(f, "Pass rate: {pct:.1}% (of non-skipped)")?;
        }
        if !self.failures.is_empty() {
            writeln!(f, "\n--- Failures (first 200) ---")?;
            for failure in self.failures.iter().take(200) {
                writeln!(
                    f,
                    "  {} [{}]: expected={}, actual={} — {}",
                    failure.id, failure.test_type, failure.expected, failure.actual, failure.detail
                )?;
            }
            if self.failures.len() > 200 {
                writeln!(f, "  ... and {} more", self.failures.len() - 200)?;
            }
        }
        Ok(())
    }
}

/// A catalog entry: a sub-catalog file and its `xml:base` path prefix.
struct CatalogEntry {
    /// Path to the catalog XML file, relative to the suite dir.
    catalog_file: &'static str,
    /// Base path prefix for test URIs, relative to the suite dir.
    base_path: &'static str,
}

/// All known sub-catalogs in the W3C XML Conformance Test Suite.
/// These are listed in `xmlconf.xml` and reference external entity sub-catalogs.
const CATALOGS: &[CatalogEntry] = &[
    CatalogEntry {
        catalog_file: "xmltest/xmltest.xml",
        base_path: "xmltest",
    },
    CatalogEntry {
        catalog_file: "japanese/japanese.xml",
        base_path: "japanese",
    },
    CatalogEntry {
        catalog_file: "sun/sun-valid.xml",
        base_path: "sun",
    },
    CatalogEntry {
        catalog_file: "sun/sun-invalid.xml",
        base_path: "sun",
    },
    CatalogEntry {
        catalog_file: "sun/sun-not-wf.xml",
        base_path: "sun",
    },
    CatalogEntry {
        catalog_file: "sun/sun-error.xml",
        base_path: "sun",
    },
    CatalogEntry {
        catalog_file: "oasis/oasis.xml",
        base_path: "oasis",
    },
    CatalogEntry {
        catalog_file: "ibm/ibm_oasis_invalid.xml",
        base_path: "ibm",
    },
    CatalogEntry {
        catalog_file: "ibm/ibm_oasis_not-wf.xml",
        base_path: "ibm",
    },
    CatalogEntry {
        catalog_file: "ibm/ibm_oasis_valid.xml",
        base_path: "ibm",
    },
    CatalogEntry {
        catalog_file: "eduni/errata-2e/errata2e.xml",
        base_path: "eduni/errata-2e",
    },
    CatalogEntry {
        catalog_file: "eduni/errata-3e/errata3e.xml",
        base_path: "eduni/errata-3e",
    },
    CatalogEntry {
        catalog_file: "eduni/errata-4e/errata4e.xml",
        base_path: "eduni/errata-4e",
    },
    CatalogEntry {
        catalog_file: "eduni/namespaces/1.0/rmt-ns10.xml",
        base_path: "eduni/namespaces/1.0",
    },
    CatalogEntry {
        catalog_file: "eduni/namespaces/errata-1e/errata1e.xml",
        base_path: "eduni/namespaces/errata-1e",
    },
    CatalogEntry {
        catalog_file: "eduni/misc/ht-bh.xml",
        base_path: "eduni/misc",
    },
    // XML 1.1 catalogs — skipped since xmloxide targets XML 1.0 only
];

/// Parses a catalog XML file and extracts test cases.
///
/// The catalog format uses `<TEST>` elements with attributes:
/// `TYPE`, `ID`, `URI`, `ENTITIES`, `SECTIONS`, `RECOMMENDATION`, `EDITION`.
///
/// Since the catalog files themselves use entity references for includes,
/// and our parser supports built-in entities only, we parse each sub-catalog
/// independently using simple string scanning.
fn parse_catalog(suite_dir: &Path, entry: &CatalogEntry) -> Vec<TestCase> {
    let catalog_path = suite_dir.join(entry.catalog_file);
    let content = match std::fs::read_to_string(&catalog_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!(
                "WARNING: Could not read catalog {}: {e}",
                catalog_path.display()
            );
            return Vec::new();
        }
    };

    let mut tests = Vec::new();
    let mut search_pos = 0;

    while let Some(start) = content[search_pos..].find("<TEST ") {
        let abs_start = search_pos + start;

        // Find the base path from the nearest TESTCASES xml:base
        let mut base = String::new();
        if let Some(tc_pos) = content[..abs_start].rfind("<TESTCASES") {
            let tc_line = &content[tc_pos..];
            if let Some(end) = tc_line.find('>') {
                let tc_tag = &tc_line[..=end];
                if let Some(b) = extract_attr(tc_tag, "xml:base") {
                    base = b;
                }
            }
        }

        // Find the end of the TEST element
        let rest = &content[abs_start..];
        let end = if let Some(e) = rest.find("</TEST>") {
            e + "</TEST>".len()
        } else if let Some(e) = rest.find("/>") {
            e + "/>".len()
        } else {
            search_pos = abs_start + 5;
            continue;
        };

        let test_element = &rest[..end];

        // Extract attributes
        let id = extract_attr(test_element, "ID").unwrap_or_default();
        let test_type = extract_attr(test_element, "TYPE").unwrap_or_default();
        let entities = extract_attr(test_element, "ENTITIES").unwrap_or_else(|| "none".to_string());
        let uri_str = extract_attr(test_element, "URI").unwrap_or_default();
        let sections = extract_attr(test_element, "SECTIONS").unwrap_or_default();
        let recommendation = extract_attr(test_element, "RECOMMENDATION");
        let edition = extract_attr(test_element, "EDITION");
        let version = extract_attr(test_element, "VERSION");
        let namespace = extract_attr(test_element, "NAMESPACE");

        // Extract description (text content between > and </TEST>)
        let description = if let Some(gt) = test_element.find('>') {
            let after_gt = &test_element[gt + 1..];
            if let Some(end_tag) = after_gt.find("</TEST>") {
                after_gt[..end_tag].trim().to_string()
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        // Build the full URI path
        let mut full_path = PathBuf::from(entry.base_path);
        if !base.is_empty() {
            full_path = full_path.join(&base);
        }
        full_path = full_path.join(&uri_str);

        if !id.is_empty() && !uri_str.is_empty() {
            tests.push(TestCase {
                id,
                test_type,
                entities,
                uri: full_path,
                sections,
                recommendation,
                edition,
                version,
                namespace,
                description,
            });
        }

        search_pos = abs_start + end;
    }

    tests
}

/// Extracts an XML attribute value from a tag string.
fn extract_attr(tag: &str, attr_name: &str) -> Option<String> {
    let patterns = [format!("{attr_name}=\""), format!("{attr_name}='")];

    for pattern in &patterns {
        if let Some(start) = tag.find(pattern.as_str()) {
            let value_start = start + pattern.len();
            let quote = tag.as_bytes()[start + pattern.len() - 1];
            let rest = &tag[value_start..];
            if let Some(end) = rest.find(quote as char) {
                return Some(rest[..end].to_string());
            }
        }
    }
    None
}

/// Returns true if this test should be skipped.
fn should_skip(test: &TestCase) -> bool {
    // Skip XML 1.1 tests — we only implement XML 1.0
    if let Some(ref rec) = test.recommendation {
        if rec.contains("1.1") {
            return true;
        }
    }

    // Skip tests with VERSION="1.1" — these require XML 1.1 features
    if let Some(ref ver) = test.version {
        if ver.contains("1.1") {
            return true;
        }
    }

    // Skip tests that require external entity handling (general, parameter, both)
    // Our parser only supports built-in entities
    if test.entities != "none" {
        return true;
    }

    // Skip tests targeting only earlier editions (1-4). Our parser implements
    // Edition 5 which broadened NameStartChar/NameChar ranges. Tests that
    // don't include edition "5" test constraints that no longer apply.
    if let Some(ref edition) = test.edition {
        let editions: Vec<&str> = edition.split_whitespace().collect();
        if !editions.is_empty() && !editions.contains(&"5") {
            return true;
        }
    }

    // Skip tests with NAMESPACE="no" — our parser always performs namespace
    // processing per Namespaces in XML 1.0, so tests that use bare colons
    // as regular name characters would be incorrectly rejected.
    if let Some(ref ns) = test.namespace {
        if ns == "no" {
            return true;
        }
    }

    false
}

/// Runs a single test case and returns (passed, skipped, failure info).
fn run_test(suite_dir: &Path, test: &TestCase) -> (bool, bool, Option<FailureInfo>) {
    if should_skip(test) {
        return (false, true, None);
    }

    let test_file = suite_dir.join(&test.uri);
    let Ok(content) = std::fs::read(&test_file) else {
        // File not found — skip
        return (false, true, None);
    };

    // Run parsing with a 5-second timeout to catch infinite loops
    let (tx, rx) = mpsc::channel();
    let content_clone = content.clone();
    std::thread::spawn(move || {
        let result = if let Ok(text) = std::str::from_utf8(&content_clone) {
            Document::parse_str(text).map(|_| ()).map_err(|e| e.message)
        } else {
            Document::parse_bytes(&content_clone)
                .map(|_| ())
                .map_err(|e| e.message)
        };
        let _ = tx.send(result);
    });

    let Ok(result) = rx.recv_timeout(Duration::from_secs(5)) else {
        // Timed out — treat as a failure
        return (
            false,
            false,
            Some(FailureInfo {
                id: test.id.clone(),
                test_type: test.test_type.clone(),
                expected: "completion",
                actual: "timeout",
                detail: "parser timed out after 5 seconds".to_string(),
            }),
        );
    };

    match test.test_type.as_str() {
        "not-wf" => {
            // Not well-formed: parser MUST reject
            if result.is_err() {
                (true, false, None)
            } else {
                (
                    false,
                    false,
                    Some(FailureInfo {
                        id: test.id.clone(),
                        test_type: test.test_type.clone(),
                        expected: "error",
                        actual: "success",
                        detail: test.description.chars().take(100).collect(),
                    }),
                )
            }
        }
        "valid" => {
            // Valid: parser MUST accept
            match &result {
                Ok(()) => (true, false, None),
                Err(msg) => (
                    false,
                    false,
                    Some(FailureInfo {
                        id: test.id.clone(),
                        test_type: test.test_type.clone(),
                        expected: "success",
                        actual: "error",
                        detail: msg.chars().take(100).collect(),
                    }),
                ),
            }
        }
        "invalid" => {
            // Invalid: well-formed but not valid. Parser should accept
            // (since we're testing well-formedness, not validation).
            match &result {
                Ok(()) => (true, false, None),
                Err(msg) => (
                    false,
                    false,
                    Some(FailureInfo {
                        id: test.id.clone(),
                        test_type: test.test_type.clone(),
                        expected: "success (well-formed)",
                        actual: "error",
                        detail: msg.chars().take(100).collect(),
                    }),
                ),
            }
        }
        "error" => {
            // Error: processor may or may not report. Always pass.
            (true, false, None)
        }
        _ => {
            // Unknown type — skip
            (false, true, None)
        }
    }
}

/// Runs the full W3C XML Conformance Test Suite and reports results.
fn run_conformance_suite() -> ConformanceResults {
    let suite_dir = PathBuf::from(SUITE_DIR);
    let mut results = ConformanceResults::default();

    // Breakdown by type
    let mut by_type: std::collections::HashMap<String, (u32, u32, u32)> =
        std::collections::HashMap::new();

    for entry in CATALOGS {
        let tests = parse_catalog(&suite_dir, entry);
        for test in &tests {
            results.total += 1;

            let (passed, skipped, failure) = run_test(&suite_dir, test);
            if skipped {
                results.skipped += 1;
            } else if passed {
                results.passed += 1;
            } else {
                results.failed += 1;
                if let Some(f) = failure {
                    results.failures.push(f);
                }
            }

            let type_counts = by_type.entry(test.test_type.clone()).or_insert((0, 0, 0));
            if skipped {
                type_counts.2 += 1;
            } else if passed {
                type_counts.0 += 1;
            } else {
                type_counts.1 += 1;
            }
        }
    }

    // Print breakdown by type
    eprintln!("\n--- Breakdown by test type ---");
    let mut types: Vec<_> = by_type.into_iter().collect();
    types.sort_by(|a, b| a.0.cmp(&b.0));
    for (test_type, (pass, fail, skip)) in &types {
        eprintln!("  {test_type:>10}: {pass} passed, {fail} failed, {skip} skipped");
    }

    results
}

#[test]
fn test_w3c_conformance_suite() {
    let suite_dir = PathBuf::from(SUITE_DIR);
    if !suite_dir.join("xmlconf.xml").exists() {
        eprintln!(
            "W3C XML Conformance Test Suite not found at {SUITE_DIR}.\n\
             Run ./scripts/download-conformance-suite.sh to download it.\n\
             Skipping conformance tests."
        );
        return;
    }

    let results = run_conformance_suite();
    eprintln!("{results}");

    // We don't assert a specific pass rate yet — this is a baseline measurement.
    // As conformance improves, we can ratchet up the required pass count.
    assert!(
        results.total > 0,
        "Should have found at least some test cases"
    );

    // Record baseline: print summary that can be used to set future thresholds
    eprintln!(
        "\nBaseline: {} passed out of {} non-skipped ({} total, {} skipped)",
        results.passed,
        results.total - results.skipped,
        results.total,
        results.skipped
    );
}

/// Runs only the not-well-formed tests for quick feedback during development.
#[test]
fn test_w3c_not_well_formed() {
    let suite_dir = PathBuf::from(SUITE_DIR);
    if !suite_dir.join("xmlconf.xml").exists() {
        return;
    }

    let mut pass = 0u32;
    let mut fail = 0u32;
    let mut skip = 0u32;
    let mut failures = Vec::new();

    for entry in CATALOGS {
        let tests = parse_catalog(&suite_dir, entry);
        for test in tests.iter().filter(|t| t.test_type == "not-wf") {
            let (passed, skipped, failure) = run_test(&suite_dir, test);
            if skipped {
                skip += 1;
            } else if passed {
                pass += 1;
            } else {
                fail += 1;
                if let Some(f) = failure {
                    failures.push(f);
                }
            }
        }
    }

    eprintln!("\n--- Not-well-formed tests ---");
    eprintln!("  Passed: {pass}, Failed: {fail}, Skipped: {skip}");
    if !failures.is_empty() {
        eprintln!("  First 20 failures:");
        for f in failures.iter().take(20) {
            eprintln!("    {} — {}", f.id, f.detail);
        }
    }
}

/// Runs only the valid document tests.
#[test]
fn test_w3c_valid_documents() {
    let suite_dir = PathBuf::from(SUITE_DIR);
    if !suite_dir.join("xmlconf.xml").exists() {
        return;
    }

    let mut pass = 0u32;
    let mut fail = 0u32;
    let mut skip = 0u32;
    let mut failures = Vec::new();

    for entry in CATALOGS {
        let tests = parse_catalog(&suite_dir, entry);
        for test in tests.iter().filter(|t| t.test_type == "valid") {
            let (passed, skipped, failure) = run_test(&suite_dir, test);
            if skipped {
                skip += 1;
            } else if passed {
                pass += 1;
            } else {
                fail += 1;
                if let Some(f) = failure {
                    failures.push(f);
                }
            }
        }
    }

    eprintln!("\n--- Valid document tests ---");
    eprintln!("  Passed: {pass}, Failed: {fail}, Skipped: {skip}");
    if !failures.is_empty() {
        eprintln!("  First 20 failures:");
        for f in failures.iter().take(20) {
            eprintln!("    {} — {}", f.id, f.detail);
        }
    }
}
