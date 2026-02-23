//! libxml2 regression test suite compatibility harness.
//!
//! This test compares xmloxide's parsing and serialization output against
//! libxml2's expected results from its regression test suite.
//!
//! The test data must be downloaded first:
//!
//! ```sh
//! ./scripts/download-libxml2-tests.sh
//! ```
//!
//! Run with: `cargo test --test libxml2_compat -- --nocapture`
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use xmloxide::parser::{self, ParseOptions};
use xmloxide::serial::html::serialize_html;
use xmloxide::serial::serialize;
use xmloxide::Document;

/// Root directory containing the downloaded libxml2 test data.
fn libxml2_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/libxml2-compat/libxml2")
}

/// Returns true if the libxml2 test data has been downloaded.
fn test_data_available() -> bool {
    let dir = libxml2_dir();
    dir.join("test").is_dir() && dir.join("result").is_dir()
}

/// Tracks results for a category of tests.
struct CompatResults {
    category: String,
    total: usize,
    passed: usize,
    failed: usize,
    skipped: usize,
    failures: Vec<String>,
}

impl CompatResults {
    fn new(category: &str) -> Self {
        Self {
            category: category.to_string(),
            total: 0,
            passed: 0,
            failed: 0,
            skipped: 0,
            failures: Vec::new(),
        }
    }

    fn pass(&mut self) {
        self.total += 1;
        self.passed += 1;
    }

    fn fail(&mut self, name: &str) {
        self.total += 1;
        self.failed += 1;
        self.failures.push(name.to_string());
    }

    fn skip(&mut self) {
        self.total += 1;
        self.skipped += 1;
    }
}

impl fmt::Display for CompatResults {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {}/{} passed ({} skipped, {} failed)",
            self.category,
            self.passed,
            self.total - self.skipped,
            self.skipped,
            self.failed
        )?;
        if !self.failures.is_empty() {
            let display_count = self.failures.len().min(10);
            write!(f, "\n  First failures:")?;
            for name in &self.failures[..display_count] {
                write!(f, "\n    - {name}")?;
            }
            if self.failures.len() > display_count {
                write!(
                    f,
                    "\n    ... and {} more",
                    self.failures.len() - display_count
                )?;
            }
        }
        Ok(())
    }
}

/// Normalizes XML output for comparison by trimming trailing whitespace
/// from each line and ensuring a consistent trailing newline.
fn normalize_xml(s: &str) -> String {
    let mut result: String = s.lines().map(str::trim_end).collect::<Vec<_>>().join("\n");
    if !result.ends_with('\n') {
        result.push('\n');
    }
    result
}

/// Collects XML files matching a pattern in a directory.
fn collect_xml_files(dir: &Path, extension: &str) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if !dir.is_dir() {
        return files;
    }
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().is_some_and(|ext| ext == extension) {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

/// Files that require external entity resolution or other unsupported features.
const SKIP_FILES: &[&str] = &[
    // External entity resolution required
    "dtd1",
    "dtd2",
    "dtd3",
    "dtd4",
    "dtd5",
    "dtd6",
    "dtd7",
    "dtd8",
    "dtd9",
    "dtd10",
    "dtd11",
    "dtd12",
    "dtds",
    "ent1",
    "ent2",
    "ent3",
    "ent4",
    "ent5",
    "ent6",
    "ent7",
    "ent8",
    "ent9",
    "ent10",
    "ent11",
    "p51",
    // External DTD subset
    "valid1",
    "valid2",
    // XInclude test files
    "xinclude",
    // Catalog test files
    "catalog",
    // Schema test files
    "schemas",
    "relaxng",
    "schematron",
    // Files with encoding issues that need system iconv
    "iso8859",
    "GB18030",
    "EUC-JP",
    "Shift_JIS",
    // UTF-16 encoded files (need transcoding support)
    "utf16",
];

/// Checks if a file should be skipped based on its stem.
fn should_skip(path: &Path) -> bool {
    let stem = path.file_stem().unwrap_or_default().to_string_lossy();
    let name = path.file_name().unwrap_or_default().to_string_lossy();
    SKIP_FILES
        .iter()
        .any(|skip| stem.starts_with(skip) || name.starts_with(skip))
}

/// Tries to read a file as UTF-8 for test processing.
/// Returns `None` for binary/non-UTF-8 files (which should be skipped).
fn try_read_utf8(path: &Path) -> Option<String> {
    fs::read_to_string(path).ok()
}

/// Compares parse+serialize output against an expected result.
fn compare_parse_output(results: &mut CompatResults, stem: &str, input: &str, expected: &str) {
    if let Ok(doc) = Document::parse_str(input) {
        let output = serialize(&doc);
        if normalize_xml(&output) == normalize_xml(expected) {
            results.pass();
        } else {
            results.fail(stem);
        }
    } else {
        results.fail(stem);
    }
}

/// Compares parse+serialize output using recovery mode (for namespace error tests).
fn compare_parse_output_recover(
    results: &mut CompatResults,
    stem: &str,
    input: &str,
    expected: &str,
) {
    let opts = ParseOptions::default().recover(true);
    match parser::parse_str_with_options(input, &opts) {
        Ok(doc) => {
            let output = serialize(&doc);
            if normalize_xml(&output) == normalize_xml(expected) {
                results.pass();
            } else {
                results.fail(stem);
            }
        }
        Err(_) => {
            results.fail(stem);
        }
    }
}

/// Test category: parse XML files and compare serialized output against
/// libxml2's expected results.
fn run_xml_parse_tests() -> CompatResults {
    let mut results = CompatResults::new("XML parse");
    let base = libxml2_dir();
    let test_dir = base.join("test");
    let result_dir = base.join("result");

    let files = collect_xml_files(&test_dir, "xml");

    for test_file in &files {
        let stem = test_file.file_stem().unwrap().to_string_lossy();

        if should_skip(test_file) {
            results.skip();
            continue;
        }

        let result_file = result_dir.join(format!("{stem}.xml"));
        if !result_file.exists() {
            results.skip();
            continue;
        }

        let Some(input) = try_read_utf8(test_file) else {
            results.skip();
            continue;
        };

        let Some(expected) = try_read_utf8(&result_file) else {
            results.skip();
            continue;
        };

        compare_parse_output(&mut results, &stem, &input, &expected);
    }

    results
}

/// Test category: parse namespace test files.
fn run_namespace_tests() -> CompatResults {
    let mut results = CompatResults::new("Namespaces");
    let base = libxml2_dir();
    let test_dir = base.join("test/namespaces");
    let result_dir = base.join("result/namespaces");

    if !test_dir.is_dir() {
        return results;
    }

    let files = collect_xml_files(&test_dir, "xml");

    for test_file in &files {
        let stem = test_file.file_stem().unwrap().to_string_lossy();

        if should_skip(test_file) {
            results.skip();
            continue;
        }

        let result_file = result_dir.join(format!("{stem}.xml"));
        if !result_file.exists() {
            results.skip();
            continue;
        }

        let Some(input) = try_read_utf8(test_file) else {
            results.skip();
            continue;
        };

        let Some(expected) = try_read_utf8(&result_file) else {
            results.skip();
            continue;
        };

        // Namespace error tests (err_*) need recovery mode — they contain
        // intentionally malformed namespace declarations.
        if stem.starts_with("err_") {
            compare_parse_output_recover(&mut results, &stem, &input, &expected);
        } else {
            compare_parse_output(&mut results, &stem, &input, &expected);
        }
    }

    results
}

/// Test category: check that error test files produce parse errors.
fn run_error_tests() -> CompatResults {
    let mut results = CompatResults::new("Error detection");
    let base = libxml2_dir();
    let test_dir = base.join("test/errors");

    if !test_dir.is_dir() {
        return results;
    }

    let files = collect_xml_files(&test_dir, "xml");

    for test_file in &files {
        let stem = test_file.file_stem().unwrap().to_string_lossy();

        if should_skip(test_file) {
            results.skip();
            continue;
        }

        let Some(input) = try_read_utf8(test_file) else {
            results.skip();
            continue;
        };

        // For error tests, we expect parsing to fail
        if Document::parse_str(&input).is_err() {
            results.pass();
        } else {
            results.fail(&stem);
        }
    }

    results
}

/// Test category: parse + serialize roundtrip for HTML files.
fn run_html_tests() -> CompatResults {
    let mut results = CompatResults::new("HTML parse");
    let base = libxml2_dir();
    let test_dir = base.join("test/HTML");
    let result_dir = base.join("result/HTML");

    if !test_dir.is_dir() {
        return results;
    }

    let mut files = Vec::new();
    if let Ok(entries) = fs::read_dir(&test_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file()
                && path
                    .extension()
                    .is_some_and(|ext| ext == "html" || ext == "htm")
            {
                files.push(path);
            }
        }
    }
    files.sort();

    for test_file in &files {
        let stem = test_file.file_stem().unwrap().to_string_lossy();

        if should_skip(test_file) {
            results.skip();
            continue;
        }

        // Look for expected result with various extensions
        let result_file = if result_dir.join(format!("{stem}.html")).exists() {
            result_dir.join(format!("{stem}.html"))
        } else if result_dir.join(format!("{stem}.htm")).exists() {
            result_dir.join(format!("{stem}.htm"))
        } else {
            results.skip();
            continue;
        };

        let Some(input) = try_read_utf8(test_file) else {
            results.skip();
            continue;
        };

        let Some(expected) = try_read_utf8(&result_file) else {
            results.skip();
            continue;
        };

        if let Ok(doc) = xmloxide::html::parse_html(&input) {
            let output = serialize_html(&doc);
            if normalize_xml(&output) == normalize_xml(&expected) {
                results.pass();
            } else {
                results.fail(&stem);
            }
        } else {
            results.fail(&stem);
        }
    }

    results
}

#[test]
fn test_libxml2_compat_suite() {
    if !test_data_available() {
        eprintln!(
            "Skipping libxml2 compatibility tests: test data not found.\n\
             Run ./scripts/download-libxml2-tests.sh to download."
        );
        return;
    }

    let xml_results = run_xml_parse_tests();
    let ns_results = run_namespace_tests();
    let error_results = run_error_tests();
    let html_results = run_html_tests();

    eprintln!("\n=== libxml2 Compatibility Test Results ===\n");
    eprintln!("{xml_results}");
    eprintln!("{ns_results}");
    eprintln!("{error_results}");
    eprintln!("{html_results}");

    let total_passed =
        xml_results.passed + ns_results.passed + error_results.passed + html_results.passed;
    let total_tests = (xml_results.total - xml_results.skipped)
        + (ns_results.total - ns_results.skipped)
        + (error_results.total - error_results.skipped)
        + (html_results.total - html_results.skipped);

    eprintln!("\nOverall: {total_passed}/{total_tests} passed");

    // The test suite is informational — we don't assert pass rates
    // because we expect some differences from libxml2's behavior.
    // Check BASELINE.md for current expected pass rates.
}
