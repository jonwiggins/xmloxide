#![allow(clippy::unwrap_used, clippy::expect_used)]
use std::fs;
use std::path::Path;

fn normalize(s: &str) -> String {
    let mut result: String = s.lines().map(str::trim_end).collect::<Vec<_>>().join("\n");
    if !result.ends_with('\n') {
        result.push('\n');
    }
    result
}

#[test]
fn debug_html_tests() {
    let base = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/libxml2-compat/libxml2");
    let test_dir = base.join("test/HTML");
    let result_dir = base.join("result/HTML");

    if !test_dir.is_dir() {
        eprintln!("Test data not available");
        return;
    }

    let tests = &[
        "attrents",
        "doc2",
        "issue318",
        "python",
        "test3",
        "xml-declaration-1",
    ];

    for stem in tests {
        let test_file = if test_dir.join(format!("{stem}.html")).exists() {
            test_dir.join(format!("{stem}.html"))
        } else {
            test_dir.join(format!("{stem}.htm"))
        };
        let result_file = if result_dir.join(format!("{stem}.html")).exists() {
            result_dir.join(format!("{stem}.html"))
        } else {
            result_dir.join(format!("{stem}.htm"))
        };

        if !test_file.exists() || !result_file.exists() {
            eprintln!("=== {stem} === MISSING FILES");
            continue;
        }

        let input = fs::read_to_string(&test_file).unwrap();
        let expected = fs::read_to_string(&result_file).unwrap();

        eprintln!("=== {stem} ===");

        match xmloxide::html::parse_html(&input) {
            Ok(doc) => {
                let output = xmloxide::serial::html::serialize_html(&doc);
                let norm_output = normalize(&output);
                let norm_expected = normalize(&expected);

                if norm_output == norm_expected {
                    eprintln!("  RESULT: PASS");
                } else {
                    let out_lines: Vec<&str> = norm_output.lines().collect();
                    let exp_lines: Vec<&str> = norm_expected.lines().collect();
                    for (i, (a, b)) in out_lines.iter().zip(exp_lines.iter()).enumerate() {
                        if a != b {
                            eprintln!("  FIRST DIFF at line {}: ", i + 1);
                            eprintln!("    ACTUAL:   {a:?}");
                            eprintln!("    EXPECTED: {b:?}");
                            break;
                        }
                    }
                    if out_lines.len() != exp_lines.len() {
                        eprintln!(
                            "  LINE COUNT: actual={} expected={}",
                            out_lines.len(),
                            exp_lines.len()
                        );
                    }
                }
            }
            Err(e) => {
                eprintln!("  PARSE ERROR: {}", e.message);
            }
        }
        eprintln!();
    }
}
