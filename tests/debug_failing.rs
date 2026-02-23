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
fn debug_failing_tests() {
    let base = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/libxml2-compat/libxml2");
    let test_dir = base.join("test");
    let result_dir = base.join("result");

    if !test_dir.is_dir() {
        eprintln!("Test data not available");
        return;
    }

    let failing = &["attrib", "entity-in-ns-uri", "utf16lebom", "wap", "wml"];

    for stem in failing {
        let test_file = test_dir.join(format!("{stem}.xml"));
        let result_file = result_dir.join(format!("{stem}.xml"));

        if !test_file.exists() || !result_file.exists() {
            eprintln!("=== {stem} === MISSING FILES");
            continue;
        }

        let input = fs::read_to_string(&test_file).unwrap();
        let expected = fs::read_to_string(&result_file).unwrap();

        eprintln!("=== {stem} ===");

        match xmloxide::Document::parse_str(&input) {
            Ok(doc) => {
                let output = xmloxide::serial::serialize(&doc);
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
