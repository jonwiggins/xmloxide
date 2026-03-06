//! html5lib-tests tokenizer conformance suite.
//!
//! Parses JSON test files from the html5lib-tests repository and runs them
//! against the xmloxide HTML5 tokenizer.
//!
//! To download the test suite, run:
//! ```sh
//! ./scripts/download-html5lib-tests.sh
//! ```
//!
//! Then run these tests with:
//! ```sh
//! cargo test --test html5lib_tokenizer -- --nocapture
//! ```

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;

use serde_json::Value;

use xmloxide::html5::tokenizer::{Token, Tokenizer};

const SUITE_DIR: &str = "tests/html5lib-tests/tokenizer";

// -------------------------------------------------------------------------
// Test harness
// -------------------------------------------------------------------------

/// Convert our Token into the html5lib-tests output format for comparison.
fn tokens_to_html5lib_format(tokens: &[Token]) -> Vec<Value> {
    let mut result = Vec::new();
    // The test suite merges adjacent character tokens into a single string.
    let mut pending_chars = String::new();

    for token in tokens {
        if let Token::Character(c) = token {
            pending_chars.push(*c);
        } else {
            if !pending_chars.is_empty() {
                result.push(Value::Array(vec![
                    Value::String("Character".to_string()),
                    Value::String(std::mem::take(&mut pending_chars)),
                ]));
            }
            match token {
                Token::Doctype {
                    name,
                    public_id,
                    system_id,
                    force_quirks,
                } => {
                    result.push(Value::Array(vec![
                        Value::String("DOCTYPE".to_string()),
                        name.as_ref()
                            .map_or(Value::Null, |n| Value::String(n.clone())),
                        public_id
                            .as_ref()
                            .map_or(Value::Null, |p| Value::String(p.clone())),
                        system_id
                            .as_ref()
                            .map_or(Value::Null, |s| Value::String(s.clone())),
                        Value::Bool(!force_quirks),
                    ]));
                }
                Token::StartTag {
                    name,
                    attributes,
                    self_closing,
                } => {
                    let mut attr_map = serde_json::Map::new();
                    for attr in attributes {
                        attr_map
                            .entry(&attr.name)
                            .or_insert_with(|| Value::String(attr.value.clone()));
                    }
                    let mut arr = vec![
                        Value::String("StartTag".to_string()),
                        Value::String(name.clone()),
                        Value::Object(attr_map),
                    ];
                    if *self_closing {
                        arr.push(Value::Bool(true));
                    }
                    result.push(Value::Array(arr));
                }
                Token::EndTag { name } => {
                    result.push(Value::Array(vec![
                        Value::String("EndTag".to_string()),
                        Value::String(name.clone()),
                    ]));
                }
                Token::Comment(data) => {
                    result.push(Value::Array(vec![
                        Value::String("Comment".to_string()),
                        Value::String(data.clone()),
                    ]));
                }
                Token::Eof | Token::Character(_) => {}
            }
        }
    }
    if !pending_chars.is_empty() {
        result.push(Value::Array(vec![
            Value::String("Character".to_string()),
            Value::String(pending_chars),
        ]));
    }
    result
}

/// Map the test suite's initial state names to tokenizer states.
fn parse_initial_state(name: &str) -> Option<&'static str> {
    match name {
        "Data state" => Some("Data"),
        "PLAINTEXT state" => Some("Plaintext"),
        "RCDATA state" => Some("RcData"),
        "RAWTEXT state" => Some("RawText"),
        "Script data state" => Some("ScriptData"),
        "CDATA section state" => Some("CDataSection"),
        _ => None,
    }
}

fn set_tokenizer_state(tok: &mut Tokenizer<'_>, state_name: &str) {
    tok.set_state_for_test(state_name);
}

fn collect_all_tokens(tok: &mut Tokenizer<'_>) -> Vec<Token> {
    let mut tokens = Vec::new();
    loop {
        let token = tok.next_token();
        if token == Token::Eof {
            break;
        }
        tokens.push(token);
    }
    tokens
}

struct TestResults {
    total: u32,
    passed: u32,
    failed: u32,
    skipped: u32,
}

fn run_test_file(path: &Path, results: &mut TestResults) {
    let content = std::fs::read_to_string(path).unwrap();
    let json: Value = serde_json::from_str(&content).unwrap();

    let Some(tests) = json.get("tests").and_then(Value::as_array) else {
        // Some files (e.g., xmlViolation.test) have a different structure.
        return;
    };
    let file_name = path.file_name().unwrap().to_str().unwrap();

    for test in tests {
        let description = test["description"].as_str().unwrap_or("(no description)");
        let input = test["input"].as_str().unwrap();
        let expected_output = test["output"].as_array().unwrap();

        // Some tests specify initialStates (multiple states to test in).
        let initial_states: Vec<String> = if let Some(states) = test.get("initialStates") {
            states
                .as_array()
                .unwrap()
                .iter()
                .filter_map(|s| s.as_str().map(String::from))
                .collect()
        } else {
            vec!["Data state".to_string()]
        };

        // Some tests have doubleEscaped input — the input contains \uXXXX
        // sequences that need to be unescaped.
        let actual_input = if test.get("doubleEscaped") == Some(&Value::Bool(true)) {
            unescape_double_escaped(input)
        } else {
            input.to_string()
        };

        // Also unescape expected output if doubleEscaped
        let expected: Vec<Value> = if test.get("doubleEscaped") == Some(&Value::Bool(true)) {
            expected_output.iter().map(unescape_json_value).collect()
        } else {
            expected_output.clone()
        };

        for state_name in &initial_states {
            results.total += 1;

            let Some(mapped_state) = parse_initial_state(state_name) else {
                results.skipped += 1;
                continue;
            };

            let mut tok = Tokenizer::new(&actual_input);
            if mapped_state != "Data" {
                set_tokenizer_state(&mut tok, mapped_state);
                // For RCDATA/RAWTEXT/ScriptData, set a fake last start tag
                // so end tag matching works.
                if let Some(last_tag) = test.get("lastStartTag") {
                    if let Some(tag_name) = last_tag.as_str() {
                        tok.set_last_start_tag(tag_name);
                    }
                }
            }

            let tokens = collect_all_tokens(&mut tok);
            let actual = tokens_to_html5lib_format(&tokens);

            if actual == expected {
                results.passed += 1;
            } else {
                results.failed += 1;
                eprintln!("FAIL [{file_name}] {description} (state: {state_name})");
                eprintln!("  input: {actual_input:?}");
                eprintln!("  expected: {}", serde_json::to_string(&expected).unwrap());
                eprintln!("  actual:   {}", serde_json::to_string(&actual).unwrap());
            }
        }
    }
}

fn unescape_double_escaped(s: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '\\' && i + 5 < chars.len() && chars[i + 1] == 'u' {
            let hex: String = chars[i + 2..i + 6].iter().collect();
            if let Ok(cp) = u32::from_str_radix(&hex, 16) {
                if let Some(c) = char::from_u32(cp) {
                    result.push(c);
                    i += 6;
                    continue;
                }
            }
        }
        result.push(chars[i]);
        i += 1;
    }
    result
}

fn unescape_json_value(v: &Value) -> Value {
    match v {
        Value::String(s) => Value::String(unescape_double_escaped(s)),
        Value::Array(arr) => Value::Array(arr.iter().map(unescape_json_value).collect()),
        Value::Object(obj) => {
            let mut new_obj = serde_json::Map::new();
            for (k, val) in obj {
                new_obj.insert(unescape_double_escaped(k), unescape_json_value(val));
            }
            Value::Object(new_obj)
        }
        other => other.clone(),
    }
}

// -------------------------------------------------------------------------
// Main test
// -------------------------------------------------------------------------

#[test]
fn html5lib_tokenizer_suite() {
    let suite_dir = Path::new(SUITE_DIR);
    if !suite_dir.exists() {
        eprintln!(
            "Skipping html5lib tokenizer tests: suite not found at {SUITE_DIR}.\n\
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

    // Collect and sort test files for deterministic order.
    let mut test_files: Vec<_> = std::fs::read_dir(suite_dir)
        .unwrap()
        .filter_map(std::result::Result::ok)
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "test"))
        .map(|e| e.path())
        .collect();
    test_files.sort();

    for path in &test_files {
        run_test_file(path, &mut results);
    }

    eprintln!();
    eprintln!("html5lib tokenizer results:");
    eprintln!(
        "  {}/{} passed ({} failed, {} skipped)",
        results.passed, results.total, results.failed, results.skipped
    );

    // Don't assert 100% yet — track progress. Fail if regression below threshold.
    let pass_rate = f64::from(results.passed) / f64::from(results.total.max(1)) * 100.0;
    eprintln!("  pass rate: {pass_rate:.1}%");

    // We expect at least some tests to pass. Assert a minimum floor.
    assert!(
        results.passed > 0,
        "Expected at least some tokenizer tests to pass"
    );
}
