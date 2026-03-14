#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::parser::{parse_str_with_options, ParseOptions};
use xmloxide::validation::schematron::{parse_schematron, validate_schematron};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Fuzz Schematron schema parsing — should never panic
        let _ = parse_schematron(s);

        // Parse as XML (recovery mode) then validate against input as a schema
        let opts = ParseOptions::default().recover(true);
        if let Ok(doc) = parse_str_with_options(s, &opts) {
            if let Ok(schema) = parse_schematron(s) {
                let _ = validate_schematron(&doc, &schema);
            }
        }

        // Split input: first half is schema, second half is document
        if s.len() >= 4 {
            let mut mid = s.len() / 2;
            while mid < s.len() && !s.is_char_boundary(mid) {
                mid += 1;
            }
            if mid < s.len() {
                let (schema_part, doc_part) = s.split_at(mid);
                if let Ok(schema) = parse_schematron(schema_part) {
                    if let Ok(doc) = parse_str_with_options(doc_part, &opts) {
                        let _ = validate_schematron(&doc, &schema);
                    }
                }
            }
        }
    }
});
