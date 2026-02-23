#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::parser::{parse_str_with_options, ParseOptions};
use xmloxide::serial::serialize;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let opts = ParseOptions::default().recover(true);
        // Parse -> serialize -> parse roundtrip should never panic
        if let Ok(doc) = parse_str_with_options(s, &opts) {
            let output = serialize(&doc);
            let _ = parse_str_with_options(&output, &opts);
        }
    }
});
