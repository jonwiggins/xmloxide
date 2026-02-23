#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::parser::{parse_str_with_options, ParseOptions};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Parse in strict mode — should never panic
        let _ = parse_str_with_options(s, &ParseOptions::default());
        // Parse in recovery mode — should never panic
        let _ = parse_str_with_options(s, &ParseOptions::default().recover(true));
    }
});
