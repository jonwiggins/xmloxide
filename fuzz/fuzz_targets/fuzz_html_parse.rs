#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::html::parse_html;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // HTML parser should never panic on any input
        let _ = parse_html(s);
    }
});
