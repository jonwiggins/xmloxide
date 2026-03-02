#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::parser::ParseOptions;
use xmloxide::sax::{parse_sax, DefaultHandler};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // SAX parser in strict mode — should never panic
        let mut handler = DefaultHandler;
        let _ = parse_sax(s, &ParseOptions::default(), &mut handler);

        // SAX parser in recovery mode — should never panic
        let mut handler2 = DefaultHandler;
        let _ = parse_sax(s, &ParseOptions::default().recover(true), &mut handler2);
    }
});
