#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::parser::{parse_str_with_options, ParseOptions};
use xmloxide::validation::dtd::{parse_dtd, validate};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Fuzz DTD parsing directly — should never panic
        let _ = parse_dtd(s);

        // Parse as XML (recovery mode) then validate against any internal DTD
        let opts = ParseOptions::default().recover(true);
        if let Ok(mut doc) = parse_str_with_options(s, &opts) {
            // Try parsing the input as a DTD and validating the doc against it
            if let Ok(dtd) = parse_dtd(s) {
                let _ = validate(&mut doc, &dtd);
            }
        }
    }
});
