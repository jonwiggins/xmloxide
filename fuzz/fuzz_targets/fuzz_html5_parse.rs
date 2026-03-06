#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::html5::{parse_html5, parse_html5_with_options, Html5ParseOptions};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Default options — should never panic
        let _ = parse_html5(s);

        // With scripting enabled — should never panic
        let opts = Html5ParseOptions {
            scripting: true,
            fragment_context: None,
        };
        let _ = parse_html5_with_options(s, &opts);
    }
});
