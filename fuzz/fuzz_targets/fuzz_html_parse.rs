#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::html::{parse_html, parse_html_with_options, HtmlParseOptions};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Default options — should never panic
        let _ = parse_html(s);

        // With blank stripping — should never panic
        let opts = HtmlParseOptions::default().no_blanks(true);
        let _ = parse_html_with_options(s, &opts);

        // Without implied elements — should never panic
        let opts = HtmlParseOptions::default().no_implied(true);
        let _ = parse_html_with_options(s, &opts);

        // Strict mode (recovery disabled) — should never panic
        let opts = HtmlParseOptions::default().recover(false);
        let _ = parse_html_with_options(s, &opts);
    }
});
