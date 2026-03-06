#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::html5::{parse_html5_with_options, Html5ParseOptions};

/// Fragment contexts to exercise different parsing modes.
static CONTEXTS: &[&str] = &[
    "body", "div", "p", "table", "select", "script", "style", "textarea",
    "title", "head", "html", "tr", "td", "th", "caption", "colgroup",
    "template", "noscript", "plaintext", "frameset",
    "svg svg", "svg foreignObject", "math math", "math mi",
];

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Pick a context based on input length to get deterministic variety
        let ctx = CONTEXTS[data.len() % CONTEXTS.len()];
        let opts = Html5ParseOptions {
            scripting: data.first().is_some_and(|b| b & 1 != 0),
            fragment_context: Some(ctx.to_string()),
        };
        let _ = parse_html5_with_options(s, &opts);
    }
});
