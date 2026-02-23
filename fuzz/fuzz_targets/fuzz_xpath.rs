#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::tree::Document;
use xmloxide::xpath::evaluate;

fuzz_target!(|data: &[u8]| {
    if let Ok(expr) = std::str::from_utf8(data) {
        // Create a minimal document to evaluate against
        if let Ok(doc) = Document::parse_str("<root><child attr=\"val\">text</child></root>") {
            if let Some(root) = doc.root_element() {
                // XPath evaluation should never panic on any expression
                let _ = evaluate(&doc, root, expr);
            }
        }
    }
});
