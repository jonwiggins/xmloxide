#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::parser::{parse_str_with_options, ParseOptions};
use xmloxide::tree::Document;
use xmloxide::xpath::evaluate;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Evaluate against a fixed document — fuzz the expression
        if let Ok(doc) =
            Document::parse_str("<root><child attr=\"val\">text</child><child id=\"2\"/></root>")
        {
            if let Some(root) = doc.root_element() {
                let _ = evaluate(&doc, root, s);
            }
        }

        // Split input in half: first half is XML, second half is XPath expression.
        // This fuzzes both the document structure and the expression together.
        if s.len() >= 4 {
            let mut mid = s.len() / 2;
            // Ensure we split at a char boundary
            while mid < s.len() && !s.is_char_boundary(mid) {
                mid += 1;
            }
            if mid >= s.len() {
                return;
            }
            let (xml_part, xpath_part) = s.split_at(mid);
            let opts = ParseOptions::default().recover(true);
            if let Ok(doc) = parse_str_with_options(xml_part, &opts) {
                if let Some(root) = doc.root_element() {
                    let _ = evaluate(&doc, root, xpath_part);
                }
            }
        }
    }
});
