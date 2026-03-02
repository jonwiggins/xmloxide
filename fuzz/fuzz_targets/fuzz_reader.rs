#![no_main]
use libfuzzer_sys::fuzz_target;
use xmloxide::parser::ParseOptions;
use xmloxide::reader::XmlReader;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Strict mode — should never panic
        let mut reader = XmlReader::new(s);
        while let Ok(true) = reader.read() {
            // Exercise all accessors on each node
            let _ = reader.node_type();
            let _ = reader.name();
            let _ = reader.local_name();
            let _ = reader.prefix();
            let _ = reader.namespace_uri();
            let _ = reader.value();
            let _ = reader.has_value();
            let _ = reader.depth();
            let _ = reader.is_empty_element();
            let _ = reader.attribute_count();
        }

        // Recovery mode — should never panic
        let opts = ParseOptions::default().recover(true);
        let mut reader2 = XmlReader::with_options(s, opts);
        while let Ok(true) = reader2.read() {}
    }
});
