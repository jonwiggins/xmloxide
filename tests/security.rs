//! Security-focused tests for xmloxide.
//!
//! These tests verify that the parser rejects malicious or pathological
//! inputs that could cause denial of service (`DoS`) via excessive resource
//! consumption.

#![allow(clippy::unwrap_used)]

use std::fmt::Write;

use xmloxide::parser::{parse_str_with_options, ParseOptions};
use xmloxide::Document;

// ---------------------------------------------------------------------------
// Depth limit tests
// ---------------------------------------------------------------------------

#[test]
fn test_deeply_nested_elements_rejected() {
    // Generate 300 nested elements — beyond the default limit of 256.
    // Run in a thread with a larger stack to avoid stack overflow in debug mode.
    let result = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(|| {
            let open_tags: String = (0..300).map(|_| "<a>").collect();
            let close_tags: String = (0..300).map(|_| "</a>").collect();
            let xml = format!("{open_tags}{close_tags}");
            Document::parse_str(&xml)
        })
        .unwrap()
        .join()
        .unwrap();
    assert!(result.is_err(), "deeply nested document should be rejected");
    let err = result.unwrap_err();
    assert!(
        err.message.contains("depth"),
        "error should mention depth: {}",
        err.message
    );
}

#[test]
fn test_depth_limit_configurable() {
    // 5 levels of nesting with a limit of 10 should succeed
    let xml = "<a><b><c><d><e/></d></c></b></a>";
    let opts = ParseOptions::default().max_depth(10);
    let doc = parse_str_with_options(xml, &opts).unwrap();
    assert!(doc.root_element().is_some());
}

#[test]
fn test_depth_limit_exact_boundary() {
    // Exactly at the limit should succeed
    let open: String = (0..3).map(|_| "<a>").collect();
    let close: String = (0..3).map(|_| "</a>").collect();
    let xml = format!("{open}{close}");

    let opts = ParseOptions::default().max_depth(3);
    let result = parse_str_with_options(&xml, &opts);
    assert!(result.is_ok(), "depth exactly at limit should succeed");
}

#[test]
fn test_depth_limit_one_over() {
    // One over the limit should fail
    let open: String = (0..4).map(|_| "<a>").collect();
    let close: String = (0..4).map(|_| "</a>").collect();
    let xml = format!("{open}{close}");

    let opts = ParseOptions::default().max_depth(3);
    let result = parse_str_with_options(&xml, &opts);
    assert!(result.is_err(), "depth one over limit should fail");
}

// ---------------------------------------------------------------------------
// Entity expansion limit tests
// ---------------------------------------------------------------------------

#[test]
fn test_many_entity_references_rejected() {
    // Generate a document with 20,000 entity references — exceeds 10,000 default
    let entities: String = (0..20_000).map(|_| "&amp;").collect();
    let xml = format!("<root>{entities}</root>");

    let result = Document::parse_str(&xml);
    assert!(
        result.is_err(),
        "document with excessive entity references should be rejected"
    );
    let err = result.unwrap_err();
    assert!(
        err.message.contains("entity expansion limit"),
        "error should mention entity expansion: {}",
        err.message
    );
}

#[test]
fn test_entity_expansion_limit_configurable() {
    // 5 entity references with a limit of 100 should succeed
    let xml = "<root>&amp;&lt;&gt;&apos;&quot;</root>";
    let opts = ParseOptions::default().max_entity_expansions(100);
    let result = parse_str_with_options(xml, &opts);
    assert!(result.is_ok());
}

// ---------------------------------------------------------------------------
// Name length limit tests
// ---------------------------------------------------------------------------

#[test]
fn test_huge_element_name_rejected() {
    let name = "a".repeat(100_000);
    let xml = format!("<{name}/>");
    let result = Document::parse_str(&xml);
    assert!(result.is_err(), "element with huge name should be rejected");
    let err = result.unwrap_err();
    assert!(
        err.message.contains("name length"),
        "error should mention name length: {}",
        err.message
    );
}

#[test]
fn test_name_length_limit_configurable() {
    let name = "a".repeat(100);
    let xml = format!("<{name}/>");
    let opts = ParseOptions::default().max_name_length(200);
    let result = parse_str_with_options(&xml, &opts);
    assert!(result.is_ok());
}

// ---------------------------------------------------------------------------
// Default limits are permissive enough for normal documents
// ---------------------------------------------------------------------------

#[test]
fn test_default_limits_allow_normal_documents() {
    let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
    <catalog>
        <book id="1">
            <title>Rust Programming</title>
            <author>Jane &amp; John Doe</author>
            <price>29.99</price>
            <description><![CDATA[A great book about <Rust>]]></description>
        </book>
        <book id="2">
            <title>XML &amp; &lt;HTML&gt; Parsing</title>
            <author>Alice O&apos;Brien</author>
            <price>19.99</price>
            <!-- A comment about this book -->
            <?note review-pending?>
        </book>
    </catalog>"#;

    let result = Document::parse_str(xml);
    assert!(
        result.is_ok(),
        "normal document should parse with default limits"
    );
}

#[test]
fn test_default_limits_allow_moderate_nesting() {
    // 100 levels of nesting should be fine (default limit is 256)
    let open = (0..100).fold(String::new(), |mut s, i| {
        write!(s, "<e{i}>").unwrap();
        s
    });
    let close = (0..100).rev().fold(String::new(), |mut s, i| {
        write!(s, "</e{i}>").unwrap();
        s
    });
    let xml = format!("{open}{close}");

    let result = Document::parse_str(&xml);
    assert!(
        result.is_ok(),
        "100 levels of nesting should work with default limits"
    );
}

#[test]
fn test_default_limits_allow_many_attributes() {
    // 100 attributes should be fine
    let attrs = (0..100).fold(String::new(), |mut s, i| {
        write!(s, " attr{i}=\"value{i}\"").unwrap();
        s
    });
    let xml = format!("<root{attrs}/>");

    let result = Document::parse_str(&xml);
    assert!(
        result.is_ok(),
        "100 attributes should work with default limits"
    );
}

#[test]
fn test_default_limits_allow_moderate_entities() {
    // 500 entity references should be fine (default limit is 10,000)
    let entities: String = (0..500).map(|_| "&amp;").collect();
    let xml = format!("<root>{entities}</root>");

    let result = Document::parse_str(&xml);
    assert!(
        result.is_ok(),
        "500 entity references should work with default limits"
    );
}

// ---------------------------------------------------------------------------
// SAX parser security limits
// ---------------------------------------------------------------------------

#[test]
fn test_sax_depth_limit() {
    use xmloxide::sax::{parse_sax, DefaultHandler};

    let open: String = (0..1000).map(|_| "<a>").collect();
    let close: String = (0..1000).map(|_| "</a>").collect();
    let xml = format!("{open}{close}");

    let mut handler = DefaultHandler;
    let result = parse_sax(&xml, &ParseOptions::default(), &mut handler);
    assert!(result.is_err(), "SAX parser should enforce depth limit");
}

// ---------------------------------------------------------------------------
// Reader security limits
// ---------------------------------------------------------------------------

#[test]
fn test_reader_depth_limit() {
    use xmloxide::reader::XmlReader;

    let open: String = (0..1000).map(|_| "<a>").collect();
    let close: String = (0..1000).map(|_| "</a>").collect();
    let xml = format!("{open}{close}");

    let mut reader = XmlReader::new(&xml);
    let mut hit_error = false;
    while let Ok(true) = reader.read() {
        // keep reading
    }
    // Try to read past the depth limit
    if reader.read().is_err() {
        hit_error = true;
    }
    // The reader should have hit the depth limit at some point during reading.
    // Check by trying to read through the whole document.
    // If we got here without error, check the last read result.
    let mut reader2 = XmlReader::new(&xml);
    loop {
        match reader2.read() {
            Ok(true) => {}
            Ok(false) => break,
            Err(e) => {
                assert!(
                    e.message.contains("depth"),
                    "reader error should mention depth: {}",
                    e.message
                );
                hit_error = true;
                break;
            }
        }
    }
    assert!(hit_error, "reader should enforce depth limit");
}

// ---------------------------------------------------------------------------
// HTML parser security limits
// ---------------------------------------------------------------------------

#[test]
fn test_html_depth_limit() {
    use xmloxide::html::{parse_html_with_options, HtmlParseOptions};

    let open: String = (0..1000).map(|_| "<div>").collect();
    let close: String = (0..1000).map(|_| "</div>").collect();
    let html = format!("{open}{close}");

    let opts = HtmlParseOptions::default();
    let result = parse_html_with_options(&html, &opts);
    // HTML parser may auto-close elements rather than erroring, but the
    // depth limit in ParserInput should still trigger
    assert!(result.is_err(), "HTML parser should enforce depth limit");
}
