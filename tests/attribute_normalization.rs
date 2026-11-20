//! Attribute-value normalization round-trip tests (XML 1.0 §3.3.3, issue #50).
//!
//! Parsing turns literal tab/LF/CR (and CRLF pairs) in attribute values into
//! spaces, while character references (`&#9;`, `&#10;`, `&#13;`) survive as
//! the literal characters. The serializer must therefore write values such
//! that parse → serialize → parse yields the same attribute values.

#![allow(clippy::unwrap_used)]

use pretty_assertions::assert_eq;
use xmloxide::serial::c14n::{canonicalize, C14nOptions};
use xmloxide::serial::serialize;
use xmloxide::Document;

/// Returns `(name, value)` for every attribute on the root element.
fn root_attrs(doc: &Document) -> Vec<(String, String)> {
    let root = doc.root_element().unwrap();
    doc.attributes(root)
        .iter()
        .map(|a| (a.name.clone(), a.value.clone()))
        .collect()
}

/// Parses `src`, serializes, reparses and asserts the attribute values and
/// serialized form are stable. Returns the first-parse attribute values.
fn assert_roundtrip(src: &str) -> Vec<(String, String)> {
    let doc1 = Document::parse_str(src).unwrap();
    let attrs1 = root_attrs(&doc1);
    let xml1 = serialize(&doc1);

    let doc2 = Document::parse_str(&xml1).unwrap();
    let attrs2 = root_attrs(&doc2);
    assert_eq!(attrs1, attrs2, "source {src:?} serialized as {xml1:?}");

    // The serialized form is a fixed point (ignoring the trailing newline
    // after the root element, which is not attribute-related).
    let xml2 = serialize(&doc2);
    assert_eq!(
        xml1.trim_end(),
        xml2.trim_end(),
        "serialization not stable for {src:?}"
    );

    // Canonical form is unchanged by the round-trip.
    let opts = C14nOptions::default();
    assert_eq!(canonicalize(&doc1, &opts), canonicalize(&doc2, &opts));
    attrs1
}

fn attr(name: &str, value: &str) -> (String, String) {
    (name.to_string(), value.to_string())
}

#[test]
fn test_roundtrip_attr_tab_with_entity_ref() {
    // The exact reproducer from issue #50.
    let attrs = assert_roundtrip("<a b='x&amp;y\tz'/>");
    assert_eq!(attrs, vec![attr("b", "x&y z")]);
}

#[test]
fn test_roundtrip_attr_lf_with_entity_ref() {
    let attrs = assert_roundtrip("<a b='x&lt;y\nz'/>");
    assert_eq!(attrs, vec![attr("b", "x<y z")]);
}

#[test]
fn test_roundtrip_attr_cr_with_entity_ref() {
    let attrs = assert_roundtrip("<a b='x&quot;y\rz'/>");
    assert_eq!(attrs, vec![attr("b", "x\"y z")]);
}

#[test]
fn test_roundtrip_attr_crlf_is_single_space() {
    // CRLF is one line break (XML 1.0 §2.11), so one space — with or
    // without an entity reference in the value.
    let attrs = assert_roundtrip("<a b='x&amp;y\r\nz' c='p\r\nq'/>");
    assert_eq!(attrs, vec![attr("b", "x&y z"), attr("c", "p q")]);
}

#[test]
fn test_roundtrip_attr_char_ref_whitespace_preserved() {
    let attrs = assert_roundtrip("<a b='&#9;x&#10;y&#13;z&#x9;' c='&amp;&#9;\t&#xA;\n'/>");
    assert_eq!(attrs, vec![attr("b", "\tx\ny\rz\t"), attr("c", "&\t \n ")]);
}

#[test]
fn test_roundtrip_attr_mixed_custom_entity_refs() {
    let src = "<!DOCTYPE a [<!ENTITY e 'E'><!ENTITY t 'x&amp;y'>]>\
               <a b='&e;\t&amp;\r\n&#9;&e;\r' c='&t;\t&e;&#10;'/>";
    let attrs = assert_roundtrip(src);
    assert_eq!(attrs, vec![attr("b", "E & \tE "), attr("c", "x&y E\n")]);
}

#[test]
fn test_roundtrip_attr_default_value_from_dtd() {
    let src = "<!DOCTYPE a [<!ENTITY e 'E'>\
               <!ATTLIST a d CDATA 'p&amp;q\tr\r\ns&#9;t&e;'\
                           f CDATA #FIXED 'x&#10;y\nz'\
                           n NMTOKENS '\t m \r\n n '>]><a/>";
    let attrs = assert_roundtrip(src);
    assert_eq!(
        attrs,
        vec![
            attr("d", "p&q r s\ttE"),
            attr("f", "x\ny z"),
            attr("n", "m n"),
        ]
    );
}

#[test]
fn test_roundtrip_attr_set_programmatically() {
    // Values set through the API (no raw source text) keep literal
    // whitespace via character references.
    let mut doc = Document::parse_str("<a/>").unwrap();
    let root = doc.root_element().unwrap();
    doc.set_attribute(root, "b", "x\ty\nz\r\n&");
    let xml = serialize(&doc);
    assert!(xml.contains(r#"b="x&#9;y&#10;z&#13;&#10;&amp;""#), "{xml}");
    let doc2 = Document::parse_str(&xml).unwrap();
    assert_eq!(root_attrs(&doc2), vec![attr("b", "x\ty\nz\r\n&")]);
}

#[test]
fn test_c14n_attr_whitespace_escaped() {
    // C14N §2.2: #x9, #xA, #xD in attribute values become &#x9; &#xA; &#xD;.
    let doc = Document::parse_str("<a b='&#9;x&amp;\ty&#10;&#13;\r\n'/>").unwrap();
    let c14n = canonicalize(&doc, &C14nOptions::default());
    assert_eq!(c14n, "<a b=\"&#x9;x&amp; y&#xA;&#xD; \"></a>");
}
