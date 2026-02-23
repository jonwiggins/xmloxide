//! XML serializer.
//!
//! Serializes a `Document` tree into a well-formed XML string.

use crate::tree::{Document, NodeId, NodeKind};

/// Serializes a document to an XML string.
///
/// # Examples
///
/// ```
/// use xmloxide::Document;
/// use xmloxide::serial::serialize;
///
/// let doc = Document::parse_str("<root><child>Hello</child></root>").unwrap();
/// let xml = serialize(&doc);
/// assert!(xml.contains("<root>"));
/// ```
#[must_use]
pub fn serialize(doc: &Document) -> String {
    let mut output = String::new();

    // XML declaration — always emit, defaulting to version 1.0 (matches libxml2)
    let version = doc.version.as_deref().unwrap_or("1.0");
    output.push_str("<?xml version=\"");
    output.push_str(version);
    output.push('"');
    if let Some(ref encoding) = doc.encoding {
        output.push_str(" encoding=\"");
        output.push_str(encoding);
        output.push('"');
    }
    if let Some(standalone) = doc.standalone {
        output.push_str(" standalone=\"");
        output.push_str(if standalone { "yes" } else { "no" });
        output.push('"');
    }
    output.push_str("?>\n");

    // When no encoding is declared, non-ASCII chars in attributes are
    // re-encoded as hex character references (matches libxml2 behavior).
    let reencode_non_ascii = doc.encoding.is_none();

    // Serialize children of the document root
    for child in doc.children(doc.root()) {
        serialize_node(doc, child, &mut output, reencode_non_ascii);
    }

    // Trailing newline (matches libxml2 output convention)
    output.push('\n');

    output
}

#[allow(clippy::too_many_lines)]
fn serialize_node(doc: &Document, id: NodeId, out: &mut String, reencode_non_ascii: bool) {
    match &doc.node(id).kind {
        NodeKind::Element {
            name,
            prefix,
            attributes,
            ..
        } => {
            out.push('<');
            if let Some(pfx) = prefix {
                out.push_str(pfx);
                out.push(':');
            }
            out.push_str(name);

            for attr in attributes {
                out.push(' ');
                if let Some(pfx) = &attr.prefix {
                    out.push_str(pfx);
                    out.push(':');
                }
                out.push_str(&attr.name);
                out.push_str("=\"");
                // Use raw_value (with entity references preserved) when available,
                // but NOT for namespace declarations — namespace URIs should
                // always use the fully expanded value.
                let is_ns_decl = attr.name == "xmlns" || attr.prefix.as_deref() == Some("xmlns");
                if let Some(raw) = attr.raw_value.as_ref().filter(|_| !is_ns_decl) {
                    write_escaped_attr_preserve_refs(out, raw, reencode_non_ascii);
                } else {
                    write_escaped_attr(out, &attr.value, reencode_non_ascii);
                }
                out.push('"');
            }

            // Check for self-closing
            if doc.first_child(id).is_none() {
                out.push_str("/>");
            } else {
                out.push('>');
                for child in doc.children(id) {
                    serialize_node(doc, child, out, reencode_non_ascii);
                }
                out.push_str("</");
                if let Some(pfx) = prefix {
                    out.push_str(pfx);
                    out.push(':');
                }
                out.push_str(name);
                out.push('>');
            }
        }
        NodeKind::Text { content } => {
            write_escaped_text(out, content, reencode_non_ascii);
        }
        NodeKind::CData { content } => {
            out.push_str("<![CDATA[");
            out.push_str(content);
            out.push_str("]]>");
        }
        NodeKind::Comment { content } => {
            out.push_str("<!--");
            out.push_str(content);
            out.push_str("-->");
        }
        NodeKind::ProcessingInstruction { target, data } => {
            out.push_str("<?");
            out.push_str(target);
            if let Some(d) = data {
                out.push(' ');
                out.push_str(d);
            }
            out.push_str("?>");
        }
        NodeKind::EntityRef { name, .. } => {
            out.push('&');
            out.push_str(name);
            out.push(';');
        }
        NodeKind::DocumentType {
            name,
            system_id,
            public_id,
            internal_subset,
        } => {
            out.push_str("<!DOCTYPE ");
            out.push_str(name);
            match (public_id, system_id) {
                (Some(pub_id), Some(sys_id)) => {
                    out.push_str(" PUBLIC \"");
                    out.push_str(pub_id);
                    out.push_str("\" \"");
                    out.push_str(sys_id);
                    out.push('"');
                }
                (None, Some(sys_id)) => {
                    out.push_str(" SYSTEM \"");
                    out.push_str(sys_id);
                    out.push('"');
                }
                _ => {}
            }
            if let Some(ref subset) = internal_subset {
                out.push_str(" [");
                out.push_str(subset);
                out.push_str("]>");
            } else {
                out.push('>');
            }
        }
        NodeKind::Document => {
            // Should not appear as a child node
        }
    }
}

/// Writes a hexadecimal character reference (`&#xHH;`) for a Unicode code point.
fn write_hex_char_ref(out: &mut String, ch: char) {
    use std::fmt::Write;
    let _ = write!(out, "&#x{:X};", ch as u32);
}

/// Escapes text content for XML output.
///
/// Follows libxml2's `xmlEscapeEntities` behavior:
/// - `<`, `>`, `&` are escaped with named entity references
/// - `\r` is encoded as `&#13;`
/// - `\t` and `\n` are passed through
/// - Control characters below 0x20 (other than `\t`, `\n`, `\r`) are hex-encoded
/// - Non-ASCII characters are passed through as raw UTF-8
fn write_escaped_text(out: &mut String, text: &str, reencode_non_ascii: bool) {
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '\r' => out.push_str("&#13;"),
            '\t' | '\n' => out.push(ch),
            c if (c as u32) < 0x20 => write_hex_char_ref(out, c),
            c if reencode_non_ascii && (c as u32) >= 0x80 => write_hex_char_ref(out, c),
            _ => out.push(ch),
        }
    }
}

/// Escapes an attribute value that contains preserved entity references.
///
/// Custom entity references (`&name;` where name is not a builtin) are
/// preserved as-is. Character references (`&#...;`) and builtin entity refs
/// (`&amp;`, `&lt;`, `&gt;`, `&apos;`, `&quot;`) are decoded to their
/// actual characters and then re-escaped normally through the attribute
/// escaping logic. This matches libxml2's serialization behavior.
fn write_escaped_attr_preserve_refs(out: &mut String, text: &str, reencode_non_ascii: bool) {
    let bytes = text.as_bytes();
    let len = bytes.len();
    let mut i = 0;

    while i < len {
        let b = bytes[i];
        if b == b'&' {
            // Check if this starts a valid reference
            if let Some(ref_end) = find_attr_reference_end(bytes, i) {
                let ref_str = &text[i..=ref_end];
                if ref_str.starts_with("&#") {
                    // Character reference — decode to char, then escape normally
                    if let Some(ch) = decode_char_ref(ref_str) {
                        write_escaped_attr_char(out, ch, reencode_non_ascii);
                    } else {
                        out.push_str(ref_str);
                    }
                } else if let Some(ch) = decode_builtin_entity_ref(ref_str) {
                    // Builtin entity ref — decode and re-escape normally
                    write_escaped_attr_char(out, ch, reencode_non_ascii);
                } else {
                    // Custom entity reference — preserve as-is
                    out.push_str(ref_str);
                }
                i = ref_end + 1;
                continue;
            }
            out.push_str("&amp;");
            i += 1;
        } else if b == b'<' {
            out.push_str("&lt;");
            i += 1;
        } else if b == b'>' {
            out.push_str("&gt;");
            i += 1;
        } else if b == b'"' {
            out.push_str("&quot;");
            i += 1;
        } else if b == b'\t' {
            out.push_str("&#9;");
            i += 1;
        } else if b == b'\n' {
            out.push_str("&#10;");
            i += 1;
        } else if b == b'\r' {
            out.push_str("&#13;");
            i += 1;
        } else {
            let ch = &text[i..];
            if let Some(c) = ch.chars().next() {
                if (c as u32) < 0x20 || (reencode_non_ascii && (c as u32) >= 0x80) {
                    write_hex_char_ref(out, c);
                } else {
                    out.push(c);
                }
                i += c.len_utf8();
            } else {
                i += 1;
            }
        }
    }
}

/// Decodes a character reference (`&#NNN;` or `&#xHHH;`) to a `char`.
fn decode_char_ref(s: &str) -> Option<char> {
    let inner = s.strip_prefix("&#")?.strip_suffix(';')?;
    let code_point = if let Some(hex) = inner.strip_prefix('x') {
        u32::from_str_radix(hex, 16).ok()?
    } else {
        inner.parse::<u32>().ok()?
    };
    char::from_u32(code_point)
}

/// Decodes a builtin entity reference to a `char`, if it is one.
/// Returns `None` for custom (non-builtin) entity references.
fn decode_builtin_entity_ref(s: &str) -> Option<char> {
    match s {
        "&amp;" => Some('&'),
        "&lt;" => Some('<'),
        "&gt;" => Some('>'),
        "&apos;" => Some('\''),
        "&quot;" => Some('"'),
        _ => None,
    }
}

/// Writes a single character with attribute escaping rules.
fn write_escaped_attr_char(out: &mut String, ch: char, reencode_non_ascii: bool) {
    match ch {
        '&' => out.push_str("&amp;"),
        '<' => out.push_str("&lt;"),
        '>' => out.push_str("&gt;"),
        '"' => out.push_str("&quot;"),
        '\t' => out.push_str("&#9;"),
        '\n' => out.push_str("&#10;"),
        '\r' => out.push_str("&#13;"),
        c if (c as u32) < 0x20 => write_hex_char_ref(out, c),
        c if reencode_non_ascii && (c as u32) >= 0x80 => write_hex_char_ref(out, c),
        _ => out.push(ch),
    }
}

/// Finds the end of an entity/character reference in attribute raw value.
fn find_attr_reference_end(bytes: &[u8], start: usize) -> Option<usize> {
    if start >= bytes.len() || bytes[start] != b'&' {
        return None;
    }
    let mut i = start + 1;
    if i >= bytes.len() {
        return None;
    }
    if bytes[i] == b'#' {
        i += 1;
        if i >= bytes.len() {
            return None;
        }
        if bytes[i] == b'x' {
            i += 1;
            let d = i;
            while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
                i += 1;
            }
            if i == d || i >= bytes.len() || bytes[i] != b';' {
                return None;
            }
        } else {
            let d = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i == d || i >= bytes.len() || bytes[i] != b';' {
                return None;
            }
        }
        Some(i)
    } else {
        if !bytes[i].is_ascii_alphabetic() && bytes[i] != b'_' && bytes[i] != b':' {
            return None;
        }
        i += 1;
        while i < bytes.len()
            && (bytes[i].is_ascii_alphanumeric()
                || bytes[i] == b'_'
                || bytes[i] == b':'
                || bytes[i] == b'-'
                || bytes[i] == b'.')
        {
            i += 1;
        }
        if i >= bytes.len() || bytes[i] != b';' {
            return None;
        }
        Some(i)
    }
}

/// Escapes attribute values for XML output.
///
/// Follows libxml2's `xmlAttrSerializeTxtContent` behavior:
/// - `<`, `>`, `&`, `"` are escaped with named entity references
/// - `\t` → `&#9;`, `\n` → `&#10;`, `\r` → `&#13;`
/// - When `reencode_non_ascii` is true (doc has no declared encoding), non-ASCII
///   characters (>= U+0080) are encoded as hex character references
fn write_escaped_attr(out: &mut String, text: &str, reencode_non_ascii: bool) {
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\t' => out.push_str("&#9;"),
            '\n' => out.push_str("&#10;"),
            '\r' => out.push_str("&#13;"),
            c if (c as u32) < 0x20 => write_hex_char_ref(out, c),
            c if reencode_non_ascii && (c as u32) >= 0x80 => write_hex_char_ref(out, c),
            _ => out.push(ch),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tree::Attribute;

    #[test]
    fn test_serialize_empty_element() {
        let mut doc = Document::new();
        let root = doc.root();
        let elem = doc.create_node(NodeKind::Element {
            name: "br".to_string(),
            prefix: None,
            namespace: None,
            attributes: vec![],
        });
        doc.append_child(root, elem);
        assert_eq!(serialize(&doc), "<?xml version=\"1.0\"?>\n<br/>\n");
    }

    #[test]
    fn test_serialize_element_with_text() {
        let mut doc = Document::new();
        let root = doc.root();
        let elem = doc.create_node(NodeKind::Element {
            name: "p".to_string(),
            prefix: None,
            namespace: None,
            attributes: vec![],
        });
        let text = doc.create_node(NodeKind::Text {
            content: "Hello".to_string(),
        });
        doc.append_child(root, elem);
        doc.append_child(elem, text);
        assert_eq!(serialize(&doc), "<?xml version=\"1.0\"?>\n<p>Hello</p>\n");
    }

    #[test]
    fn test_serialize_element_with_attributes() {
        let mut doc = Document::new();
        let root = doc.root();
        let elem = doc.create_node(NodeKind::Element {
            name: "div".to_string(),
            prefix: None,
            namespace: None,
            attributes: vec![
                Attribute {
                    name: "id".to_string(),
                    value: "main".to_string(),
                    prefix: None,
                    namespace: None,
                    raw_value: None,
                },
                Attribute {
                    name: "class".to_string(),
                    value: "big".to_string(),
                    prefix: None,
                    namespace: None,
                    raw_value: None,
                },
            ],
        });
        doc.append_child(root, elem);
        assert_eq!(
            serialize(&doc),
            "<?xml version=\"1.0\"?>\n<div id=\"main\" class=\"big\"/>\n"
        );
    }

    #[test]
    fn test_serialize_escaping() {
        let mut doc = Document::new();
        let root = doc.root();
        let elem = doc.create_node(NodeKind::Element {
            name: "p".to_string(),
            prefix: None,
            namespace: None,
            attributes: vec![],
        });
        let text = doc.create_node(NodeKind::Text {
            content: "a < b & c > d".to_string(),
        });
        doc.append_child(root, elem);
        doc.append_child(elem, text);
        assert_eq!(
            serialize(&doc),
            "<?xml version=\"1.0\"?>\n<p>a &lt; b &amp; c &gt; d</p>\n"
        );
    }

    #[test]
    fn test_serialize_comment() {
        let mut doc = Document::new();
        let root = doc.root();
        let comment = doc.create_node(NodeKind::Comment {
            content: " a comment ".to_string(),
        });
        doc.append_child(root, comment);
        assert_eq!(
            serialize(&doc),
            "<?xml version=\"1.0\"?>\n<!-- a comment -->\n"
        );
    }

    #[test]
    fn test_serialize_cdata() {
        let mut doc = Document::new();
        let root = doc.root();
        let elem = doc.create_node(NodeKind::Element {
            name: "script".to_string(),
            prefix: None,
            namespace: None,
            attributes: vec![],
        });
        let cdata = doc.create_node(NodeKind::CData {
            content: "x < 1 && y > 2".to_string(),
        });
        doc.append_child(root, elem);
        doc.append_child(elem, cdata);
        assert_eq!(
            serialize(&doc),
            "<?xml version=\"1.0\"?>\n<script><![CDATA[x < 1 && y > 2]]></script>\n"
        );
    }

    #[test]
    fn test_serialize_processing_instruction() {
        let mut doc = Document::new();
        let root = doc.root();
        let pi = doc.create_node(NodeKind::ProcessingInstruction {
            target: "xml-stylesheet".to_string(),
            data: Some("type=\"text/css\" href=\"style.css\"".to_string()),
        });
        doc.append_child(root, pi);
        assert_eq!(
            serialize(&doc),
            "<?xml version=\"1.0\"?>\n<?xml-stylesheet type=\"text/css\" href=\"style.css\"?>\n"
        );
    }

    #[test]
    fn test_serialize_xml_declaration() {
        let mut doc = Document::new();
        doc.version = Some("1.0".to_string());
        doc.encoding = Some("UTF-8".to_string());
        let root = doc.root();
        let elem = doc.create_node(NodeKind::Element {
            name: "root".to_string(),
            prefix: None,
            namespace: None,
            attributes: vec![],
        });
        doc.append_child(root, elem);
        assert_eq!(
            serialize(&doc),
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<root/>\n"
        );
    }

    #[test]
    fn test_serialize_attr_escaping() {
        let mut doc = Document::new();
        let root = doc.root();
        let elem = doc.create_node(NodeKind::Element {
            name: "a".to_string(),
            prefix: None,
            namespace: None,
            attributes: vec![Attribute {
                name: "title".to_string(),
                value: "He said \"hello\" & <bye>".to_string(),
                prefix: None,
                namespace: None,
                raw_value: None,
            }],
        });
        doc.append_child(root, elem);
        assert_eq!(
            serialize(&doc),
            "<?xml version=\"1.0\"?>\n<a title=\"He said &quot;hello&quot; &amp; &lt;bye&gt;\"/>\n"
        );
    }
}
