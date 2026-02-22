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

    // XML declaration
    if let Some(ref version) = doc.version {
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
        output.push_str("?>");
    }

    // Serialize children of the document root
    for child in doc.children(doc.root()) {
        serialize_node(doc, child, &mut output);
    }

    output
}

fn serialize_node(doc: &Document, id: NodeId, out: &mut String) {
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
                write_escaped_attr(out, &attr.value);
                out.push('"');
            }

            // Check for self-closing
            if doc.first_child(id).is_none() {
                out.push_str("/>");
            } else {
                out.push('>');
                for child in doc.children(id) {
                    serialize_node(doc, child, out);
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
            write_escaped_text(out, content);
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
        NodeKind::EntityRef { name } => {
            out.push('&');
            out.push_str(name);
            out.push(';');
        }
        NodeKind::DocumentType {
            name,
            system_id,
            public_id,
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
            out.push('>');
        }
        NodeKind::Document => {
            // Should not appear as a child node
        }
    }
}

/// Escapes text content for XML output.
fn write_escaped_text(out: &mut String, text: &str) {
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            _ => out.push(ch),
        }
    }
}

/// Escapes attribute values for XML output.
fn write_escaped_attr(out: &mut String, text: &str) {
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
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
        assert_eq!(serialize(&doc), "<br/>");
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
        assert_eq!(serialize(&doc), "<p>Hello</p>");
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
                },
                Attribute {
                    name: "class".to_string(),
                    value: "big".to_string(),
                    prefix: None,
                    namespace: None,
                },
            ],
        });
        doc.append_child(root, elem);
        assert_eq!(serialize(&doc), "<div id=\"main\" class=\"big\"/>");
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
        assert_eq!(serialize(&doc), "<p>a &lt; b &amp; c &gt; d</p>");
    }

    #[test]
    fn test_serialize_comment() {
        let mut doc = Document::new();
        let root = doc.root();
        let comment = doc.create_node(NodeKind::Comment {
            content: " a comment ".to_string(),
        });
        doc.append_child(root, comment);
        assert_eq!(serialize(&doc), "<!-- a comment -->");
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
            "<script><![CDATA[x < 1 && y > 2]]></script>"
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
            "<?xml-stylesheet type=\"text/css\" href=\"style.css\"?>"
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
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
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
            }],
        });
        doc.append_child(root, elem);
        assert_eq!(
            serialize(&doc),
            "<a title=\"He said &quot;hello&quot; &amp; &lt;bye&gt;\"/>"
        );
    }
}
