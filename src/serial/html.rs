//! HTML serializer.
//!
//! Serializes a `Document` tree into an HTML string, following libxml2's
//! `htmlSaveFile` behavior. Key differences from XML serialization:
//!
//! - No XML declaration (`<?xml ...?>`)
//! - Void elements use `<br>` syntax (no `/>`)
//! - Non-void empty elements use `<p></p>` (no `<p/>`)
//! - Raw text elements (script, style) are not escaped
//! - Non-ASCII characters are re-encoded as HTML named entities where possible
//! - Formatting newlines around block-level elements

use crate::html::entities::reverse_lookup_entity;
use crate::html::{is_raw_text_element, is_void_element};
use crate::tree::{Document, NodeId, NodeKind};

/// Serializes a document to an HTML string.
///
/// Produces output compatible with libxml2's HTML serialization:
/// - DOCTYPE declaration (if present, or default HTML 4.0 Transitional)
/// - HTML void elements serialized without self-closing slash
/// - Script/style content preserved without escaping
/// - Non-ASCII characters re-encoded as named HTML entities
///
/// # Examples
///
/// ```
/// use xmloxide::html::parse_html;
/// use xmloxide::serial::html::serialize_html;
///
/// let doc = parse_html("<p>Hello</p>").unwrap();
/// let html = serialize_html(&doc);
/// assert!(html.contains("<p>"));
/// ```
#[must_use]
pub fn serialize_html(doc: &Document) -> String {
    let mut output = String::new();

    // Detect whether the document declares UTF-8 charset.
    // If so, non-ASCII characters are preserved as raw UTF-8.
    // Otherwise (default ISO-8859-1), they are re-encoded as named entities.
    let reencode = !detect_utf8_charset(doc);

    // Serialize children of the document root (DOCTYPE, elements, etc.)
    for child in doc.children(doc.root()) {
        serialize_html_node(doc, child, &mut output, reencode);
    }

    // Trailing newline (matches libxml2 output convention)
    if !output.ends_with('\n') {
        output.push('\n');
    }

    output
}

/// Detects whether the document declares a UTF-8 charset via `<meta>` tags.
///
/// Checks for:
/// - `<meta charset="utf-8">`
/// - `<meta http-equiv="Content-Type" content="...charset=utf-8...">`
///
/// When the charset is UTF-8, non-ASCII characters are preserved as raw
/// UTF-8 in the output. Otherwise (default ISO-8859-1 for HTML), they
/// are re-encoded as named HTML entities.
fn detect_utf8_charset(doc: &Document) -> bool {
    let root = doc.root();
    for id in doc.children(root) {
        if check_meta_charset(doc, id) {
            return true;
        }
    }
    false
}

/// Recursively checks an element subtree for meta charset declarations.
fn check_meta_charset(doc: &Document, id: NodeId) -> bool {
    if let NodeKind::Element {
        name, attributes, ..
    } = &doc.node(id).kind
    {
        if name == "meta" {
            // Check <meta charset="utf-8">
            for attr in attributes {
                if attr.name == "charset" && attr.value.eq_ignore_ascii_case("utf-8") {
                    return true;
                }
            }
            // Check <meta http-equiv="Content-Type" content="...charset=utf-8...">
            let is_content_type = attributes
                .iter()
                .any(|a| a.name == "http-equiv" && a.value.eq_ignore_ascii_case("content-type"));
            if is_content_type {
                for attr in attributes {
                    if attr.name == "content" {
                        let lower = attr.value.to_ascii_lowercase();
                        if lower.contains("charset=utf-8") {
                            return true;
                        }
                    }
                }
            }
        }
        // Recurse into children
        for child in doc.children(id) {
            if check_meta_charset(doc, child) {
                return true;
            }
        }
    }
    false
}

/// Returns true if the element is an HTML inline element.
///
/// libxml2 categorizes elements as inline or block-level. Block-level
/// elements get formatting newlines around them in the serialized output.
fn is_inline_element(tag: &str) -> bool {
    matches!(
        tag,
        "a" | "abbr"
            | "acronym"
            | "b"
            | "bdo"
            | "big"
            | "br"
            | "cite"
            | "code"
            | "dfn"
            | "em"
            | "font"
            | "i"
            | "img"
            | "input"
            | "kbd"
            | "label"
            | "q"
            | "s"
            | "samp"
            | "select"
            | "small"
            | "span"
            | "strike"
            | "strong"
            | "sub"
            | "sup"
            | "textarea"
            | "tt"
            | "u"
            | "var"
    )
}

/// Returns true if the node kind is text-like (`Text`, `CData`, or `EntityRef`).
///
/// libxml2 suppresses formatting newlines when adjacent to text-like nodes.
fn is_text_like(kind: &NodeKind) -> bool {
    matches!(
        kind,
        NodeKind::Text { .. } | NodeKind::CData { .. } | NodeKind::EntityRef { .. }
    )
}

/// Checks whether a formatting newline should be added after a block-level
/// element's opening tag (libxml2 behavior).
///
/// Adds `\n` when:
/// - Element is not inline
/// - Element name does not start with 'p' (p, pre, param)
/// - First child is not a text-like node
/// - Element has more than one child
fn maybe_newline_after_open(doc: &Document, id: NodeId, tag: &str, out: &mut String) {
    if is_inline_element(tag) || tag.starts_with('p') {
        return;
    }
    let Some(first) = doc.first_child(id) else {
        return;
    };
    if is_text_like(&doc.node(first).kind) {
        return;
    }
    // Check that the element has more than one child
    if doc.next_sibling(first).is_none() {
        return;
    }
    out.push('\n');
}

/// Checks whether a formatting newline should be added before a block-level
/// element's closing tag (libxml2 behavior).
///
/// Adds `\n` when:
/// - Element is not inline
/// - Element name does not start with 'p' (p, pre, param)
/// - Last child is not a text-like node
/// - Element has more than one child
fn maybe_newline_before_close(doc: &Document, id: NodeId, tag: &str, out: &mut String) {
    if is_inline_element(tag) || tag.starts_with('p') {
        return;
    }
    let Some(first) = doc.first_child(id) else {
        return;
    };
    let Some(last) = doc.last_child(id) else {
        return;
    };
    if is_text_like(&doc.node(last).kind) {
        return;
    }
    // More than one child
    if doc.next_sibling(first).is_none() {
        return;
    }
    out.push('\n');
}

/// Checks whether a formatting newline should be added after a block-level
/// element's closing tag (libxml2 behavior).
///
/// Adds `\n` when:
/// - Element is not inline
/// - Next sibling exists and is not a text-like node
/// - Parent element name does not start with 'p'
fn maybe_newline_after_close(doc: &Document, id: NodeId, tag: &str, out: &mut String) {
    if is_inline_element(tag) {
        return;
    }
    let Some(next) = doc.next_sibling(id) else {
        return;
    };
    if is_text_like(&doc.node(next).kind) {
        return;
    }
    if let Some(parent) = doc.parent(id) {
        let parent_name = doc.node_name(parent).unwrap_or("");
        if parent_name.starts_with('p') {
            return;
        }
    }
    out.push('\n');
}

#[allow(clippy::too_many_lines)]
fn serialize_html_node(doc: &Document, id: NodeId, out: &mut String, reencode: bool) {
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
                // Boolean attributes: output without value when value == name
                if attr.value != attr.name {
                    // Use single quotes when value contains double quotes
                    if attr.value.contains('"') && !attr.value.contains('\'') {
                        out.push_str("='");
                        write_html_escaped_attr_sq(out, &attr.value, reencode);
                        out.push('\'');
                    } else {
                        out.push_str("=\"");
                        if is_uri_attribute(&attr.name) {
                            write_html_uri_attr(out, &attr.value, reencode);
                        } else {
                            write_html_escaped_attr(out, &attr.value, reencode);
                        }
                        out.push('"');
                    }
                }
            }
            out.push('>');

            let lower = name.to_ascii_lowercase();

            // Void elements: no closing tag
            if is_void_element(&lower) {
                maybe_newline_after_close(doc, id, &lower, out);
                return;
            }

            // Formatting newline after opening tag for block elements
            maybe_newline_after_open(doc, id, &lower, out);

            // Raw text elements: output content without escaping
            if is_raw_text_element(&lower) {
                for child in doc.children(id) {
                    if let NodeKind::Text { content } = &doc.node(child).kind {
                        out.push_str(content);
                    } else {
                        serialize_html_node(doc, child, out, reencode);
                    }
                }
            } else {
                for child in doc.children(id) {
                    serialize_html_node(doc, child, out, reencode);
                }
            }

            // Formatting newline before closing tag for block elements
            maybe_newline_before_close(doc, id, &lower, out);

            // Closing tag
            out.push_str("</");
            if let Some(pfx) = prefix {
                out.push_str(pfx);
                out.push(':');
            }
            out.push_str(name);
            out.push('>');

            // Formatting newline after closing tag for block elements
            maybe_newline_after_close(doc, id, &lower, out);
        }
        NodeKind::Text { content } => {
            write_html_escaped_text(out, content, reencode);
        }
        NodeKind::CData { content } => {
            // In HTML, CDATA is not standard — output as text
            write_html_escaped_text(out, content, reencode);
        }
        NodeKind::Comment { content } => {
            out.push_str("<!--");
            out.push_str(content);
            out.push_str("-->");
        }
        NodeKind::ProcessingInstruction { target, data } => {
            // HTML PIs use '>' as terminator, not '?>' (XML style)
            out.push_str("<?");
            out.push_str(target);
            if let Some(d) = data {
                out.push(' ');
                out.push_str(d);
            }
            out.push('>');
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
            ..
        } => {
            out.push_str("<!DOCTYPE ");
            out.push_str(name);
            match (public_id, system_id) {
                (Some(pub_id), Some(sys_id)) => {
                    out.push_str(" PUBLIC \"");
                    out.push_str(pub_id);
                    out.push('"');
                    if !sys_id.is_empty() {
                        out.push_str(" \"");
                        out.push_str(sys_id);
                        out.push('"');
                    }
                }
                (Some(pub_id), None) => {
                    out.push_str(" PUBLIC \"");
                    out.push_str(pub_id);
                    out.push('"');
                }
                (None, Some(sys_id)) => {
                    out.push_str(" SYSTEM \"");
                    out.push_str(sys_id);
                    out.push('"');
                }
                _ => {}
            }
            out.push_str(">\n");
        }
        NodeKind::Document => {
            // Should not appear as a child node
        }
    }
}

/// Escapes text content for HTML output.
///
/// - `&` → `&amp;`
/// - `<` → `&lt;`
/// - `>` → `&gt;`
/// - Non-ASCII characters with known HTML entities → `&name;` (when `reencode` is true)
fn write_html_escaped_text(out: &mut String, text: &str, reencode: bool) {
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            c if reencode && (c as u32) >= 0x80 => {
                if let Some(name) = reverse_lookup_entity(c) {
                    out.push('&');
                    out.push_str(name);
                    out.push(';');
                } else {
                    out.push(c);
                }
            }
            _ => out.push(ch),
        }
    }
}

/// Returns true if the attribute name is a URI-type attribute that should
/// have its value URL-encoded (spaces → `%20`, etc.).
fn is_uri_attribute(name: &str) -> bool {
    matches!(
        name,
        "href"
            | "src"
            | "action"
            | "background"
            | "cite"
            | "classid"
            | "codebase"
            | "data"
            | "longdesc"
            | "profile"
            | "usemap"
    )
}

/// Writes a URI attribute value with URL encoding for non-URI characters.
///
/// Spaces are encoded as `%20`. HTML-special characters (`&`, `<`, `>`)
/// are entity-escaped. Non-ASCII characters are handled based on the
/// `reencode` flag.
fn write_html_uri_attr(out: &mut String, text: &str, reencode: bool) {
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            ' ' => out.push_str("%20"),
            c if reencode && (c as u32) >= 0x80 => {
                if let Some(name) = reverse_lookup_entity(c) {
                    out.push('&');
                    out.push_str(name);
                    out.push(';');
                } else {
                    out.push(c);
                }
            }
            _ => out.push(ch),
        }
    }
}

/// Escapes an attribute value for HTML output (single-quote delimited).
///
/// Used when the value contains `"` characters and is delimited by `'`.
/// - `&` → `&amp;`
/// - `'` → `&#39;`
/// - `<` → `&lt;`
/// - `>` → `&gt;`
/// - Non-ASCII characters with known HTML entities → `&name;` (when `reencode` is true)
fn write_html_escaped_attr_sq(out: &mut String, text: &str, reencode: bool) {
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '\'' => out.push_str("&#39;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            c if reencode && (c as u32) >= 0x80 => {
                if let Some(name) = reverse_lookup_entity(c) {
                    out.push('&');
                    out.push_str(name);
                    out.push(';');
                } else {
                    out.push(c);
                }
            }
            _ => out.push(ch),
        }
    }
}

/// Escapes an attribute value for HTML output.
///
/// - `&` → `&amp;`
/// - `"` → `&quot;`
/// - `<` → `&lt;`
/// - `>` → `&gt;`
/// - Non-ASCII characters with known HTML entities → `&name;` (when `reencode` is true)
fn write_html_escaped_attr(out: &mut String, text: &str, reencode: bool) {
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            c if reencode && (c as u32) >= 0x80 => {
                if let Some(name) = reverse_lookup_entity(c) {
                    out.push('&');
                    out.push_str(name);
                    out.push(';');
                } else {
                    out.push(c);
                }
            }
            _ => out.push(ch),
        }
    }
}
