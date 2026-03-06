//! Custom HTML5 parser integration tests.
//!
//! These supplement the html5lib-tests suite with xmloxide-specific scenarios:
//! roundtrip tests, `XPath` on HTML5 trees, real-world HTML5 documents, etc.

#![allow(clippy::unwrap_used)]

use xmloxide::html5::parse_html5;
use xmloxide::tree::{Document, NodeId, NodeKind};

// -------------------------------------------------------------------------
// Helpers
// -------------------------------------------------------------------------

/// Collects child element names under a given node.
fn child_element_names(doc: &Document, parent: NodeId) -> Vec<String> {
    let mut names = Vec::new();
    let mut child = doc.node(parent).first_child;
    while let Some(id) = child {
        if let NodeKind::Element { name, .. } = &doc.node(id).kind {
            names.push(name.clone());
        }
        child = doc.node(id).next_sibling;
    }
    names
}

/// Finds the first descendant element with the given name.
fn find_element(doc: &Document, root: NodeId, target: &str) -> Option<NodeId> {
    let mut stack = vec![root];
    while let Some(id) = stack.pop() {
        if doc.node_name(id) == Some(target) {
            return Some(id);
        }
        // Push children in reverse order for DFS
        let mut children = Vec::new();
        let mut child = doc.node(id).first_child;
        while let Some(c) = child {
            children.push(c);
            child = doc.node(c).next_sibling;
        }
        for c in children.into_iter().rev() {
            stack.push(c);
        }
    }
    None
}

// -------------------------------------------------------------------------
// Basic structure tests
// -------------------------------------------------------------------------

#[test]
fn test_html5_minimal_document() {
    let doc = parse_html5("<!DOCTYPE html><html><head></head><body></body></html>").unwrap();
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("html"));
    let children = child_element_names(&doc, root);
    assert_eq!(children, vec!["head", "body"]);
}

#[test]
fn test_html5_implied_structure() {
    // Even bare text should get wrapped in html>head+body
    let doc = parse_html5("Hello world").unwrap();
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("html"));
    let body = find_element(&doc, root, "body").unwrap();
    assert_eq!(doc.text_content(body), "Hello world");
}

#[test]
fn test_html5_doctype_html5() {
    let doc = parse_html5("<!DOCTYPE html><p>Hi</p>").unwrap();
    // Should have a DocumentType node as first child of document root
    let doc_root = doc.root();
    let first = doc.node(doc_root).first_child.unwrap();
    if let NodeKind::DocumentType { name, .. } = &doc.node(first).kind {
        assert_eq!(name, "html");
    } else {
        panic!("Expected DocumentType node, got {:?}", doc.node(first).kind);
    }
}

// -------------------------------------------------------------------------
// HTML5 semantic elements
// -------------------------------------------------------------------------

#[test]
fn test_html5_semantic_elements() {
    let html = r#"<!DOCTYPE html>
<html>
<body>
<header><h1>Title</h1></header>
<nav><a href="/">Home</a></nav>
<main>
  <article>
    <section><p>Content</p></section>
  </article>
  <aside>Sidebar</aside>
</main>
<footer>Footer</footer>
</body>
</html>"#;
    let doc = parse_html5(html).unwrap();
    let body = find_element(&doc, doc.root(), "body").unwrap();
    let children = child_element_names(&doc, body);
    // Should contain all HTML5 semantic elements
    assert!(children.contains(&"header".to_string()));
    assert!(children.contains(&"nav".to_string()));
    assert!(children.contains(&"main".to_string()));
    assert!(children.contains(&"footer".to_string()));

    let main_el = find_element(&doc, body, "main").unwrap();
    let main_children = child_element_names(&doc, main_el);
    assert!(main_children.contains(&"article".to_string()));
    assert!(main_children.contains(&"aside".to_string()));
}

// -------------------------------------------------------------------------
// Void elements
// -------------------------------------------------------------------------

#[test]
fn test_html5_void_elements() {
    let doc = parse_html5("<p>Line 1<br>Line 2<br>Line 3</p>").unwrap();
    let p = find_element(&doc, doc.root(), "p").unwrap();
    // p should have: "Line 1", <br>, "Line 2", <br>, "Line 3"
    let mut count = 0;
    let mut child = doc.node(p).first_child;
    while let Some(id) = child {
        count += 1;
        child = doc.node(id).next_sibling;
    }
    assert_eq!(count, 5, "Expected 5 children (3 text + 2 br)");
}

#[test]
fn test_html5_img_with_attributes() {
    let doc = parse_html5(r#"<img src="test.png" alt="Test" width="100">"#).unwrap();
    let img = find_element(&doc, doc.root(), "img").unwrap();
    assert_eq!(doc.attribute(img, "src"), Some("test.png"));
    assert_eq!(doc.attribute(img, "alt"), Some("Test"));
    assert_eq!(doc.attribute(img, "width"), Some("100"));
    // img should have no children
    assert!(doc.node(img).first_child.is_none());
}

// -------------------------------------------------------------------------
// Auto-closing and nesting
// -------------------------------------------------------------------------

#[test]
fn test_html5_p_auto_close() {
    let doc = parse_html5("<p>First<p>Second<p>Third").unwrap();
    let body = find_element(&doc, doc.root(), "body").unwrap();
    let p_elements = child_element_names(&doc, body);
    assert_eq!(p_elements, vec!["p", "p", "p"]);
}

#[test]
fn test_html5_li_auto_close() {
    let doc = parse_html5("<ul><li>A<li>B<li>C</ul>").unwrap();
    let ul = find_element(&doc, doc.root(), "ul").unwrap();
    let items = child_element_names(&doc, ul);
    assert_eq!(items, vec!["li", "li", "li"]);
}

#[test]
fn test_html5_dt_dd_auto_close() {
    let doc = parse_html5("<dl><dt>Term<dd>Definition<dt>Term2<dd>Def2</dl>").unwrap();
    let dl = find_element(&doc, doc.root(), "dl").unwrap();
    let items = child_element_names(&doc, dl);
    assert_eq!(items, vec!["dt", "dd", "dt", "dd"]);
}

// -------------------------------------------------------------------------
// Raw text elements
// -------------------------------------------------------------------------

#[test]
fn test_html5_script_raw_text() {
    let doc = parse_html5("<script>var x = '<p>not an element</p>';</script><p>After</p>").unwrap();
    let script = find_element(&doc, doc.root(), "script").unwrap();
    assert_eq!(doc.text_content(script), "var x = '<p>not an element</p>';");
    // The <p> after </script> should be a real element
    let p = find_element(&doc, doc.root(), "p").unwrap();
    assert_eq!(doc.text_content(p), "After");
}

#[test]
fn test_html5_style_raw_text() {
    let doc = parse_html5("<style>p { color: red; }</style>").unwrap();
    let style = find_element(&doc, doc.root(), "style").unwrap();
    assert_eq!(doc.text_content(style), "p { color: red; }");
}

#[test]
fn test_html5_textarea_rcdata() {
    let doc = parse_html5("<textarea>Some &amp; text with <b>tags</b></textarea>").unwrap();
    let textarea = find_element(&doc, doc.root(), "textarea").unwrap();
    // In RCDATA mode, tags are NOT parsed, but entities ARE resolved
    let content = doc.text_content(textarea);
    assert!(content.contains("Some & text with <b>tags</b>"));
}

// -------------------------------------------------------------------------
// Character references
// -------------------------------------------------------------------------

#[test]
fn test_html5_named_entities() {
    let doc = parse_html5("<p>&amp; &lt; &gt; &nbsp; &copy;</p>").unwrap();
    let p = find_element(&doc, doc.root(), "p").unwrap();
    let text = doc.text_content(p);
    assert!(text.contains('&'));
    assert!(text.contains('<'));
    assert!(text.contains('>'));
    assert!(text.contains('\u{00A0}')); // nbsp
    assert!(text.contains('\u{00A9}')); // copyright
}

#[test]
fn test_html5_numeric_entities() {
    let doc = parse_html5("<p>&#65;&#x42;&#169;</p>").unwrap();
    let p = find_element(&doc, doc.root(), "p").unwrap();
    let text = doc.text_content(p);
    assert!(text.contains('A')); // &#65;
    assert!(text.contains('B')); // &#x42;
    assert!(text.contains('\u{00A9}')); // &#169; (copyright)
}

// -------------------------------------------------------------------------
// Tables
// -------------------------------------------------------------------------

#[test]
fn test_html5_table_auto_tbody() {
    let doc = parse_html5("<table><tr><td>Cell</td></tr></table>").unwrap();
    let table = find_element(&doc, doc.root(), "table").unwrap();
    // HTML5 spec auto-inserts tbody
    let children = child_element_names(&doc, table);
    assert!(
        children.contains(&"tbody".to_string()),
        "Expected auto-inserted tbody, got: {children:?}"
    );
}

// -------------------------------------------------------------------------
// Comments
// -------------------------------------------------------------------------

#[test]
fn test_html5_comments_preserved() {
    let doc = parse_html5("<!-- before --><p>text</p><!-- after -->").unwrap();
    let root = doc.root();
    let mut has_before = false;
    let mut has_after = false;
    let mut child = doc.node(root).first_child;
    while let Some(id) = child {
        if let NodeKind::Comment { content } = &doc.node(id).kind {
            if content.trim() == "before" {
                has_before = true;
            }
            if content.trim() == "after" {
                has_after = true;
            }
        }
        child = doc.node(id).next_sibling;
    }
    // At least the "before" comment should be somewhere in the tree
    // (exact placement depends on insertion mode)
    assert!(
        has_before || has_after,
        "Expected at least one comment to be preserved"
    );
}

// -------------------------------------------------------------------------
// Same Document type as HTML 4.01 and XML
// -------------------------------------------------------------------------

#[test]
fn test_html5_produces_same_document_type() {
    let doc5 = parse_html5("<p>Hello</p>").unwrap();
    let doc4 = xmloxide::html::parse_html("<p>Hello</p>").unwrap();

    // Both produce a Document with an html root element
    let root5 = doc5.root_element().unwrap();
    let root4 = doc4.root_element().unwrap();
    assert_eq!(doc5.node_name(root5), Some("html"));
    assert_eq!(doc4.node_name(root4), Some("html"));
}

// -------------------------------------------------------------------------
// XPath on HTML5-parsed documents
// -------------------------------------------------------------------------

#[test]
fn test_html5_xpath_works() {
    let doc = parse_html5("<html><body><p>Hello</p><p>World</p></body></html>").unwrap();
    let result = xmloxide::xpath::evaluate(&doc, doc.root(), "//p").unwrap();
    if let xmloxide::xpath::XPathValue::NodeSet(nodes) = &result {
        assert_eq!(nodes.len(), 2);
    } else {
        panic!("Expected NodeSet from XPath");
    }
}

// -------------------------------------------------------------------------
// Real-world-ish HTML5
// -------------------------------------------------------------------------

#[test]
fn test_html5_real_world_snippet() {
    let html = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Test Page</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <div id="app">
        <h1>Welcome</h1>
        <p>This is a <strong>test</strong> page with <em>formatting</em>.</p>
        <ul>
            <li>Item 1
            <li>Item 2
            <li>Item 3
        </ul>
        <img src="photo.jpg" alt="Photo">
        <br>
        <input type="text" placeholder="Enter text" disabled>
    </div>
    <script>console.log("hello");</script>
</body>
</html>"#;
    let doc = parse_html5(html).unwrap();
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("html"));
    assert_eq!(doc.attribute(root, "lang"), Some("en"));

    let title = find_element(&doc, root, "title").unwrap();
    assert_eq!(doc.text_content(title), "Test Page");

    let ul = find_element(&doc, root, "ul").unwrap();
    let li_count = child_element_names(&doc, ul)
        .iter()
        .filter(|n| *n == "li")
        .count();
    assert_eq!(li_count, 3);

    let script = find_element(&doc, root, "script").unwrap();
    assert_eq!(doc.text_content(script), "console.log(\"hello\");");
}

// -------------------------------------------------------------------------
// Error recovery
// -------------------------------------------------------------------------

#[test]
fn test_html5_unclosed_tags() {
    // Unclosed tags should be handled gracefully
    let doc = parse_html5("<div><p>Unclosed paragraph<div>New div</div>").unwrap();
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("html"));
    // Should produce a tree without panicking
}

#[test]
fn test_html5_empty_input() {
    let doc = parse_html5("").unwrap();
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("html"));
}

#[test]
fn test_html5_only_whitespace() {
    let doc = parse_html5("   \n\t  ").unwrap();
    let root = doc.root_element().unwrap();
    assert_eq!(doc.node_name(root), Some("html"));
}
