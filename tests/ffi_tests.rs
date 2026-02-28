//! Integration tests for the C FFI layer.
//!
//! These tests call the `extern "C"` functions directly from Rust to verify
//! correctness before exposing them to actual C consumers.
#![cfg(feature = "ffi")]
#![allow(unsafe_code, clippy::unwrap_used)]

use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use xmloxide::ffi::c14n::*;
use xmloxide::ffi::catalog::*;
use xmloxide::ffi::document::*;
use xmloxide::ffi::push::*;
use xmloxide::ffi::reader::*;
use xmloxide::ffi::serial::*;
use xmloxide::ffi::strings::*;
use xmloxide::ffi::tree::*;
use xmloxide::ffi::validation::*;
use xmloxide::ffi::xpath::*;

/// Helper to convert a C string pointer to a Rust `String`, then free it.
unsafe fn c_string_to_owned(ptr: *mut c_char) -> Option<String> {
    if ptr.is_null() {
        return None;
    }
    let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
    unsafe { xmloxide_free_string(ptr) };
    Some(s)
}

/// Helper to get the last error as a Rust string.
fn last_error() -> Option<String> {
    unsafe {
        let ptr = xmloxide::ffi::xmloxide_last_error();
        if ptr.is_null() {
            None
        } else {
            Some(CStr::from_ptr(ptr).to_str().unwrap().to_owned())
        }
    }
}

// ---------- Document lifecycle tests ----------

#[test]
fn test_parse_str_and_free() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null(), "parse should succeed");
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_parse_str_null_returns_null() {
    unsafe {
        let doc = xmloxide_parse_str(std::ptr::null());
        assert!(doc.is_null());
        assert!(last_error().is_some());
    }
}

#[test]
fn test_parse_bytes_and_free() {
    let xml = b"<root>hello</root>";
    unsafe {
        let doc = xmloxide_parse_bytes(xml.as_ptr(), xml.len());
        assert!(!doc.is_null(), "parse should succeed");
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_parse_bytes_null_returns_null() {
    unsafe {
        let doc = xmloxide_parse_bytes(std::ptr::null(), 0);
        assert!(doc.is_null());
        assert!(last_error().is_some());
    }
}

#[test]
fn test_free_doc_null_is_safe() {
    unsafe {
        xmloxide_free_doc(std::ptr::null_mut());
    }
}

#[test]
fn test_doc_version() {
    let xml = CString::new("<?xml version=\"1.0\"?><root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());
        let version = c_string_to_owned(xmloxide_doc_version(doc));
        assert_eq!(version.as_deref(), Some("1.0"));
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_doc_encoding() {
    let xml = CString::new("<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());
        let encoding = c_string_to_owned(xmloxide_doc_encoding(doc));
        assert_eq!(encoding.as_deref(), Some("UTF-8"));
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_doc_version_null_doc() {
    unsafe {
        let ptr = xmloxide_doc_version(std::ptr::null());
        assert!(ptr.is_null());
    }
}

// ---------- Tree navigation tests ----------

#[test]
fn test_doc_root_and_root_element() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        let root = xmloxide_doc_root(doc);
        assert_ne!(root, 0);

        let root_elem = xmloxide_doc_root_element(doc);
        assert_ne!(root_elem, 0);

        // Root element should be a child of doc root
        let parent = xmloxide_node_parent(doc, root_elem);
        assert_eq!(parent, root);

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_navigation_children_and_siblings() {
    let xml = CString::new("<root><a/><b/><c/></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        let root_elem = xmloxide_doc_root_element(doc);
        assert_ne!(root_elem, 0);

        // First child should be <a>
        let a = xmloxide_node_first_child(doc, root_elem);
        assert_ne!(a, 0);
        let a_name = c_string_to_owned(xmloxide_node_name(doc, a));
        assert_eq!(a_name.as_deref(), Some("a"));

        // Next sibling of <a> should be <b>
        let b = xmloxide_node_next_sibling(doc, a);
        assert_ne!(b, 0);
        let b_name = c_string_to_owned(xmloxide_node_name(doc, b));
        assert_eq!(b_name.as_deref(), Some("b"));

        // Next sibling of <b> should be <c>
        let c = xmloxide_node_next_sibling(doc, b);
        assert_ne!(c, 0);
        let c_name = c_string_to_owned(xmloxide_node_name(doc, c));
        assert_eq!(c_name.as_deref(), Some("c"));

        // No more siblings
        let none = xmloxide_node_next_sibling(doc, c);
        assert_eq!(none, 0);

        // Last child should be <c>
        let last = xmloxide_node_last_child(doc, root_elem);
        assert_eq!(last, c);

        // Previous sibling of <c> should be <b>
        let prev = xmloxide_node_prev_sibling(doc, c);
        assert_eq!(prev, b);

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_type_element() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        assert_eq!(xmloxide_node_type(doc, root_elem), XMLOXIDE_NODE_ELEMENT);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_type_text() {
    let xml = CString::new("<root>hello</root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let text_node = xmloxide_node_first_child(doc, root_elem);
        assert_ne!(text_node, 0);
        assert_eq!(xmloxide_node_type(doc, text_node), XMLOXIDE_NODE_TEXT);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_type_comment() {
    let xml = CString::new("<root><!-- comment --></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let comment = xmloxide_node_first_child(doc, root_elem);
        assert_ne!(comment, 0);
        assert_eq!(xmloxide_node_type(doc, comment), XMLOXIDE_NODE_COMMENT);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_type_pi() {
    let xml = CString::new("<root><?target data?></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let pi = xmloxide_node_first_child(doc, root_elem);
        assert_ne!(pi, 0);
        assert_eq!(xmloxide_node_type(doc, pi), XMLOXIDE_NODE_PI);

        let name = c_string_to_owned(xmloxide_node_name(doc, pi));
        assert_eq!(name.as_deref(), Some("target"));

        let text = c_string_to_owned(xmloxide_node_text(doc, pi));
        assert_eq!(text.as_deref(), Some("data"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_type_document() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root(doc);
        assert_eq!(xmloxide_node_type(doc, root), XMLOXIDE_NODE_DOCUMENT);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_type_invalid() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        // Node id 0 is invalid
        assert_eq!(xmloxide_node_type(doc, 0), -1);
        // Null doc
        assert_eq!(xmloxide_node_type(std::ptr::null(), 1), -1);
        xmloxide_free_doc(doc);
    }
}

// ---------- Node content tests ----------

#[test]
fn test_node_name() {
    let xml = CString::new("<myElement/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let name = c_string_to_owned(xmloxide_node_name(doc, root_elem));
        assert_eq!(name.as_deref(), Some("myElement"));
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_text() {
    let xml = CString::new("<root>hello world</root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let text_node = xmloxide_node_first_child(doc, root_elem);
        let text = c_string_to_owned(xmloxide_node_text(doc, text_node));
        assert_eq!(text.as_deref(), Some("hello world"));
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_text_content() {
    let xml = CString::new("<root>hello <b>world</b></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let content = c_string_to_owned(xmloxide_node_text_content(doc, root_elem));
        assert_eq!(content.as_deref(), Some("hello world"));
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_text_returns_null_for_element() {
    let xml = CString::new("<root><child/></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        // node_text returns null for elements
        let text = xmloxide_node_text(doc, root_elem);
        assert!(text.is_null());
        xmloxide_free_doc(doc);
    }
}

// ---------- Attribute tests ----------

#[test]
fn test_node_attribute() {
    let xml = CString::new("<root id=\"42\" class=\"main\"/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);

        let attr_name = CString::new("id").unwrap();
        let val = c_string_to_owned(xmloxide_node_attribute(doc, root_elem, attr_name.as_ptr()));
        assert_eq!(val.as_deref(), Some("42"));

        let attr_name = CString::new("class").unwrap();
        let val = c_string_to_owned(xmloxide_node_attribute(doc, root_elem, attr_name.as_ptr()));
        assert_eq!(val.as_deref(), Some("main"));

        // Non-existent attribute
        let attr_name = CString::new("missing").unwrap();
        let val = xmloxide_node_attribute(doc, root_elem, attr_name.as_ptr());
        assert!(val.is_null());

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_attribute_count() {
    let xml = CString::new("<root a=\"1\" b=\"2\" c=\"3\"/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        assert_eq!(xmloxide_node_attribute_count(doc, root_elem), 3);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_attribute_null_name() {
    let xml = CString::new("<root a=\"1\"/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let val = xmloxide_node_attribute(doc, root_elem, std::ptr::null());
        assert!(val.is_null());
        xmloxide_free_doc(doc);
    }
}

// ---------- Namespace tests ----------

#[test]
fn test_node_namespace() {
    let xml = CString::new("<root xmlns=\"http://example.com\"/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let ns = c_string_to_owned(xmloxide_node_namespace(doc, root_elem));
        assert_eq!(ns.as_deref(), Some("http://example.com"));
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_node_namespace_none() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root_elem = xmloxide_doc_root_element(doc);
        let ns = xmloxide_node_namespace(doc, root_elem);
        assert!(ns.is_null());
        xmloxide_free_doc(doc);
    }
}

// ---------- Serialization tests ----------

#[test]
fn test_serialize_roundtrip() {
    let xml = CString::new("<root><child>text</child></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        let output = c_string_to_owned(xmloxide_serialize(doc));
        assert!(output.is_some());
        let output = output.unwrap();
        assert!(output.contains("<root>"));
        assert!(output.contains("<child>text</child>"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_serialize_null_doc() {
    unsafe {
        let ptr = xmloxide_serialize(std::ptr::null());
        assert!(ptr.is_null());
    }
}

// ---------- XPath tests ----------

#[test]
fn test_xpath_eval_nodeset() {
    let xml = CString::new("<root><a/><b/><a/></root>").unwrap();
    let expr = CString::new("//a").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        let result = xmloxide_xpath_eval(doc, 0, expr.as_ptr());
        assert!(!result.is_null());

        assert_eq!(xmloxide_xpath_result_type(result), XMLOXIDE_XPATH_NODESET);
        assert_eq!(xmloxide_xpath_nodeset_count(result), 2);

        // Each item should be a valid node
        let item0 = xmloxide_xpath_nodeset_item(result, 0);
        assert_ne!(item0, 0);
        let name = c_string_to_owned(xmloxide_node_name(doc, item0));
        assert_eq!(name.as_deref(), Some("a"));

        let item1 = xmloxide_xpath_nodeset_item(result, 1);
        assert_ne!(item1, 0);

        // Out of bounds returns 0
        assert_eq!(xmloxide_xpath_nodeset_item(result, 99), 0);

        xmloxide_xpath_free_result(result);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_xpath_eval_boolean() {
    let xml = CString::new("<root><a/></root>").unwrap();
    let expr = CString::new("boolean(//a)").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let result = xmloxide_xpath_eval(doc, 0, expr.as_ptr());
        assert!(!result.is_null());

        assert_eq!(xmloxide_xpath_result_type(result), XMLOXIDE_XPATH_BOOLEAN);
        assert_eq!(xmloxide_xpath_result_boolean(result), 1);

        xmloxide_xpath_free_result(result);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_xpath_eval_number() {
    let xml = CString::new("<root><a>42</a></root>").unwrap();
    let expr = CString::new("number(//a)").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let result = xmloxide_xpath_eval(doc, 0, expr.as_ptr());
        assert!(!result.is_null());

        assert_eq!(xmloxide_xpath_result_type(result), XMLOXIDE_XPATH_NUMBER);
        let num = xmloxide_xpath_result_number(result);
        assert!((num - 42.0).abs() < f64::EPSILON);

        xmloxide_xpath_free_result(result);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_xpath_eval_string() {
    let xml = CString::new("<root>hello</root>").unwrap();
    let expr = CString::new("string(/root)").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let result = xmloxide_xpath_eval(doc, 0, expr.as_ptr());
        assert!(!result.is_null());

        assert_eq!(xmloxide_xpath_result_type(result), XMLOXIDE_XPATH_STRING);
        let s = c_string_to_owned(xmloxide_xpath_result_string(result));
        assert_eq!(s.as_deref(), Some("hello"));

        xmloxide_xpath_free_result(result);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_xpath_with_context_node() {
    let xml = CString::new("<root><a><b>inner</b></a><b>outer</b></root>").unwrap();
    let expr = CString::new(".//b").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());

        // Get the <a> element to use as context
        let root_elem = xmloxide_doc_root_element(doc);
        let a = xmloxide_node_first_child(doc, root_elem);
        assert_ne!(a, 0);

        let result = xmloxide_xpath_eval(doc, a, expr.as_ptr());
        assert!(!result.is_null());

        // Should find only the <b> inside <a>
        assert_eq!(xmloxide_xpath_nodeset_count(result), 1);

        xmloxide_xpath_free_result(result);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_xpath_null_args() {
    let expr = CString::new("//a").unwrap();
    unsafe {
        // Null doc
        let result = xmloxide_xpath_eval(std::ptr::null(), 0, expr.as_ptr());
        assert!(result.is_null());
        assert!(last_error().is_some());

        // Null expr
        let xml = CString::new("<root/>").unwrap();
        let doc = xmloxide_parse_str(xml.as_ptr());
        let result = xmloxide_xpath_eval(doc, 0, std::ptr::null());
        assert!(result.is_null());

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_xpath_free_null_is_safe() {
    unsafe {
        xmloxide_xpath_free_result(std::ptr::null_mut());
    }
}

#[test]
fn test_xpath_result_accessors_null() {
    unsafe {
        assert_eq!(xmloxide_xpath_result_type(std::ptr::null()), -1);
        assert_eq!(xmloxide_xpath_result_boolean(std::ptr::null()), 0);
        assert!(xmloxide_xpath_result_number(std::ptr::null()).is_nan());
        assert!(xmloxide_xpath_result_string(std::ptr::null()).is_null());
        assert_eq!(xmloxide_xpath_nodeset_count(std::ptr::null()), 0);
        assert_eq!(xmloxide_xpath_nodeset_item(std::ptr::null(), 0), 0);
    }
}

// ---------- Error handling tests ----------

#[test]
fn test_error_cleared_on_success() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        // First cause an error
        let _ = xmloxide_parse_str(std::ptr::null());
        assert!(last_error().is_some());

        // Now succeed — error should be cleared
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());
        assert!(last_error().is_none());

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_parse_error_message() {
    let xml = CString::new("<unclosed").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(doc.is_null());
        let err = last_error();
        assert!(err.is_some());
        // Error message should contain something meaningful
        assert!(!err.unwrap().is_empty());
    }
}

// ---------- String lifecycle tests ----------

#[test]
fn test_free_string_null_is_safe() {
    unsafe {
        xmloxide_free_string(std::ptr::null_mut());
    }
}

// ---------- Navigation null safety ----------

#[test]
fn test_navigation_null_doc() {
    unsafe {
        assert_eq!(xmloxide_doc_root(std::ptr::null()), 0);
        assert_eq!(xmloxide_doc_root_element(std::ptr::null()), 0);
        assert_eq!(xmloxide_node_parent(std::ptr::null(), 1), 0);
        assert_eq!(xmloxide_node_first_child(std::ptr::null(), 1), 0);
        assert_eq!(xmloxide_node_last_child(std::ptr::null(), 1), 0);
        assert_eq!(xmloxide_node_next_sibling(std::ptr::null(), 1), 0);
        assert_eq!(xmloxide_node_prev_sibling(std::ptr::null(), 1), 0);
        assert!(xmloxide_node_name(std::ptr::null(), 1).is_null());
        assert!(xmloxide_node_text(std::ptr::null(), 1).is_null());
        assert!(xmloxide_node_text_content(std::ptr::null(), 1).is_null());
        assert!(xmloxide_node_namespace(std::ptr::null(), 1).is_null());
        assert_eq!(xmloxide_node_attribute_count(std::ptr::null(), 1), 0);
    }
}

#[test]
fn test_navigation_zero_node() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert_eq!(xmloxide_node_parent(doc, 0), 0);
        assert_eq!(xmloxide_node_first_child(doc, 0), 0);
        assert_eq!(xmloxide_node_last_child(doc, 0), 0);
        assert_eq!(xmloxide_node_next_sibling(doc, 0), 0);
        assert_eq!(xmloxide_node_prev_sibling(doc, 0), 0);
        assert_eq!(xmloxide_node_type(doc, 0), -1);
        assert!(xmloxide_node_name(doc, 0).is_null());
        assert!(xmloxide_node_text(doc, 0).is_null());
        assert!(xmloxide_node_text_content(doc, 0).is_null());
        xmloxide_free_doc(doc);
    }
}

// ---------- Attribute index access tests ----------

#[test]
fn test_attribute_name_at() {
    let xml = CString::new("<root a=\"1\" b=\"2\"/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let name0 = c_string_to_owned(xmloxide_node_attribute_name_at(doc, root, 0));
        assert_eq!(name0.as_deref(), Some("a"));

        let name1 = c_string_to_owned(xmloxide_node_attribute_name_at(doc, root, 1));
        assert_eq!(name1.as_deref(), Some("b"));

        // Out of bounds
        assert!(xmloxide_node_attribute_name_at(doc, root, 99).is_null());

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_attribute_value_at() {
    let xml = CString::new("<root x=\"hello\" y=\"world\"/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let val0 = c_string_to_owned(xmloxide_node_attribute_value_at(doc, root, 0));
        assert_eq!(val0.as_deref(), Some("hello"));

        let val1 = c_string_to_owned(xmloxide_node_attribute_value_at(doc, root, 1));
        assert_eq!(val1.as_deref(), Some("world"));

        // Out of bounds
        assert!(xmloxide_node_attribute_value_at(doc, root, 99).is_null());

        xmloxide_free_doc(doc);
    }
}

// ---------- HTML parsing tests ----------

#[test]
fn test_parse_html() {
    let html = CString::new("<p>Hello <br> World").unwrap();
    unsafe {
        let doc = xmloxide_parse_html(html.as_ptr());
        assert!(!doc.is_null(), "HTML parse should succeed");

        let root = xmloxide_doc_root_element(doc);
        assert_ne!(root, 0);
        let name = c_string_to_owned(xmloxide_node_name(doc, root));
        assert_eq!(name.as_deref(), Some("html"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_parse_html_null() {
    unsafe {
        let doc = xmloxide_parse_html(std::ptr::null());
        assert!(doc.is_null());
        assert!(last_error().is_some());
    }
}

// ---------- File parsing tests ----------

#[test]
fn test_parse_file_nonexistent() {
    let path = CString::new("/nonexistent/path/to/file.xml").unwrap();
    unsafe {
        let doc = xmloxide_parse_file(path.as_ptr());
        assert!(doc.is_null());
        assert!(last_error().is_some());
    }
}

#[test]
fn test_parse_file_null() {
    unsafe {
        let doc = xmloxide_parse_file(std::ptr::null());
        assert!(doc.is_null());
        assert!(last_error().is_some());
    }
}

// ---------- Tree mutation tests ----------

#[test]
fn test_create_element_and_append() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let name = CString::new("child").unwrap();
        let child = xmloxide_create_element(doc, name.as_ptr());
        assert_ne!(child, 0);

        let result = xmloxide_append_child(doc, root, child);
        assert_eq!(result, 1);

        // Verify child is now under root
        let first = xmloxide_node_first_child(doc, root);
        assert_eq!(first, child);
        let child_name = c_string_to_owned(xmloxide_node_name(doc, child));
        assert_eq!(child_name.as_deref(), Some("child"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_create_text_and_append() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let content = CString::new("hello world").unwrap();
        let text = xmloxide_create_text(doc, content.as_ptr());
        assert_ne!(text, 0);

        xmloxide_append_child(doc, root, text);

        // Verify text content
        let tc = c_string_to_owned(xmloxide_node_text_content(doc, root));
        assert_eq!(tc.as_deref(), Some("hello world"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_create_comment_and_append() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let content = CString::new("a comment").unwrap();
        let comment = xmloxide_create_comment(doc, content.as_ptr());
        assert_ne!(comment, 0);

        xmloxide_append_child(doc, root, comment);

        let first = xmloxide_node_first_child(doc, root);
        assert_eq!(xmloxide_node_type(doc, first), XMLOXIDE_NODE_COMMENT);
        let text = c_string_to_owned(xmloxide_node_text(doc, first));
        assert_eq!(text.as_deref(), Some("a comment"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_create_element_null_args() {
    unsafe {
        // Null doc
        let name = CString::new("elem").unwrap();
        assert_eq!(
            xmloxide_create_element(std::ptr::null_mut(), name.as_ptr()),
            0
        );

        // Null name
        let xml = CString::new("<root/>").unwrap();
        let doc = xmloxide_parse_str(xml.as_ptr());
        assert_eq!(xmloxide_create_element(doc, std::ptr::null()), 0);
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_remove_node() {
    let xml = CString::new("<root><a/><b/></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let a = xmloxide_node_first_child(doc, root);
        assert_ne!(a, 0);

        let result = xmloxide_remove_node(doc, a);
        assert_eq!(result, 1);

        // First child should now be <b>
        let first = xmloxide_node_first_child(doc, root);
        let name = c_string_to_owned(xmloxide_node_name(doc, first));
        assert_eq!(name.as_deref(), Some("b"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_remove_node_null() {
    unsafe {
        assert_eq!(xmloxide_remove_node(std::ptr::null_mut(), 1), 0);
    }
}

#[test]
fn test_clone_node_deep() {
    let xml = CString::new("<root><parent><child>text</child></parent></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);
        let parent = xmloxide_node_first_child(doc, root);

        let cloned = xmloxide_clone_node(doc, parent, 1);
        assert_ne!(cloned, 0);
        assert_ne!(cloned, parent);

        // Cloned node should have the same name
        let name = c_string_to_owned(xmloxide_node_name(doc, cloned));
        assert_eq!(name.as_deref(), Some("parent"));

        // Deep clone should include child
        let cloned_child = xmloxide_node_first_child(doc, cloned);
        assert_ne!(cloned_child, 0);
        let child_name = c_string_to_owned(xmloxide_node_name(doc, cloned_child));
        assert_eq!(child_name.as_deref(), Some("child"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_clone_node_shallow() {
    let xml = CString::new("<root><parent><child/></parent></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);
        let parent = xmloxide_node_first_child(doc, root);

        let cloned = xmloxide_clone_node(doc, parent, 0);
        assert_ne!(cloned, 0);

        // Shallow clone should have no children
        assert_eq!(xmloxide_node_first_child(doc, cloned), 0);

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_clone_node_null() {
    unsafe {
        assert_eq!(xmloxide_clone_node(std::ptr::null_mut(), 1, 1), 0);
    }
}

#[test]
fn test_set_attribute_new() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let name = CString::new("id").unwrap();
        let value = CString::new("42").unwrap();
        let result = xmloxide_set_attribute(doc, root, name.as_ptr(), value.as_ptr());
        assert_eq!(result, 1);

        // Verify attribute was set
        let val = c_string_to_owned(xmloxide_node_attribute(doc, root, name.as_ptr()));
        assert_eq!(val.as_deref(), Some("42"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_set_attribute_update() {
    let xml = CString::new("<root id=\"old\"/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let name = CString::new("id").unwrap();
        let value = CString::new("new").unwrap();
        let result = xmloxide_set_attribute(doc, root, name.as_ptr(), value.as_ptr());
        assert_eq!(result, 1);

        let val = c_string_to_owned(xmloxide_node_attribute(doc, root, name.as_ptr()));
        assert_eq!(val.as_deref(), Some("new"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_set_attribute_null_args() {
    unsafe {
        let name = CString::new("x").unwrap();
        let value = CString::new("y").unwrap();
        // Null doc
        assert_eq!(
            xmloxide_set_attribute(std::ptr::null_mut(), 1, name.as_ptr(), value.as_ptr()),
            0
        );
    }
}

#[test]
fn test_append_child_null() {
    unsafe {
        assert_eq!(xmloxide_append_child(std::ptr::null_mut(), 1, 2), 0);
    }
}

// ---------- Pretty serialization tests ----------

#[test]
fn test_serialize_pretty() {
    let xml = CString::new("<root><child>text</child></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let output = c_string_to_owned(xmloxide_serialize_pretty(doc));
        assert!(output.is_some());
        let output = output.unwrap();
        // Pretty output should contain indentation
        assert!(
            output.contains("  <child>"),
            "expected indented output, got: {output}"
        );

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_serialize_pretty_custom() {
    let xml = CString::new("<root><child>text</child></root>").unwrap();
    let indent = CString::new("\t").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let output = c_string_to_owned(xmloxide_serialize_pretty_custom(doc, indent.as_ptr()));
        assert!(output.is_some());
        let output = output.unwrap();
        assert!(
            output.contains("\t<child>"),
            "expected tab-indented output, got: {output}"
        );

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_serialize_pretty_null() {
    unsafe {
        assert!(xmloxide_serialize_pretty(std::ptr::null()).is_null());
        assert!(xmloxide_serialize_pretty_custom(std::ptr::null(), std::ptr::null()).is_null());
    }
}

// ---------- HTML serialization tests ----------

#[test]
fn test_serialize_html() {
    let html = CString::new("<p>Hello <br> World</p>").unwrap();
    unsafe {
        let doc = xmloxide_parse_html(html.as_ptr());
        assert!(!doc.is_null());

        let output = c_string_to_owned(xmloxide_serialize_html(doc));
        assert!(output.is_some());
        let output = output.unwrap();
        // HTML serializer should produce HTML-style output
        assert!(output.contains("<p>") || output.contains("<body>"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_serialize_html_null() {
    unsafe {
        assert!(xmloxide_serialize_html(std::ptr::null()).is_null());
    }
}

// ---------- DTD validation tests ----------

#[test]
fn test_dtd_validate_valid() {
    let dtd_str = CString::new("<!ELEMENT root (#PCDATA)>").unwrap();
    let xml = CString::new("<root>hello</root>").unwrap();
    unsafe {
        let dtd = xmloxide_parse_dtd(dtd_str.as_ptr());
        assert!(!dtd.is_null(), "DTD parse should succeed");

        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        let result = xmloxide_validate_dtd(doc, dtd);
        assert!(!result.is_null());
        assert_eq!(xmloxide_validation_is_valid(result), 1);
        assert_eq!(xmloxide_validation_error_count(result), 0);

        xmloxide_free_validation_result(result);
        xmloxide_free_doc(doc);
        xmloxide_free_dtd(dtd);
    }
}

#[test]
fn test_dtd_validate_invalid() {
    let dtd_str = CString::new("<!ELEMENT root (child)>\n<!ELEMENT child (#PCDATA)>").unwrap();
    let xml = CString::new("<root>text only</root>").unwrap();
    unsafe {
        let dtd = xmloxide_parse_dtd(dtd_str.as_ptr());
        assert!(!dtd.is_null());

        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        let result = xmloxide_validate_dtd(doc, dtd);
        assert!(!result.is_null());
        assert_eq!(xmloxide_validation_is_valid(result), 0);
        assert!(xmloxide_validation_error_count(result) > 0);

        // Get error message
        let msg = c_string_to_owned(xmloxide_validation_error_message(result, 0));
        assert!(msg.is_some());
        assert!(!msg.unwrap().is_empty());

        // Out of bounds returns null
        let bad = xmloxide_validation_error_message(result, 999);
        assert!(bad.is_null());

        xmloxide_free_validation_result(result);
        xmloxide_free_doc(doc);
        xmloxide_free_dtd(dtd);
    }
}

#[test]
fn test_dtd_parse_null() {
    unsafe {
        let dtd = xmloxide_parse_dtd(std::ptr::null());
        assert!(dtd.is_null());
        assert!(last_error().is_some());
    }
}

#[test]
fn test_dtd_free_null() {
    unsafe {
        xmloxide_free_dtd(std::ptr::null_mut());
    }
}

#[test]
fn test_validate_dtd_null_args() {
    unsafe {
        let result = xmloxide_validate_dtd(std::ptr::null_mut(), std::ptr::null());
        assert!(result.is_null());
    }
}

// ---------- RelaxNG validation tests ----------

#[test]
fn test_relaxng_validate_valid() {
    let schema_str = CString::new(
        r#"<element name="root" xmlns="http://relaxng.org/ns/structure/1.0">
            <text/>
        </element>"#,
    )
    .unwrap();
    let xml = CString::new("<root>hello</root>").unwrap();
    unsafe {
        let schema = xmloxide_parse_relaxng(schema_str.as_ptr());
        assert!(!schema.is_null(), "RelaxNG parse should succeed");

        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        let result = xmloxide_validate_relaxng(doc, schema);
        assert!(!result.is_null());
        assert_eq!(xmloxide_validation_is_valid(result), 1);

        xmloxide_free_validation_result(result);
        xmloxide_free_doc(doc);
        xmloxide_free_relaxng(schema);
    }
}

#[test]
fn test_relaxng_parse_null() {
    unsafe {
        let schema = xmloxide_parse_relaxng(std::ptr::null());
        assert!(schema.is_null());
    }
}

#[test]
fn test_relaxng_free_null() {
    unsafe {
        xmloxide_free_relaxng(std::ptr::null_mut());
    }
}

// ---------- XSD validation tests ----------

#[test]
fn test_xsd_validate_valid() {
    let schema_str = CString::new(
        r#"<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="root" type="xs:string"/>
        </xs:schema>"#,
    )
    .unwrap();
    let xml = CString::new("<root>hello</root>").unwrap();
    unsafe {
        let schema = xmloxide_parse_xsd(schema_str.as_ptr());
        assert!(!schema.is_null(), "XSD parse should succeed");

        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        let result = xmloxide_validate_xsd(doc, schema);
        assert!(!result.is_null());
        assert_eq!(xmloxide_validation_is_valid(result), 1);

        xmloxide_free_validation_result(result);
        xmloxide_free_doc(doc);
        xmloxide_free_xsd(schema);
    }
}

#[test]
fn test_xsd_parse_null() {
    unsafe {
        let schema = xmloxide_parse_xsd(std::ptr::null());
        assert!(schema.is_null());
    }
}

#[test]
fn test_xsd_free_null() {
    unsafe {
        xmloxide_free_xsd(std::ptr::null_mut());
    }
}

// ---------- Validation result accessors ----------

#[test]
fn test_validation_result_null_safety() {
    unsafe {
        assert_eq!(xmloxide_validation_is_valid(std::ptr::null()), 0);
        assert_eq!(xmloxide_validation_error_count(std::ptr::null()), 0);
        assert!(xmloxide_validation_error_message(std::ptr::null(), 0).is_null());
    }
}

#[test]
fn test_free_validation_result_null() {
    unsafe {
        xmloxide_free_validation_result(std::ptr::null_mut());
    }
}

// ---------- Tree mutation + serialization roundtrip ----------

#[test]
fn test_mutation_roundtrip() {
    let xml = CString::new("<root/>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        // Build: <root><item id="1">Hello</item></root>
        let elem_name = CString::new("item").unwrap();
        let item = xmloxide_create_element(doc, elem_name.as_ptr());

        let attr_name = CString::new("id").unwrap();
        let attr_val = CString::new("1").unwrap();
        xmloxide_set_attribute(doc, item, attr_name.as_ptr(), attr_val.as_ptr());

        let text_content = CString::new("Hello").unwrap();
        let text = xmloxide_create_text(doc, text_content.as_ptr());
        xmloxide_append_child(doc, item, text);
        xmloxide_append_child(doc, root, item);

        // Serialize and verify
        let output = c_string_to_owned(xmloxide_serialize(doc));
        let output = output.unwrap();
        assert!(
            output.contains("<item id=\"1\">Hello</item>"),
            "got: {output}"
        );

        xmloxide_free_doc(doc);
    }
}

// ---------- set_text_content tests ----------

#[test]
fn test_set_text_content_on_text_node() {
    let xml = CString::new("<root>old</root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);
        let text_node = xmloxide_node_first_child(doc, root);

        let new_content = CString::new("new").unwrap();
        let result = xmloxide_set_text_content(doc, text_node, new_content.as_ptr());
        assert_eq!(result, 1);

        let tc = c_string_to_owned(xmloxide_node_text_content(doc, root));
        assert_eq!(tc.as_deref(), Some("new"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_set_text_content_on_element() {
    let xml = CString::new("<root><child>old</child></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        let new_content = CString::new("replaced").unwrap();
        let result = xmloxide_set_text_content(doc, root, new_content.as_ptr());
        assert_eq!(result, 1);

        // Element should now have just one text child
        let tc = c_string_to_owned(xmloxide_node_text_content(doc, root));
        assert_eq!(tc.as_deref(), Some("replaced"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_set_text_content_null() {
    unsafe {
        assert_eq!(
            xmloxide_set_text_content(std::ptr::null_mut(), 1, std::ptr::null()),
            0
        );
    }
}

// ---------- insert_before, element_by_id, node_prefix tests ----------

#[test]
fn test_insert_before() {
    let xml = CString::new("<root><b/><c/></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        // Create <a/> and insert before <b/>
        let a_name = CString::new("a").unwrap();
        let a = xmloxide_create_element(doc, a_name.as_ptr());
        let b = xmloxide_node_first_child(doc, root);

        let result = xmloxide_insert_before(doc, b, a);
        assert_eq!(result, 1);

        // First child should now be <a>
        let first = xmloxide_node_first_child(doc, root);
        let name = c_string_to_owned(xmloxide_node_name(doc, first));
        assert_eq!(name.as_deref(), Some("a"));

        // Next should be <b>
        let next = xmloxide_node_next_sibling(doc, first);
        let name = c_string_to_owned(xmloxide_node_name(doc, next));
        assert_eq!(name.as_deref(), Some("b"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_insert_before_null() {
    unsafe {
        assert_eq!(xmloxide_insert_before(std::ptr::null_mut(), 1, 2), 0);
    }
}

#[test]
fn test_node_prefix() {
    let xml = CString::new("<root xmlns:ns=\"http://example.com\"><ns:child/></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let root = xmloxide_doc_root_element(doc);

        // Root has no prefix
        assert!(xmloxide_node_prefix(doc, root).is_null());

        // ns:child has prefix "ns"
        let child = xmloxide_node_first_child(doc, root);
        let prefix = c_string_to_owned(xmloxide_node_prefix(doc, child));
        assert_eq!(prefix.as_deref(), Some("ns"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_element_by_id() {
    // element_by_id requires the id_map to be populated (via DTD validation)
    let dtd_str = CString::new(
        "<!ELEMENT root (item)*>\n<!ELEMENT item (#PCDATA)>\n<!ATTLIST item id ID #IMPLIED>",
    )
    .unwrap();
    let xml = CString::new("<root><item id=\"foo\">hello</item></root>").unwrap();
    unsafe {
        let dtd = xmloxide_parse_dtd(dtd_str.as_ptr());
        assert!(!dtd.is_null());

        let doc = xmloxide_parse_str(xml.as_ptr());
        assert!(!doc.is_null());

        // Validate to populate id_map
        let vresult = xmloxide_validate_dtd(doc, dtd);
        xmloxide_free_validation_result(vresult);

        let id = CString::new("foo").unwrap();
        let node = xmloxide_element_by_id(doc, id.as_ptr());
        assert_ne!(node, 0);

        let name = c_string_to_owned(xmloxide_node_name(doc, node));
        assert_eq!(name.as_deref(), Some("item"));

        // Non-existent ID
        let bad_id = CString::new("nonexistent").unwrap();
        assert_eq!(xmloxide_element_by_id(doc, bad_id.as_ptr()), 0);

        xmloxide_free_doc(doc);
        xmloxide_free_dtd(dtd);
    }
}

// ---------- C14N tests ----------

#[test]
fn test_canonicalize() {
    let xml = CString::new("<root><b/><a/></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        let output = c_string_to_owned(xmloxide_canonicalize(doc));
        assert!(output.is_some());
        let output = output.unwrap();
        assert!(output.contains("<root>"));
        assert!(output.contains("<b></b>"));
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_canonicalize_opts_no_comments() {
    let xml = CString::new("<root><!-- comment --><a/></root>").unwrap();
    unsafe {
        let doc = xmloxide_parse_str(xml.as_ptr());
        // With comments
        let with = c_string_to_owned(xmloxide_canonicalize_opts(doc, 1, 0));
        assert!(with.unwrap().contains("<!-- comment -->"));

        // Without comments
        let without = c_string_to_owned(xmloxide_canonicalize_opts(doc, 0, 0));
        assert!(!without.unwrap().contains("<!-- comment -->"));

        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_canonicalize_null() {
    unsafe {
        assert!(xmloxide_canonicalize(std::ptr::null()).is_null());
        assert!(xmloxide_canonicalize_opts(std::ptr::null(), 1, 0).is_null());
        assert!(xmloxide_canonicalize_subtree(std::ptr::null(), 1, 1, 0).is_null());
    }
}

// ---------- Catalog tests ----------

#[test]
fn test_parse_catalog_and_resolve() {
    let catalog_xml = CString::new(
        r#"<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
            <system systemId="http://example.com/schema.dtd" uri="local/schema.dtd"/>
        </catalog>"#,
    )
    .unwrap();
    unsafe {
        let cat = xmloxide_parse_catalog(catalog_xml.as_ptr());
        assert!(!cat.is_null(), "catalog parse should succeed");

        let sys_id = CString::new("http://example.com/schema.dtd").unwrap();
        let resolved = c_string_to_owned(xmloxide_catalog_resolve_system(cat, sys_id.as_ptr()));
        assert_eq!(resolved.as_deref(), Some("local/schema.dtd"));

        // Not found
        let bad = CString::new("http://other.com/missing.dtd").unwrap();
        assert!(xmloxide_catalog_resolve_system(cat, bad.as_ptr()).is_null());

        xmloxide_free_catalog(cat);
    }
}

#[test]
fn test_catalog_null_safety() {
    unsafe {
        assert!(xmloxide_parse_catalog(std::ptr::null()).is_null());
        xmloxide_free_catalog(std::ptr::null_mut());
        assert!(xmloxide_catalog_resolve_system(std::ptr::null(), std::ptr::null()).is_null());
        assert!(xmloxide_catalog_resolve_public(std::ptr::null(), std::ptr::null()).is_null());
        assert!(xmloxide_catalog_resolve_uri(std::ptr::null(), std::ptr::null()).is_null());
    }
}

// ---------- Push parser tests ----------

#[test]
fn test_push_parser_basic() {
    unsafe {
        let parser = xmloxide_push_parser_new();
        assert!(!parser.is_null());

        let chunk1 = b"<root>";
        let chunk2 = b"<child>Hello</child>";
        let chunk3 = b"</root>";

        xmloxide_push_parser_push(parser, chunk1.as_ptr(), chunk1.len());
        xmloxide_push_parser_push(parser, chunk2.as_ptr(), chunk2.len());
        xmloxide_push_parser_push(parser, chunk3.as_ptr(), chunk3.len());

        let doc = xmloxide_push_parser_finish(parser);
        assert!(!doc.is_null());

        let root = xmloxide_doc_root_element(doc);
        let name = c_string_to_owned(xmloxide_node_name(doc, root));
        assert_eq!(name.as_deref(), Some("root"));

        let child = xmloxide_node_first_child(doc, root);
        let child_name = c_string_to_owned(xmloxide_node_name(doc, child));
        assert_eq!(child_name.as_deref(), Some("child"));

        let text = c_string_to_owned(xmloxide_node_text_content(doc, child));
        assert_eq!(text.as_deref(), Some("Hello"));

        xmloxide_free_doc(doc);
        // parser was consumed by finish — do NOT free it
    }
}

#[test]
fn test_push_parser_buffered_bytes() {
    unsafe {
        let parser = xmloxide_push_parser_new();
        assert_eq!(xmloxide_push_parser_buffered_bytes(parser), 0);

        let data = b"<root/>";
        xmloxide_push_parser_push(parser, data.as_ptr(), data.len());
        assert_eq!(xmloxide_push_parser_buffered_bytes(parser), 7);

        let doc = xmloxide_push_parser_finish(parser);
        assert!(!doc.is_null());
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_push_parser_reset() {
    unsafe {
        let parser = xmloxide_push_parser_new();

        let bad = b"<incomplete";
        xmloxide_push_parser_push(parser, bad.as_ptr(), bad.len());
        assert_eq!(xmloxide_push_parser_buffered_bytes(parser), 11);

        xmloxide_push_parser_reset(parser);
        assert_eq!(xmloxide_push_parser_buffered_bytes(parser), 0);

        let good = b"<root/>";
        xmloxide_push_parser_push(parser, good.as_ptr(), good.len());

        let doc = xmloxide_push_parser_finish(parser);
        assert!(!doc.is_null());
        xmloxide_free_doc(doc);
    }
}

#[test]
fn test_push_parser_free_without_finish() {
    unsafe {
        let parser = xmloxide_push_parser_new();
        let data = b"<root/>";
        xmloxide_push_parser_push(parser, data.as_ptr(), data.len());
        // Discard without finishing
        xmloxide_push_parser_free(parser);
    }
}

#[test]
fn test_push_parser_null_safety() {
    unsafe {
        // Push to null parser — should not crash
        xmloxide_push_parser_push(std::ptr::null_mut(), std::ptr::null(), 0);
        // Finish null parser
        let doc = xmloxide_push_parser_finish(std::ptr::null_mut());
        assert!(doc.is_null());
        // Buffered bytes on null
        assert_eq!(xmloxide_push_parser_buffered_bytes(std::ptr::null()), 0);
        // Reset null
        xmloxide_push_parser_reset(std::ptr::null_mut());
        // Free null
        xmloxide_push_parser_free(std::ptr::null_mut());
    }
}

#[test]
fn test_push_parser_malformed_error() {
    unsafe {
        let parser = xmloxide_push_parser_new();
        let data = b"<a></b>";
        xmloxide_push_parser_push(parser, data.as_ptr(), data.len());
        let doc = xmloxide_push_parser_finish(parser);
        assert!(doc.is_null());
        assert!(last_error().is_some());
    }
}

#[test]
fn test_push_parser_split_tokens() {
    unsafe {
        let parser = xmloxide_push_parser_new();
        // Split element name across chunks
        let chunks: &[&[u8]] = &[b"<ro", b"ot at", b"tr=\"val", b"ue\"/>"];
        for chunk in chunks {
            xmloxide_push_parser_push(parser, chunk.as_ptr(), chunk.len());
        }
        let doc = xmloxide_push_parser_finish(parser);
        assert!(!doc.is_null());

        let root = xmloxide_doc_root_element(doc);
        let name = c_string_to_owned(xmloxide_node_name(doc, root));
        assert_eq!(name.as_deref(), Some("root"));

        let attr_name = CString::new("attr").unwrap();
        let val = c_string_to_owned(xmloxide_node_attribute(doc, root, attr_name.as_ptr()));
        assert_eq!(val.as_deref(), Some("value"));

        xmloxide_free_doc(doc);
    }
}

// ---------- XmlReader tests ----------

#[test]
fn test_reader_basic_traversal() {
    let xml = CString::new("<root><child>Hello</child></root>").unwrap();
    unsafe {
        let reader = xmloxide_reader_new(xml.as_ptr());
        assert!(!reader.is_null());

        // First read: <root>
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_ELEMENT);
        let name = c_string_to_owned(xmloxide_reader_name(reader));
        assert_eq!(name.as_deref(), Some("root"));
        assert_eq!(xmloxide_reader_depth(reader), 0);

        // <child>
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_ELEMENT);
        let name = c_string_to_owned(xmloxide_reader_name(reader));
        assert_eq!(name.as_deref(), Some("child"));
        assert_eq!(xmloxide_reader_depth(reader), 1);

        // Text "Hello"
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_TEXT);
        let val = c_string_to_owned(xmloxide_reader_value(reader));
        assert_eq!(val.as_deref(), Some("Hello"));
        assert_eq!(xmloxide_reader_has_value(reader), 1);

        // </child>
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(
            xmloxide_reader_node_type(reader),
            XMLOXIDE_READER_END_ELEMENT
        );

        // </root>
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(
            xmloxide_reader_node_type(reader),
            XMLOXIDE_READER_END_ELEMENT
        );

        // End of document
        assert_eq!(xmloxide_reader_read(reader), 0);

        xmloxide_reader_free(reader);
    }
}

#[test]
fn test_reader_attributes() {
    let xml = CString::new("<root id=\"42\" class=\"big\"/>").unwrap();
    unsafe {
        let reader = xmloxide_reader_new(xml.as_ptr());
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_ELEMENT);
        assert_eq!(xmloxide_reader_is_empty_element(reader), 1);
        assert_eq!(xmloxide_reader_attribute_count(reader), 2);

        let attr_name = CString::new("id").unwrap();
        let val = c_string_to_owned(xmloxide_reader_get_attribute(reader, attr_name.as_ptr()));
        assert_eq!(val.as_deref(), Some("42"));

        let attr_name = CString::new("class").unwrap();
        let val = c_string_to_owned(xmloxide_reader_get_attribute(reader, attr_name.as_ptr()));
        assert_eq!(val.as_deref(), Some("big"));

        // Missing attribute
        let missing = CString::new("nonexistent").unwrap();
        assert!(xmloxide_reader_get_attribute(reader, missing.as_ptr()).is_null());

        xmloxide_reader_free(reader);
    }
}

#[test]
fn test_reader_attribute_navigation() {
    let xml = CString::new("<root a=\"1\" b=\"2\"/>").unwrap();
    unsafe {
        let reader = xmloxide_reader_new(xml.as_ptr());
        assert_eq!(xmloxide_reader_read(reader), 1);

        // Move to first attribute
        assert_eq!(xmloxide_reader_move_to_first_attribute(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_ATTRIBUTE);
        let name = c_string_to_owned(xmloxide_reader_name(reader));
        assert_eq!(name.as_deref(), Some("a"));
        let val = c_string_to_owned(xmloxide_reader_value(reader));
        assert_eq!(val.as_deref(), Some("1"));

        // Move to next attribute
        assert_eq!(xmloxide_reader_move_to_next_attribute(reader), 1);
        let name = c_string_to_owned(xmloxide_reader_name(reader));
        assert_eq!(name.as_deref(), Some("b"));
        let val = c_string_to_owned(xmloxide_reader_value(reader));
        assert_eq!(val.as_deref(), Some("2"));

        // No more attributes
        assert_eq!(xmloxide_reader_move_to_next_attribute(reader), 0);

        // Move back to element
        assert_eq!(xmloxide_reader_move_to_element(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_ELEMENT);

        xmloxide_reader_free(reader);
    }
}

#[test]
fn test_reader_namespace() {
    let xml =
        CString::new("<ns:root xmlns:ns=\"http://example.com\"><ns:child/></ns:root>").unwrap();
    unsafe {
        let reader = xmloxide_reader_new(xml.as_ptr());
        assert_eq!(xmloxide_reader_read(reader), 1);

        let local = c_string_to_owned(xmloxide_reader_local_name(reader));
        assert_eq!(local.as_deref(), Some("root"));

        let prefix = c_string_to_owned(xmloxide_reader_prefix(reader));
        assert_eq!(prefix.as_deref(), Some("ns"));

        let ns = c_string_to_owned(xmloxide_reader_namespace_uri(reader));
        assert_eq!(ns.as_deref(), Some("http://example.com"));

        xmloxide_reader_free(reader);
    }
}

#[test]
fn test_reader_comment_and_pi() {
    let xml = CString::new("<?target data?><root><!-- comment --></root>").unwrap();
    unsafe {
        let reader = xmloxide_reader_new(xml.as_ptr());

        // PI
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_PI);
        let name = c_string_to_owned(xmloxide_reader_name(reader));
        assert_eq!(name.as_deref(), Some("target"));
        let val = c_string_to_owned(xmloxide_reader_value(reader));
        assert_eq!(val.as_deref(), Some("data"));

        // <root>
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_ELEMENT);

        // Comment
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_COMMENT);
        let val = c_string_to_owned(xmloxide_reader_value(reader));
        assert_eq!(val.as_deref(), Some(" comment "));

        xmloxide_reader_free(reader);
    }
}

#[test]
fn test_reader_null_safety() {
    unsafe {
        // Null input
        let reader = xmloxide_reader_new(std::ptr::null());
        assert!(reader.is_null());
        assert!(last_error().is_some());

        // Null reader in all functions
        assert_eq!(xmloxide_reader_read(std::ptr::null_mut()), -1);
        assert_eq!(
            xmloxide_reader_node_type(std::ptr::null()),
            XMLOXIDE_READER_NONE
        );
        assert!(xmloxide_reader_name(std::ptr::null()).is_null());
        assert!(xmloxide_reader_local_name(std::ptr::null()).is_null());
        assert!(xmloxide_reader_prefix(std::ptr::null()).is_null());
        assert!(xmloxide_reader_namespace_uri(std::ptr::null()).is_null());
        assert!(xmloxide_reader_value(std::ptr::null()).is_null());
        assert_eq!(xmloxide_reader_depth(std::ptr::null()), 0);
        assert_eq!(xmloxide_reader_is_empty_element(std::ptr::null()), 0);
        assert_eq!(xmloxide_reader_has_value(std::ptr::null()), 0);
        assert_eq!(xmloxide_reader_attribute_count(std::ptr::null()), 0);
        assert!(xmloxide_reader_get_attribute(std::ptr::null(), std::ptr::null()).is_null());
        assert_eq!(
            xmloxide_reader_move_to_first_attribute(std::ptr::null_mut()),
            0
        );
        assert_eq!(
            xmloxide_reader_move_to_next_attribute(std::ptr::null_mut()),
            0
        );
        assert_eq!(xmloxide_reader_move_to_element(std::ptr::null_mut()), 0);
        // Free null is safe
        xmloxide_reader_free(std::ptr::null_mut());
    }
}

#[test]
fn test_reader_empty_element() {
    let xml = CString::new("<root><br/></root>").unwrap();
    unsafe {
        let reader = xmloxide_reader_new(xml.as_ptr());

        // <root>
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_is_empty_element(reader), 0);

        // <br/>
        assert_eq!(xmloxide_reader_read(reader), 1);
        assert_eq!(xmloxide_reader_node_type(reader), XMLOXIDE_READER_ELEMENT);
        assert_eq!(xmloxide_reader_is_empty_element(reader), 1);

        xmloxide_reader_free(reader);
    }
}
