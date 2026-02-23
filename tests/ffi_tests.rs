//! Integration tests for the C FFI layer.
//!
//! These tests call the `extern "C"` functions directly from Rust to verify
//! correctness before exposing them to actual C consumers.
#![cfg(feature = "ffi")]
#![allow(unsafe_code, clippy::unwrap_used)]

use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use xmloxide::ffi::document::*;
use xmloxide::ffi::serial::*;
use xmloxide::ffi::strings::*;
use xmloxide::ffi::tree::*;
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
