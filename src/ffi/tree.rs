//! Tree navigation and node inspection FFI functions.
#![allow(unsafe_code, clippy::missing_safety_doc)]

use std::os::raw::c_char;

use crate::tree::{Document, NodeId, NodeKind};

use super::strings::to_c_string;

// Node type constants matching common XML conventions.

/// Element node type constant.
pub const XMLOXIDE_NODE_ELEMENT: i32 = 1;
/// Text node type constant.
pub const XMLOXIDE_NODE_TEXT: i32 = 3;
/// CDATA section node type constant.
pub const XMLOXIDE_NODE_CDATA: i32 = 4;
/// Entity reference node type constant.
pub const XMLOXIDE_NODE_ENTITY_REF: i32 = 5;
/// Processing instruction node type constant.
pub const XMLOXIDE_NODE_PI: i32 = 7;
/// Comment node type constant.
pub const XMLOXIDE_NODE_COMMENT: i32 = 8;
/// Document node type constant.
pub const XMLOXIDE_NODE_DOCUMENT: i32 = 9;
/// Document type node type constant.
pub const XMLOXIDE_NODE_DOCUMENT_TYPE: i32 = 10;

/// Helper to convert `Option<NodeId>` to a raw u32 (0 = no node).
fn node_id_to_raw(id: Option<NodeId>) -> u32 {
    id.map_or(0, NodeId::into_raw)
}

/// Helper to safely dereference a document pointer and node id.
///
/// Returns `None` if either the document is null or the raw node id is 0.
unsafe fn doc_and_node(doc: *const Document, raw_node: u32) -> Option<(&'static Document, NodeId)> {
    if doc.is_null() {
        return None;
    }
    let doc = unsafe { &*doc };
    let node_id = NodeId::from_raw(raw_node)?;
    Some((doc, node_id))
}

/// Returns the document root node id.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_doc_root(doc: *const Document) -> u32 {
    if doc.is_null() {
        return 0;
    }
    let doc = unsafe { &*doc };
    doc.root().into_raw()
}

/// Returns the root element of the document, or 0 if none.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_doc_root_element(doc: *const Document) -> u32 {
    if doc.is_null() {
        return 0;
    }
    let doc = unsafe { &*doc };
    node_id_to_raw(doc.root_element())
}

/// Returns the parent of a node, or 0 if none.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_parent(doc: *const Document, node: u32) -> u32 {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return 0;
    };
    node_id_to_raw(doc.parent(node_id))
}

/// Returns the first child of a node, or 0 if none.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_first_child(doc: *const Document, node: u32) -> u32 {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return 0;
    };
    node_id_to_raw(doc.first_child(node_id))
}

/// Returns the last child of a node, or 0 if none.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_last_child(doc: *const Document, node: u32) -> u32 {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return 0;
    };
    node_id_to_raw(doc.last_child(node_id))
}

/// Returns the next sibling of a node, or 0 if none.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_next_sibling(doc: *const Document, node: u32) -> u32 {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return 0;
    };
    node_id_to_raw(doc.next_sibling(node_id))
}

/// Returns the previous sibling of a node, or 0 if none.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_prev_sibling(doc: *const Document, node: u32) -> u32 {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return 0;
    };
    node_id_to_raw(doc.prev_sibling(node_id))
}

/// Returns the node type as an integer constant.
///
/// Returns -1 if the document or node is invalid.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_type(doc: *const Document, node: u32) -> i32 {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return -1;
    };
    match &doc.node(node_id).kind {
        NodeKind::Document => XMLOXIDE_NODE_DOCUMENT,
        NodeKind::Element { .. } => XMLOXIDE_NODE_ELEMENT,
        NodeKind::Text { .. } => XMLOXIDE_NODE_TEXT,
        NodeKind::CData { .. } => XMLOXIDE_NODE_CDATA,
        NodeKind::Comment { .. } => XMLOXIDE_NODE_COMMENT,
        NodeKind::ProcessingInstruction { .. } => XMLOXIDE_NODE_PI,
        NodeKind::EntityRef { .. } => XMLOXIDE_NODE_ENTITY_REF,
        NodeKind::DocumentType { .. } => XMLOXIDE_NODE_DOCUMENT_TYPE,
    }
}

/// Returns the name of a node (element local name or PI target).
///
/// Returns null for node types that have no name. The returned string
/// must be freed with `xmloxide_free_string`.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_name(doc: *const Document, node: u32) -> *mut c_char {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return std::ptr::null_mut();
    };
    match doc.node_name(node_id) {
        Some(name) => to_c_string(name),
        None => std::ptr::null_mut(),
    }
}

/// Returns the direct text content of a text, comment, CDATA, or PI node.
///
/// Returns null for element and document nodes. The returned string
/// must be freed with `xmloxide_free_string`.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_text(doc: *const Document, node: u32) -> *mut c_char {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return std::ptr::null_mut();
    };
    match doc.node_text(node_id) {
        Some(text) => to_c_string(text),
        None => std::ptr::null_mut(),
    }
}

/// Returns the concatenated text content of a node and all descendants.
///
/// The returned string must be freed with `xmloxide_free_string`.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_text_content(
    doc: *const Document,
    node: u32,
) -> *mut c_char {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return std::ptr::null_mut();
    };
    let text = doc.text_content(node_id);
    to_c_string(&text)
}

/// Returns the namespace URI of an element node, or null if none.
///
/// The returned string must be freed with `xmloxide_free_string`.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_namespace(doc: *const Document, node: u32) -> *mut c_char {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return std::ptr::null_mut();
    };
    match doc.node_namespace(node_id) {
        Some(ns) => to_c_string(ns),
        None => std::ptr::null_mut(),
    }
}

/// Returns the value of an attribute by name on an element node.
///
/// Returns null if the attribute is not present. The returned string
/// must be freed with `xmloxide_free_string`.
///
/// # Safety
///
/// `doc` must be a valid document pointer. `name` must be a valid
/// null-terminated UTF-8 string.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_attribute(
    doc: *const Document,
    node: u32,
    name: *const c_char,
) -> *mut c_char {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return std::ptr::null_mut();
    };
    if name.is_null() {
        return std::ptr::null_mut();
    }
    let c_name = unsafe { std::ffi::CStr::from_ptr(name) };
    let Ok(name_str) = c_name.to_str() else {
        return std::ptr::null_mut();
    };
    match doc.attribute(node_id, name_str) {
        Some(val) => to_c_string(val),
        None => std::ptr::null_mut(),
    }
}

/// Returns the number of attributes on an element node.
///
/// Returns 0 for non-element nodes.
///
/// # Safety
///
/// `doc` must be a valid document pointer.
#[no_mangle]
pub unsafe extern "C" fn xmloxide_node_attribute_count(doc: *const Document, node: u32) -> usize {
    let Some((doc, node_id)) = (unsafe { doc_and_node(doc, node) }) else {
        return 0;
    };
    doc.attributes(node_id).len()
}
