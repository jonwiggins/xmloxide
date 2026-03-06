//! Python bindings for xmloxide via PyO3.
//!
//! Exposes `Document`, `NodeId`, and core parsing/navigation/XPath APIs.
#![allow(unsafe_code)]

use pyo3::exceptions::{PyRuntimeError, PyValueError};
use pyo3::prelude::*;

use xmloxide::serial::SerializeOptions;
use xmloxide::tree::{Document as RustDocument, NodeId as RustNodeId, NodeKind};

/// A parsed XML/HTML document.
#[pyclass]
struct Document {
    inner: RustDocument,
}

/// A node identifier within a document.
#[pyclass(from_py_object)]
#[derive(Clone, Copy)]
struct NodeId {
    id: RustNodeId,
}

#[pymethods]
impl Document {
    /// Parse an XML string.
    #[staticmethod]
    fn parse_xml(xml: &str) -> PyResult<Self> {
        RustDocument::parse_str(xml)
            .map(|inner| Self { inner })
            .map_err(|e| PyValueError::new_err(e.to_string()))
    }

    /// Parse raw bytes as XML (with encoding detection).
    #[staticmethod]
    fn parse_bytes(data: &[u8]) -> PyResult<Self> {
        RustDocument::parse_bytes(data)
            .map(|inner| Self { inner })
            .map_err(|e| PyValueError::new_err(e.to_string()))
    }

    /// Parse an HTML string (error-tolerant HTML 4.01 parser).
    #[staticmethod]
    fn parse_html(html: &str) -> PyResult<Self> {
        xmloxide::html::parse_html(html)
            .map(|inner| Self { inner })
            .map_err(|e| PyValueError::new_err(e.to_string()))
    }

    /// Parse an HTML5 string (WHATWG parsing algorithm).
    #[staticmethod]
    fn parse_html5(html: &str) -> PyResult<Self> {
        xmloxide::html5::parse_html5(html)
            .map(|inner| Self { inner })
            .map_err(|e| PyValueError::new_err(e.to_string()))
    }

    /// Get the root element of the document.
    fn root_element(&self) -> PyResult<Option<NodeId>> {
        Ok(self.inner.root_element().map(|id| NodeId { id }))
    }

    /// Get the name of a node (element tag name or PI target).
    fn node_name(&self, node: &NodeId) -> Option<String> {
        self.inner.node_name(node.id).map(String::from)
    }

    /// Get the text content of a node (direct text for text/comment nodes).
    fn node_text(&self, node: &NodeId) -> Option<String> {
        self.inner.node_text(node.id).map(String::from)
    }

    /// Get the concatenated text content of a node and all descendants.
    fn text_content(&self, node: &NodeId) -> String {
        self.inner.text_content(node.id)
    }

    /// Get the node type as a string.
    fn node_type(&self, node: &NodeId) -> &str {
        match &self.inner.node(node.id).kind {
            NodeKind::Document => "document",
            NodeKind::Element { .. } => "element",
            NodeKind::Text { .. } => "text",
            NodeKind::CData { .. } => "cdata",
            NodeKind::Comment { .. } => "comment",
            NodeKind::ProcessingInstruction { .. } => "processing_instruction",
            NodeKind::EntityRef { .. } => "entity_ref",
            NodeKind::DocumentType { .. } => "document_type",
        }
    }

    /// Get the first child of a node.
    fn first_child(&self, node: &NodeId) -> Option<NodeId> {
        self.inner.first_child(node.id).map(|id| NodeId { id })
    }

    /// Get the last child of a node.
    fn last_child(&self, node: &NodeId) -> Option<NodeId> {
        self.inner.last_child(node.id).map(|id| NodeId { id })
    }

    /// Get the next sibling of a node.
    fn next_sibling(&self, node: &NodeId) -> Option<NodeId> {
        self.inner.next_sibling(node.id).map(|id| NodeId { id })
    }

    /// Get the previous sibling of a node.
    fn prev_sibling(&self, node: &NodeId) -> Option<NodeId> {
        self.inner.prev_sibling(node.id).map(|id| NodeId { id })
    }

    /// Get the parent of a node.
    fn parent(&self, node: &NodeId) -> Option<NodeId> {
        self.inner.parent(node.id).map(|id| NodeId { id })
    }

    /// Get all children of a node as a list.
    fn children(&self, node: &NodeId) -> Vec<NodeId> {
        self.inner
            .children(node.id)
            .map(|id| NodeId { id })
            .collect()
    }

    /// Get an attribute value by name.
    fn attribute(&self, node: &NodeId, name: &str) -> Option<String> {
        self.inner.attribute(node.id, name).map(String::from)
    }

    /// Get all attribute names on an element.
    fn attribute_names(&self, node: &NodeId) -> Vec<String> {
        self.inner
            .attributes(node.id)
            .iter()
            .map(|a| a.name.clone())
            .collect()
    }

    /// Get the namespace URI of an element.
    fn namespace(&self, node: &NodeId) -> Option<String> {
        self.inner.node_namespace(node.id).map(String::from)
    }

    /// Evaluate an XPath expression and return matching node ids.
    fn xpath(&self, node: &NodeId, expr: &str) -> PyResult<Vec<NodeId>> {
        match xmloxide::xpath::evaluate(&self.inner, node.id, expr) {
            Ok(value) => {
                let nodes = value.as_node_set().cloned().unwrap_or_default();
                Ok(nodes.into_iter().map(|id| NodeId { id }).collect())
            }
            Err(e) => Err(PyRuntimeError::new_err(e.to_string())),
        }
    }

    /// Serialize the document to XML.
    fn to_xml(&self) -> String {
        xmloxide::serial::serialize(&self.inner)
    }

    /// Serialize the document to pretty-printed XML.
    fn to_xml_pretty(&self) -> String {
        xmloxide::serial::serialize_with_options(
            &self.inner,
            &SerializeOptions::default().indent(true),
        )
    }

    /// Serialize the document to HTML.
    fn to_html(&self) -> String {
        xmloxide::serial::html::serialize_html(&self.inner)
    }

    // --- Mutation ---

    /// Create a new element node (detached).
    fn create_element(&mut self, name: &str) -> NodeId {
        NodeId {
            id: self.inner.create_element(name),
        }
    }

    /// Create a new text node (detached).
    fn create_text(&mut self, content: &str) -> NodeId {
        NodeId {
            id: self.inner.create_text(content),
        }
    }

    /// Append a child node to a parent.
    fn append_child(&mut self, parent: &NodeId, child: &NodeId) {
        self.inner.append_child(parent.id, child.id);
    }

    /// Remove a node from the tree.
    fn remove_node(&mut self, node: &NodeId) {
        self.inner.remove_node(node.id);
    }

    /// Set an attribute on an element.
    fn set_attribute(&mut self, node: &NodeId, name: &str, value: &str) -> bool {
        self.inner.set_attribute(node.id, name, value)
    }

    /// Remove an attribute from an element.
    fn remove_attribute(&mut self, node: &NodeId, name: &str) -> bool {
        self.inner.remove_attribute(node.id, name)
    }

    /// Set the text content of a node.
    fn set_text_content(&mut self, node: &NodeId, content: &str) -> bool {
        self.inner.set_text_content(node.id, content)
    }

    fn __repr__(&self) -> String {
        let count = self.inner.node_count();
        format!("<Document nodes={count}>")
    }
}

#[pymethods]
impl NodeId {
    fn __repr__(&self) -> String {
        format!("<NodeId {}>", self.id.into_raw())
    }

    fn __eq__(&self, other: &NodeId) -> bool {
        self.id == other.id
    }

    fn __hash__(&self) -> u32 {
        self.id.into_raw()
    }
}

/// Python module for pyxmloxide.
#[pymodule]
fn pyxmloxide(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Document>()?;
    m.add_class::<NodeId>()?;
    Ok(())
}
