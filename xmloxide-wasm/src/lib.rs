//! WebAssembly bindings for xmloxide via wasm-bindgen.
//!
//! Exposes XML/HTML parsing, tree navigation, CSS selectors, XPath, and
//! serialization to JavaScript.
#![allow(unsafe_code)]

use wasm_bindgen::prelude::*;
use xmloxide::tree::{Document as RustDocument, NodeId as RustNodeId, NodeKind};

/// A parsed XML/HTML document.
#[wasm_bindgen]
pub struct WasmDocument {
    inner: RustDocument,
}

/// A node identifier within a document (opaque handle).
#[wasm_bindgen]
#[derive(Clone, Copy)]
pub struct WasmNodeId {
    id: RustNodeId,
}

#[wasm_bindgen]
impl WasmDocument {
    /// Parse an XML string into a document.
    #[wasm_bindgen(js_name = parseXml)]
    pub fn parse_xml(xml: &str) -> Result<WasmDocument, JsError> {
        RustDocument::parse_str(xml)
            .map(|inner| Self { inner })
            .map_err(|e| JsError::new(&e.to_string()))
    }

    /// Parse an HTML string (error-tolerant HTML 4 parser).
    #[wasm_bindgen(js_name = parseHtml)]
    pub fn parse_html(html: &str) -> Result<WasmDocument, JsError> {
        xmloxide::html::parse_html(html)
            .map(|inner| Self { inner })
            .map_err(|e| JsError::new(&e.to_string()))
    }

    /// Parse an HTML5 string (WHATWG parsing algorithm).
    #[wasm_bindgen(js_name = parseHtml5)]
    pub fn parse_html5(html: &str) -> Result<WasmDocument, JsError> {
        xmloxide::html5::parse_html5(html)
            .map(|inner| Self { inner })
            .map_err(|e| JsError::new(&e.to_string()))
    }

    /// Get the root element of the document.
    #[wasm_bindgen(js_name = rootElement)]
    pub fn root_element(&self) -> Option<WasmNodeId> {
        self.inner.root_element().map(|id| WasmNodeId { id })
    }

    /// Get the tag name of an element node.
    #[wasm_bindgen(js_name = nodeName)]
    pub fn node_name(&self, node: &WasmNodeId) -> Option<String> {
        self.inner.node_name(node.id).map(String::from)
    }

    /// Get the text content of a text/comment/cdata node.
    #[wasm_bindgen(js_name = nodeText)]
    pub fn node_text(&self, node: &WasmNodeId) -> Option<String> {
        self.inner.node_text(node.id).map(String::from)
    }

    /// Get the concatenated text of a node and all its descendants.
    #[wasm_bindgen(js_name = textContent)]
    pub fn text_content(&self, node: &WasmNodeId) -> String {
        self.inner.text_content(node.id)
    }

    /// Get the node type as a string.
    #[wasm_bindgen(js_name = nodeType)]
    pub fn node_type(&self, node: &WasmNodeId) -> String {
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
        .to_string()
    }

    /// Get the first child of a node.
    #[wasm_bindgen(js_name = firstChild)]
    pub fn first_child(&self, node: &WasmNodeId) -> Option<WasmNodeId> {
        self.inner.first_child(node.id).map(|id| WasmNodeId { id })
    }

    /// Get the last child of a node.
    #[wasm_bindgen(js_name = lastChild)]
    pub fn last_child(&self, node: &WasmNodeId) -> Option<WasmNodeId> {
        self.inner.last_child(node.id).map(|id| WasmNodeId { id })
    }

    /// Get the next sibling of a node.
    #[wasm_bindgen(js_name = nextSibling)]
    pub fn next_sibling(&self, node: &WasmNodeId) -> Option<WasmNodeId> {
        self.inner
            .next_sibling(node.id)
            .map(|id| WasmNodeId { id })
    }

    /// Get the previous sibling of a node.
    #[wasm_bindgen(js_name = prevSibling)]
    pub fn prev_sibling(&self, node: &WasmNodeId) -> Option<WasmNodeId> {
        self.inner
            .prev_sibling(node.id)
            .map(|id| WasmNodeId { id })
    }

    /// Get the parent of a node.
    pub fn parent(&self, node: &WasmNodeId) -> Option<WasmNodeId> {
        self.inner.parent(node.id).map(|id| WasmNodeId { id })
    }

    /// Get all children of a node as a `Vec` (returns as JS array).
    pub fn children(&self, node: &WasmNodeId) -> Vec<WasmNodeId> {
        self.inner
            .children(node.id)
            .map(|id| WasmNodeId { id })
            .collect()
    }

    /// Get an attribute value by name.
    pub fn attribute(&self, node: &WasmNodeId, name: &str) -> Option<String> {
        self.inner.attribute(node.id, name).map(String::from)
    }

    /// Evaluate a CSS selector and return matching nodes.
    #[wasm_bindgen(js_name = querySelectorAll)]
    pub fn query_selector_all(
        &self,
        node: &WasmNodeId,
        selector: &str,
    ) -> Result<Vec<WasmNodeId>, JsError> {
        xmloxide::css::select(&self.inner, node.id, selector)
            .map(|nodes| nodes.into_iter().map(|id| WasmNodeId { id }).collect())
            .map_err(|e| JsError::new(&e.to_string()))
    }

    /// Evaluate an XPath expression and return matching nodes.
    pub fn xpath(&self, node: &WasmNodeId, expr: &str) -> Result<Vec<WasmNodeId>, JsError> {
        match xmloxide::xpath::evaluate(&self.inner, node.id, expr) {
            Ok(value) => {
                let nodes = value.as_node_set().cloned().unwrap_or_default();
                Ok(nodes.into_iter().map(|id| WasmNodeId { id }).collect())
            }
            Err(e) => Err(JsError::new(&e.to_string())),
        }
    }

    /// Serialize the document to XML.
    #[wasm_bindgen(js_name = toXml)]
    pub fn to_xml(&self) -> String {
        xmloxide::serial::serialize(&self.inner)
    }

    /// Serialize the document to HTML.
    #[wasm_bindgen(js_name = toHtml)]
    pub fn to_html(&self) -> String {
        xmloxide::serial::html::serialize_html(&self.inner)
    }

    /// Get the total number of nodes in the document.
    #[wasm_bindgen(js_name = nodeCount)]
    pub fn node_count(&self) -> usize {
        self.inner.node_count()
    }

    // -- Validation --

    /// Validate against a RelaxNG schema (XML string).
    /// Returns a `WasmValidationResult`.
    #[wasm_bindgen(js_name = validateRelaxng)]
    pub fn validate_relaxng(&self, schema_xml: &str) -> Result<WasmValidationResult, JsError> {
        let schema = xmloxide::validation::relaxng::parse_relaxng(schema_xml)
            .map_err(|e| JsError::new(&e.to_string()))?;
        let result = xmloxide::validation::relaxng::validate(&self.inner, &schema);
        Ok(WasmValidationResult::from_result(&result))
    }

    /// Validate against an XML Schema (XSD string).
    /// Returns a `WasmValidationResult`.
    #[wasm_bindgen(js_name = validateXsd)]
    pub fn validate_xsd(&self, schema_xml: &str) -> Result<WasmValidationResult, JsError> {
        let schema = xmloxide::validation::xsd::parse_xsd(schema_xml)
            .map_err(|e| JsError::new(&e.to_string()))?;
        let result = xmloxide::validation::xsd::validate_xsd(&self.inner, &schema);
        Ok(WasmValidationResult::from_result(&result))
    }

    /// Validate against an ISO Schematron schema (XML string).
    /// Returns a `WasmValidationResult`.
    #[wasm_bindgen(js_name = validateSchematron)]
    pub fn validate_schematron(
        &self,
        schema_xml: &str,
    ) -> Result<WasmValidationResult, JsError> {
        let schema = xmloxide::validation::schematron::parse_schematron(schema_xml)
            .map_err(|e| JsError::new(&e.to_string()))?;
        let result = xmloxide::validation::schematron::validate_schematron(&self.inner, &schema);
        Ok(WasmValidationResult::from_result(&result))
    }
}

/// Result of validating a document against a schema.
#[wasm_bindgen]
pub struct WasmValidationResult {
    valid: bool,
    error_messages: Vec<String>,
    warning_messages: Vec<String>,
}

#[wasm_bindgen]
impl WasmValidationResult {
    /// Whether the document is valid.
    #[wasm_bindgen(getter, js_name = isValid)]
    pub fn is_valid(&self) -> bool {
        self.valid
    }

    /// Validation error messages.
    #[wasm_bindgen(getter)]
    pub fn errors(&self) -> Vec<String> {
        self.error_messages.clone()
    }

    /// Validation warning messages.
    #[wasm_bindgen(getter)]
    pub fn warnings(&self) -> Vec<String> {
        self.warning_messages.clone()
    }
}

impl WasmValidationResult {
    fn from_result(result: &xmloxide::validation::ValidationResult) -> Self {
        Self {
            valid: result.is_valid,
            error_messages: result.errors.iter().map(|e| e.message.clone()).collect(),
            warning_messages: result.warnings.iter().map(|w| w.message.clone()).collect(),
        }
    }
}

#[wasm_bindgen]
impl WasmNodeId {
    /// Get the raw numeric ID (for debugging).
    #[wasm_bindgen(js_name = rawId)]
    pub fn raw_id(&self) -> u32 {
        self.id.into_raw()
    }
}
