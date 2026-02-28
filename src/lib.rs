//! # xmloxide
//!
//! A pure Rust reimplementation of libxml2 — the de facto standard XML/HTML
//! parsing library. Memory-safe, high-performance, and conformant with the
//! W3C XML 1.0 (Fifth Edition) specification.
//!
//! ## Modules
//!
//! - [`tree`] — DOM tree representation with arena-allocated nodes ([`Document`], [`NodeId`])
//! - [`parser`] — XML 1.0 parser with error recovery and push/incremental parsing
//! - [`html`] — Error-tolerant HTML 4.01 parser
//! - [`sax`] — SAX2 event-driven streaming parser
//! - [`reader`] — `XmlReader` pull-based parsing API
//! - [`xpath`] — `XPath` 1.0 expression evaluation
//! - [`validation`] — DTD, `RelaxNG`, and XML Schema (XSD) validation
//! - [`serial`] — XML/HTML serialization and Canonical XML (C14N)
//! - [`encoding`] — Character encoding detection and conversion
//! - [`xinclude`] — `XInclude` 1.0 document inclusion
//! - [`catalog`] — OASIS XML Catalogs for URI resolution
//! - [`error`] — Error types and diagnostics
//!
//! ## Quick Start
//!
//! ```
//! use xmloxide::Document;
//!
//! let doc = Document::parse_str("<root><child>Hello</child></root>").unwrap();
//! let root = doc.root_element().unwrap();
//! assert_eq!(doc.node_name(root), Some("root"));
//! ```

pub mod catalog;
pub mod encoding;
pub mod error;
#[cfg(feature = "ffi")]
pub mod ffi;
pub mod html;
pub mod parser;
pub mod reader;
pub mod sax;
pub mod serial;
pub mod tree;
#[allow(dead_code)]
pub(crate) mod util;
pub mod validation;
pub mod xinclude;
pub mod xpath;

// Re-export primary types at the crate root for convenience.
pub use tree::{Attribute, Document, NodeId};
