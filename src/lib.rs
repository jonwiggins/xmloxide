//! # xmloxide
//!
//! A pure Rust reimplementation of libxml2 — the de facto standard XML/HTML
//! parsing library. Memory-safe, high-performance, and conformant with the
//! W3C XML 1.0 (Fifth Edition) specification.
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
pub mod ffi;
pub mod html;
pub mod parser;
pub mod reader;
pub mod sax;
pub mod serial;
pub mod tree;
pub mod util;
pub mod validation;
pub mod xinclude;
pub mod xpath;

// Re-export primary types at the crate root for convenience.
pub use tree::{Attribute, Document, NodeId};
