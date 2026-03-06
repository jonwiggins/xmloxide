//! WHATWG HTML5 parser.
//!
//! This module implements the [WHATWG HTML Living Standard] parsing algorithm,
//! including tokenization (§13.2.5), tree construction (§13.2.6), and the full
//! set of named character references (§13.5).
//!
//! The parser produces the same [`Document`](crate::tree::Document) tree
//! structure as the XML and HTML 4.01 parsers, using arena-allocated nodes.
//!
//! # Conformance
//!
//! - **Tokenizer:** 7032/7032 html5lib-tests passing (100%)
//! - **Tree construction:** 1778/1778 html5lib-tests passing (100%)
//!
//! # Quick start
//!
//! ```
//! use xmloxide::html5::parse_html5;
//!
//! let doc = parse_html5("<p>Hello <b>world</b>").unwrap();
//! let root = doc.root_element().unwrap();
//! assert_eq!(doc.node_name(root), Some("html"));
//! ```
//!
//! # Fragment parsing
//!
//! Fragment parsing (the algorithm behind `innerHTML`) is supported via
//! [`Html5ParseOptions::fragment_context`]:
//!
//! ```
//! use xmloxide::html5::{parse_html5_with_options, Html5ParseOptions};
//!
//! let opts = Html5ParseOptions {
//!     scripting: false,
//!     fragment_context: Some("body".to_string()),
//! };
//! let doc = parse_html5_with_options("<p>fragment</p>", &opts).unwrap();
//! ```
//!
//! # Error reporting
//!
//! Use [`parse_html5_full`] to get the document tree together with all parse
//! errors (as [`ParseDiagnostic`](crate::error::ParseDiagnostic)s):
//!
//! ```
//! use xmloxide::html5::parse_html5_full;
//!
//! let result = parse_html5_full("<p>text");
//! println!("errors: {}", result.errors.len());
//! let _doc = result.document;
//! ```
//!
//! [WHATWG HTML Living Standard]: https://html.spec.whatwg.org/

pub mod entities;
pub mod tokenizer;
pub(crate) mod tree_builder;

pub use tree_builder::{
    parse_html5, parse_html5_full, parse_html5_full_with_options, parse_html5_with_options,
    Html5ParseOptions, Html5ParseResult,
};
