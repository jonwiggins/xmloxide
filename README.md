# xmloxide

[![CI](https://github.com/jonwiggins/xmloxide/actions/workflows/ci.yml/badge.svg)](https://github.com/jonwiggins/xmloxide/actions/workflows/ci.yml)
[![crates.io](https://img.shields.io/crates/v/xmloxide.svg)](https://crates.io/crates/xmloxide)
[![docs.rs](https://docs.rs/xmloxide/badge.svg)](https://docs.rs/xmloxide)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![MSRV](https://img.shields.io/badge/MSRV-1.81-blue.svg)](https://www.rust-lang.org)

A pure Rust reimplementation of [libxml2](https://gitlab.gnome.org/GNOME/libxml2) — the de facto standard XML/HTML parsing library in the open-source world.

libxml2 became officially unmaintained in December 2025 with known security issues. xmloxide aims to be a memory-safe, high-performance replacement that passes the same conformance test suites.

## Features

- **Memory-safe** — arena-based tree with zero `unsafe` in the public API
- **Conformant** — 100% pass rate on the W3C XML Conformance Test Suite (1727/1727 applicable tests)
- **Error recovery** — parse malformed XML and still produce a usable tree, just like libxml2
- **Multiple parsing APIs** — DOM tree, SAX2 streaming, XmlReader pull, push/incremental
- **HTML parser** — error-tolerant HTML 4.01 parsing with auto-closing and void elements
- **XPath 1.0** — full expression parser and evaluator with all core functions
- **Validation** — DTD, RelaxNG, and XML Schema (XSD) validation
- **Canonical XML** — C14N 1.0 and Exclusive C14N serialization
- **XInclude** — document inclusion processing
- **XML Catalogs** — OASIS XML Catalogs for URI resolution
- **`xmllint` CLI** — command-line tool for parsing, validating, and querying XML
- **Zero-copy where possible** — string interning for fast comparisons
- **No global state** — each `Document` is self-contained and `Send + Sync`
- **C/C++ FFI** — full C API with header file (`include/xmloxide.h`) for embedding in C/C++ projects
- **Minimal dependencies** — only `encoding_rs` (library has zero other deps; `clap` is CLI-only)

## Quick Start

```rust
use xmloxide::Document;

let doc = Document::parse_str("<root><child>Hello</child></root>").unwrap();
let root = doc.root_element().unwrap();
assert_eq!(doc.node_name(root), Some("root"));
assert_eq!(doc.text_content(root), "Hello");
```

### Serialization

```rust
use xmloxide::Document;
use xmloxide::serial::serialize;

let doc = Document::parse_str("<root><child>Hello</child></root>").unwrap();
let xml = serialize(&doc);
assert_eq!(xml, "<root><child>Hello</child></root>");
```

### XPath Queries

```rust
use xmloxide::Document;
use xmloxide::xpath::{evaluate, XPathValue};

let doc = Document::parse_str("<library><book><title>Rust</title></book></library>").unwrap();
let root = doc.root_element().unwrap();
let result = evaluate(&doc, root, "count(book)").unwrap();
assert_eq!(result.to_number(), 1.0);
```

### SAX2 Streaming

```rust
use xmloxide::sax::{parse_sax, SaxHandler, DefaultHandler};
use xmloxide::parser::ParseOptions;

struct MyHandler;
impl SaxHandler for MyHandler {
    fn start_element(&mut self, name: &str, _: Option<&str>, _: Option<&str>,
                     _: &[(String, String, Option<String>, Option<String>)]) {
        println!("Element: {name}");
    }
}

parse_sax("<root><child/></root>", &ParseOptions::default(), &mut MyHandler).unwrap();
```

### HTML Parsing

```rust
use xmloxide::html::parse_html;

let doc = parse_html("<p>Hello <br> World").unwrap();
let root = doc.root_element().unwrap();
assert_eq!(doc.node_name(root), Some("html"));
```

### Error Recovery

```rust
use xmloxide::parser::{parse_str_with_options, ParseOptions};

let opts = ParseOptions::default().recover(true);
let doc = parse_str_with_options("<root><unclosed>", &opts).unwrap();
for diag in &doc.diagnostics {
    eprintln!("{}", diag);
}
```

## CLI Tool

```sh
# Parse and pretty-print
xmllint --format document.xml

# Validate against a schema
xmllint --schema schema.xsd document.xml
xmllint --relaxng schema.rng document.xml
xmllint --dtdvalid schema.dtd document.xml

# XPath query
xmllint --xpath "//title" document.xml

# Canonical XML
xmllint --c14n document.xml

# Parse HTML
xmllint --html page.html
```

## Module Overview

| Module | Description |
|--------|-------------|
| `tree` | Arena-based DOM tree (`Document`, `NodeId`, `NodeKind`) |
| `parser` | XML 1.0 recursive descent parser with error recovery |
| `parser::push` | Push/incremental parser for chunked input |
| `html` | Error-tolerant HTML 4.01 parser |
| `sax` | SAX2 streaming event-driven parser |
| `reader` | XmlReader pull-based parsing API |
| `serial` | XML serializer and Canonical XML (C14N) |
| `xpath` | XPath 1.0 expression parser and evaluator |
| `validation::dtd` | DTD parsing and validation |
| `validation::relaxng` | RelaxNG schema validation |
| `validation::xsd` | XML Schema (XSD) validation |
| `xinclude` | XInclude 1.0 document inclusion |
| `catalog` | OASIS XML Catalogs for URI resolution |
| `encoding` | Character encoding detection and transcoding |
| `ffi` | C/C++ FFI bindings (`include/xmloxide.h`) |

## Performance

Parsing throughput is competitive with libxml2 — within 3-11% on most documents, and **15% faster** on SVG. Serialization is **1.5-2.4x faster** thanks to the arena-based tree design.

**Parsing:**

| Document | Size | xmloxide | libxml2 | Result |
|----------|------|----------|---------|--------|
| Atom feed | 4.9 KB | 26.3 µs (178 MiB/s) | 24.3 µs (193 MiB/s) | ~8% slower |
| SVG drawing | 6.3 KB | 55.6 µs (108 MiB/s) | 63.8 µs (94 MiB/s) | **15% faster** |
| Maven POM | 11.5 KB | 74.6 µs (147 MiB/s) | 71.7 µs (153 MiB/s) | ~4% slower |
| XHTML page | 10.2 KB | 67.3 µs (144 MiB/s) | 59.8 µs (162 MiB/s) | ~11% slower |
| Large (374 KB) | 374 KB | 2.13 ms (171 MiB/s) | 2.07 ms (177 MiB/s) | ~3% slower |

**Serialization:**

| Document | Size | xmloxide | libxml2 | Result |
|----------|------|----------|---------|--------|
| Atom feed | 4.9 KB | 11.4 µs | 17.6 µs | **1.5x faster** |
| Maven POM | 11.5 KB | 19.5 µs | 46.4 µs | **2.4x faster** |
| Large (374 KB) | 374 KB | 608 µs | 1367 µs | **2.3x faster** |

Key optimizations: arena-based tree for fast serialization, byte-level pre-checks for character validation, bulk text scanning, ASCII fast paths for name parsing, zero-copy element name splitting, and inline entity resolution.

```sh
# Run benchmarks (requires libxml2 system library)
cargo bench --features bench-libxml2 --bench comparison_bench
```

## Testing

- **778 unit tests** across all modules
- **libxml2 compatibility suite** — 119/119 tests passing (100%) covering XML parsing, namespaces, error detection, and HTML parsing
- **W3C XML Conformance Test Suite** — 1727/1727 applicable tests passing (100%)
- **Integration tests** covering real-world XML documents, edge cases, and error recovery

```sh
cargo test --all-features
```

## C/C++ FFI

xmloxide provides a C-compatible API for embedding in C/C++ projects (like Chromium, game engines, or any codebase that currently uses libxml2).

To build the C library, temporarily set `crate-type = ["cdylib", "staticlib"]` in `Cargo.toml`'s `[lib]` section, or build directly:

```sh
# Shared library (.so / .dylib / .dll)
cargo rustc --lib --release -- --crate-type cdylib

# Static library (.a / .lib)
cargo rustc --lib --release -- --crate-type staticlib
```

```c
#include "xmloxide.h"

xmloxide_document *doc = xmloxide_parse_str("<root>Hello</root>");
uint32_t root = xmloxide_doc_root_element(doc);
char *name = xmloxide_node_name(doc, root);   // "root"
char *text = xmloxide_node_text_content(doc, root); // "Hello"

xmloxide_free_string(name);
xmloxide_free_string(text);
xmloxide_free_doc(doc);
```

The full API — including tree navigation and mutation, XPath evaluation, serialization (plain and pretty-printed), HTML parsing, DTD/RelaxNG/XSD validation, C14N, and XML Catalogs — is declared in [`include/xmloxide.h`](include/xmloxide.h).

## Migrating from libxml2

| libxml2 | xmloxide (Rust) | xmloxide (C FFI) |
|---------|----------------|------------------|
| `xmlReadMemory` | `Document::parse_str` | `xmloxide_parse_str` |
| `xmlReadFile` | `Document::parse_file` | `xmloxide_parse_file` |
| `xmlParseDoc` | `Document::parse_bytes` | `xmloxide_parse_bytes` |
| `htmlReadMemory` | `html::parse_html` | `xmloxide_parse_html` |
| `xmlFreeDoc` | (drop `Document`) | `xmloxide_free_doc` |
| `xmlDocGetRootElement` | `doc.root_element()` | `xmloxide_doc_root_element` |
| `xmlNodeGetContent` | `doc.text_content(id)` | `xmloxide_node_text_content` |
| `xmlNodeSetContent` | `doc.set_text_content(id, s)` | `xmloxide_set_text_content` |
| `xmlGetProp` | `doc.attribute(id, name)` | `xmloxide_node_attribute` |
| `xmlSetProp` | `doc.set_attribute(...)` | `xmloxide_set_attribute` |
| `xmlNewNode` | `doc.create_node(...)` | `xmloxide_create_element` |
| `xmlNewText` | `doc.create_node(Text{..})` | `xmloxide_create_text` |
| `xmlAddChild` | `doc.append_child(p, c)` | `xmloxide_append_child` |
| `xmlAddPrevSibling` | `doc.insert_before(ref, c)` | `xmloxide_insert_before` |
| `xmlUnlinkNode` | `doc.remove_node(id)` | `xmloxide_remove_node` |
| `xmlCopyNode` | `doc.clone_node(id, deep)` | `xmloxide_clone_node` |
| `xmlGetID` | `doc.element_by_id(s)` | `xmloxide_element_by_id` |
| `xmlDocDumpMemory` | `serial::serialize(&doc)` | `xmloxide_serialize` |
| `xmlDocDumpFormatMemory` | `serial::serialize_with_options` | `xmloxide_serialize_pretty` |
| `htmlDocDumpMemory` | `serial::html::serialize_html` | `xmloxide_serialize_html` |
| `xmlC14NDocDumpMemory` | `serial::c14n::canonicalize` | `xmloxide_canonicalize` |
| `xmlXPathEvalExpression` | `xpath::evaluate` | `xmloxide_xpath_eval` |
| `xmlValidateDtd` | `validation::dtd::validate` | `xmloxide_validate_dtd` |
| `xmlRelaxNGValidateDoc` | `validation::relaxng::validate` | `xmloxide_validate_relaxng` |
| `xmlSchemaValidateDoc` | `validation::xsd::validate_xsd` | `xmloxide_validate_xsd` |
| `xmlXIncludeProcess` | `xinclude::process_xincludes` | `xmloxide_process_xincludes` |
| `xmlLoadCatalog` | `Catalog::parse` | `xmloxide_parse_catalog` |
| `xmlSAX2...` callbacks | `sax::SaxHandler` trait | — |
| `xmlTextReaderRead` | `reader::XmlReader` | — |
| `xmlCreatePushParserCtxt` | `parser::PushParser` | — |

**Thread safety:** Unlike libxml2, xmloxide has no global state. Each `Document` is self-contained and `Send + Sync`. The FFI layer uses thread-local storage for the last error message — each thread has its own error state. No initialization or cleanup functions are needed.

## Fuzzing

xmloxide includes fuzz targets for security testing:

```sh
# Install cargo-fuzz (requires nightly)
cargo install cargo-fuzz

# Run a fuzz target
cargo +nightly fuzz run fuzz_xml_parse
cargo +nightly fuzz run fuzz_html_parse
cargo +nightly fuzz run fuzz_xpath
cargo +nightly fuzz run fuzz_roundtrip
```

## Building

```sh
cargo build
cargo test
cargo clippy --all-targets --all-features -- -D warnings
cargo bench
```

Minimum supported Rust version: **1.81**

## Limitations

- **No XML 1.1** — xmloxide implements XML 1.0 (Fifth Edition) only. XML 1.1 is rarely used and not planned.
- **No XSLT** — XSLT is a separate specification (libxslt) and is out of scope.
- **No Schematron** — Schematron validation is not implemented. DTD, RelaxNG, and XSD are supported.
- **HTML 4.01 only** — the HTML parser targets HTML 4.01, not the HTML5 parsing algorithm.
- **Push parser buffers internally** — the push/incremental parser API (`PushParser`) currently buffers all pushed data and performs the full parse on `finish()`, rather than truly streaming like libxml2's `xmlParseChunk`. SAX streaming (`parse_sax`) is available as an alternative for memory-constrained large-document processing.
- **XPath `namespace::` axis** — the `namespace::` axis is not implemented; queries using it return an empty node set rather than an error.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup and guidelines.

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for version history.

## License

MIT
