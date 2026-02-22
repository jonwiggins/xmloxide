# xmloxide

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
- **Minimal dependencies** — only `encoding_rs` and `clap` (CLI only)

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

struct MyHandler;
impl SaxHandler for MyHandler {
    fn start_element(&mut self, name: &str, _: Option<&str>, _: Option<&str>,
                     _: &[(String, String, Option<String>, Option<String>)]) {
        println!("Element: {name}");
    }
}

parse_sax("<root><child/></root>", &mut MyHandler);
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

## Roadmap

| Phase | Scope | Status |
|-------|-------|--------|
| 1 | Core tree, XML parser, serialization | Done |
| 2 | HTML parser, SAX2, XmlReader, push parser | Done |
| 3 | XPath 1.0 | Done |
| 4 | DTD + RelaxNG validation | Done |
| 5 | XML Schema, C14N, XInclude, Catalogs | Done |
| 6 | `xmllint` CLI, benchmarks | Done |

## Building

```sh
cargo build
cargo test --all-features
cargo clippy --all-targets --all-features -- -D warnings
cargo bench
```

Minimum supported Rust version: **1.70**

## License

MIT
