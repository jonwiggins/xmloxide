# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.1] - 2026-03-06

### Fixed

- Pin `tempfile` dev-dependency to `<3.20` to avoid transitive `getrandom 0.4`
  which requires `edition2024` (Rust 1.85+), breaking the MSRV of 1.81

### Improved

- Pre-commit hook now includes an MSRV check: runs `cargo check` with the 1.81
  toolchain (if installed) or scans `Cargo.lock` for edition2024 dependencies

## [0.3.0] - 2026-03-06

### Added

- **CSS selector engine** (`css` module) — query document trees with familiar CSS
  syntax including tag, class, ID, attribute, descendant, child, adjacent sibling,
  general sibling combinators, `:first-child`, `:last-child`, `:only-child`,
  `:empty`, `:not()`, `:nth-child()`, `:nth-last-child()`, and selector groups
- **Streaming HTML5 SAX API** (`html5::sax` module) — callback-driven API that
  wraps the WHATWG HTML5 tokenizer directly without building a DOM tree, with
  automatic character coalescing for efficient text handling
- **Auto-populated `id_map`** — `element_by_id()` now works out of the box for
  XML, HTML 4, and HTML5 documents without requiring DTD validation; the parser
  automatically indexes `id` attributes during tree construction
- **Fast `#id` CSS selector path** — pure `#id` selectors use O(1) hash lookup
  via `element_by_id()` instead of tree traversal
- **Tree mutation API** — `Document::create_element()`, `create_text()`,
  `create_comment()`, `append_child()`, `insert_before()`, `remove_node()`,
  `clone_node()`, `set_text_content()`, `set_attribute()`, `remove_attribute()`
- **`Document::with_capacity(n)`** — pre-size the arena when expected node count
  is known
- **`Document::is_element(id)`** — convenience method for checking node type
- **Serde XML support** (`serde` feature) — serialize/deserialize Rust types
  to/from XML via `serde_xml` module
- **Async XML parsing** (`async` feature) — `parse_async()` for parsing from
  `tokio::io::AsyncRead` sources
- **WebAssembly bindings** (`xmloxide-wasm` subcrate) — parse, query, and
  serialize XML/HTML from JavaScript via `wasm-bindgen`
- **Python bindings** (`pyxmloxide` subcrate) — parse, query, and serialize
  XML/HTML from Python via PyO3
- **Property-based testing** — 20 proptest properties covering roundtrip parsing,
  serialization invariants, and edge cases
- **Ecosystem benchmarks** — head-to-head benchmarks against `roxmltree` and
  `quick-xml`

### Fixed

- HTML 4 parser infinite loop on bare `<` not followed by a valid tag start
- HTML5 tokenizer panic on multi-byte characters in the ambiguous ampersand state

### Improved

- **Parser performance** — `#[inline]` annotations on hot-path tree accessors
  (`node_name`, `attributes`, `attribute`, `NodeId::as_index`/`from_index`),
  direct node field access in `Descendants` and `Children` iterators (avoiding
  method-call indirection), arena pre-sizing from estimated input node count
- Unit tests expanded from 848 to 936
- FFI tests expanded from 112 to 128

## [0.2.0] - 2026-03-05

### Added

- **WHATWG HTML5 parser** — full implementation of the HTML Living Standard
  parsing algorithm (§13.2.5 tokenizer, §13.2.6 tree construction, §13.5
  named character references)
  - 7032/7032 html5lib tokenizer tests passing (100%)
  - 1778/1778 html5lib tree construction tests passing (100%)
  - Fragment parsing (the `innerHTML` algorithm) via `Html5ParseOptions::fragment_context`
  - Scripting flag support (`<noscript>` raw text vs normal parsing)
  - `parse_html5()`, `parse_html5_with_options()`, `parse_html5_full()` API
- **HTML5 serializer** — `serialize_html5()` with WHATWG-compliant output:
  void elements without closing tags, raw text elements without escaping,
  foreign content (`SVG`/`MathML`) self-closing tags
- **HTML5 error reporting** — `parse_html5_full()` returns `Html5ParseResult`
  with all parse errors as `ParseDiagnostic` values with source locations
- **HTML5 FFI bindings** — `xmloxide_parse_html5()`,
  `xmloxide_parse_html5_fragment()`, `xmloxide_serialize_html5()` C functions
- **`xmllint --html5`** — CLI support for HTML5 parsing
- **HTML5 fuzz targets** — `fuzz_html5_parse` and `fuzz_html5_fragment` for
  security testing across 24 different fragment contexts
- **HTML5 benchmarks** — full document and fragment parsing benchmarks
- **html5lib-tests CI** — tokenizer and tree construction conformance suites
  run on every push/PR and weekly

### Improved

- **HTML5 parser performance** — 24% faster than initial implementation via
  bulk text scanning in Data state, fast-path tag name/attribute scanning,
  and character batching in the tree builder (~197µs for a 50-section document)
- Fuzz targets expanded from 4 to 10 (added SAX, reader, push, validation,
  HTML5 parse, HTML5 fragment)
- FFI tests expanded from 112 to 118 (6 new HTML5 tests)
- Unit tests expanded from 785 to 848

## [0.1.1] - 2026-03-02

### Fixed

- Fix docs.rs build failure caused by `all-features = true` pulling in the
  `bench-libxml2` feature, which requires system libxml2 headers unavailable
  in the docs.rs sandbox. Now explicitly lists `cli` and `ffi` features.

### Improved

- Expanded doc comments on `Document` navigation, iteration, and mutation
  methods, `HtmlParseOptions` builder methods, `XmlReader` accessors, and
  `SerializeOptions` builder methods.

## [0.1.0] - 2026-03-01

Initial release of xmloxide — a pure Rust reimplementation of libxml2.

### Added

- **XML 1.0 parser** — hand-rolled recursive descent parser with full W3C XML
  1.0 (Fifth Edition) conformance (1727/1727 applicable tests passing)
- **Error recovery** — parse malformed XML and produce a usable tree, matching
  libxml2's recovery behavior (119/119 libxml2 compatibility tests passing)
- **Arena-based DOM tree** — `Document` with `NodeId` indices for O(1) access,
  cache-friendly layout, and safe bulk deallocation
- **HTML parser** — error-tolerant HTML 4.01 parsing with auto-closing tags,
  implicit elements, and void element handling
- **SAX2 streaming parser** — event-driven API via `SaxHandler` trait
- **XmlReader** — pull-based parsing API
- **Push/incremental parser** — feed chunks of data as they arrive
- **XPath 1.0** — full expression parser and evaluator with all core functions
  and axes, including `namespace::` axis support
- **DTD validation** — parse and validate against Document Type Definitions
- **RelaxNG validation** — parse and validate against RelaxNG schemas
- **XML Schema (XSD) validation** — parse and validate against XML Schema
  definitions
- **Canonical XML** — C14N 1.0 and Exclusive C14N serialization
- **XInclude** — document inclusion processing
- **XML Catalogs** — OASIS XML Catalogs for URI resolution
- **XML serialization** — 1.5-2.4x faster than libxml2
- **HTML serialization** — void elements, attribute rules
- **C/C++ FFI** — full C API with header file (`include/xmloxide.h`) covering
  document parsing, tree navigation and mutation, serialization, XPath, SAX2
  streaming, push parser, XmlReader, validation, C14N, XInclude, and catalogs
- **`xmllint` CLI** — command-line tool for parsing, validating, and querying
  XML/HTML (behind `cli` feature flag)
- **Character encoding** — automatic detection and transcoding via `encoding_rs`
- **Namespace support** — full Namespaces in XML 1.0 implementation
- **String interning** — dictionary-based interning for fast comparisons
- **Fuzz targets** — XML, HTML, XPath, and roundtrip fuzz testing
- **Benchmark suite** — criterion benchmarks for parsing, serialization, SAX,
  XmlReader, XPath, push parsing, and head-to-head comparison with libxml2

### Performance

- Parsing within 3-4% of libxml2 on most documents, 12% faster on SVG
- Serialization is 1.5-2.4x faster than libxml2
- XPath is 1.1-2.7x faster than libxml2 across all benchmarks
- Key optimizations: O(1) character peek, bulk text scanning, ASCII fast paths,
  zero-copy element name splitting, inline entity resolution, XPath `//` step
  fusion with fused axis expansion, inlined tree accessors, and name-test fast
  paths for child/descendant axes

### Testing

- 785 unit tests across all modules
- 112 FFI integration tests covering the full C API surface
- 1727/1727 W3C XML Conformance Test Suite tests (100%)
- 119/119 libxml2 compatibility tests (100%)
- Real-world XML, security/DoS, and entity resolver integration tests

[0.3.1]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.3.1
[0.3.0]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.3.0
[0.2.0]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.2.0
[0.1.1]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.1.1
[0.1.0]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.1.0
