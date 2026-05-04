# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.3] - 2026-05-04

### Fixed

- **Exclusive C14N: emit `xmlns=""` default-namespace undeclaration** when an
  element with no default namespace is canonicalized inside scope of an
  inherited default namespace (#16, thanks @williamareynolds). Previously the
  undeclaration was only emitted in inclusive C14N. Per Canonical XML 1.0 §2.3
  (inherited by Exclusive C14N §3), the empty default namespace must be made
  explicit so the canonical form correctly reflects the element's namespace
  context.

### CI / Maintenance

- Bump `actions/checkout` 4.3.1 → 6.0.2 across all workflows (#14)
- Bump `actions/download-artifact` v4 → v8.0.1 in release workflow (#13)
- Bump `actions/upload-artifact` v4 → v7.0.1 in release workflow (#15)
- Bump `softprops/action-gh-release` v2 → v3.0.0 in release workflow (#12)
- Bump `taiki-e/install-action` 2.75.19 → 2.75.26 in release workflow (#17)
- Remove `bench.yml` workflow — the regression check required a `gh-pages`
  branch that was never created, so it had been failing on every PR. Run
  benchmarks locally with `cargo bench`.

## [0.4.1] - 2026-03-15

### Added

- **Schematron C FFI** — `xmloxide_parse_schematron`, `xmloxide_free_schematron`,
  `xmloxide_validate_schematron`, and `xmloxide_validate_schematron_with_phase` for
  C/C++ consumers; Schematron was previously only available in Rust, Python, and WASM
- **CSS selector C FFI** — `xmloxide_css_select`, `xmloxide_css_select_first`, and
  `xmloxide_free_nodeid_array` for querying elements with CSS selectors from C/C++
- **CSS selectors in Python** — `css_select()` and `css_select_first()` methods on
  `Document` in pyxmloxide
- **WASM tree mutation APIs** — `createElement`, `createText`, `createComment`,
  `appendChild`, `removeNode`, `setAttribute`, `removeAttribute`, `setTextContent`,
  `insertBefore`, `cloneNode` on `WasmDocument`
- **Validation benchmarks** — criterion benchmarks for DTD, RelaxNG, XSD, and
  Schematron validation
- **Expanded XPath benchmarks** — count, string function, position predicate,
  ancestor axis, and union expression benchmarks
- **CSS selector benchmarks** — class selector and complex combinator benchmarks
- 58 CSS evaluator inline tests covering tag, class, ID, attribute, pseudo-class,
  combinator, and universal selector matching
- 10 new FFI tests (5 Schematron + 5 CSS) bringing FFI test total to 138

### Fixed

- **README incorrectly listed Schematron as unsupported** — the Limitations section
  claimed "No Schematron" despite Schematron being added in 0.4.0
- **README listed XPath as "1.0 only"** — updated to "XPath 1.0+" reflecting the
  17+ XPath 2.0 functions added in prior releases
- **Outdated test counts in README** — updated from 936 to 1078 unit tests

### Improved

- Unit tests expanded from 1010 to 1078
- FFI tests expanded from 128 to 138
- README now documents serde, async, and Schematron features
- MIGRATION.md expanded with HTML5 parsing, HTML5 streaming, Schematron validation,
  and CSS selector migration examples
- CLAUDE.md module map updated with css/, serde_xml/, async_xml, and full ffi/ listing
- `xmllint --schematron` added to CLI documentation in README
- Schematron added to migration table in README
- `xmloxide.h` header updated with Schematron and CSS selector declarations

## [0.4.0] - 2026-03-14

### Added

- **ISO Schematron validation** (`validation::schematron` module) — rule-based XML
  validation per ISO/IEC 19757-3, complementing DTD, RelaxNG, and XSD
  - `parse_schematron()` / `validate_schematron()` / `validate_schematron_with_phase()` API
  - Assert/report checks with `XPath`-driven test expressions
  - Firing rule semantics (first matching rule wins per pattern)
  - Three-level `<sch:let>` variables (schema, pattern, rule scope)
  - Message interpolation via `<sch:value-of select="..."/>`
  - Phase-based selective validation (`<sch:phase>` / `<sch:active>`)
  - Dual namespace support: ISO (`http://purl.oclc.org/dml/schematron`) and
    classic 1.5 (`http://www.ascc.net/xml/schematron`), plus `sch:` prefix
  - 31 unit tests + 11 integration tests with realistic purchase order schema
- **`xmllint --schematron`** — CLI validation against Schematron schemas, following
  the existing `--relaxng` and `--schema` patterns
- **XPath `matches()` function** — regex matching for Schematron pattern validation,
  with a hand-rolled engine (no `regex` crate dependency) supporting character classes,
  quantifiers, shorthand (`\d`, `\s`, `\w`), alternation, grouping, counted
  quantifiers `{n,m}`, and flags (`i`, `s`)
- **XPath namespace-aware name matching** — `XPathContext::set_namespace()` registers
  prefix→URI bindings so that prefixed name tests like `//inv:invoice` resolve via
  namespace URI comparison instead of string matching; Schematron `<sch:ns>` bindings
  are automatically threaded through
- **XSD `elementFormDefault` support** — when set to `"qualified"`, child elements
  in instance documents must carry the schema's target namespace; fixes namespace
  validation for UBL 2.4 and similar schemas
- **WASM validation APIs** — `validateRelaxng()`, `validateXsd()`,
  `validateSchematron()` on `WasmDocument`, returning `WasmValidationResult`
  with `isValid`, `errors`, and `warnings`
- **Python validation APIs** — `validate_relaxng()`, `validate_xsd()`,
  `validate_schematron()` on `Document`, returning `ValidationResult` with
  `is_valid`, `errors`, `warnings`, and `__bool__()` support
- `fuzz_schematron` fuzz target for schema parsing and validation (11 total)

### Fixed

- **XPath attribute path returning String instead of NodeSet** — multi-step paths
  ending with an attribute axis (e.g., `item/@amount`) now correctly return a
  `NodeSet`, fixing `sum()`, `count()`, and comparison operations on attribute
  collections
- **XPath `prefix:*` tokenization** — the lexer now correctly tokenizes namespace
  wildcard expressions like `inv:*` as a single token instead of failing with a
  parse error
- **Schematron message interpolation for NodeSets** — `<sch:value-of>` expressions
  that return element NodeSets now correctly compute string values using the document
  context instead of returning empty strings

### Improved

- Unit tests expanded from 988 to 1010
- Fuzz targets expanded from 10 to 11

## [0.3.3] - 2026-03-13

### Added

- **`xsd:import` and `xsd:include` support** ([#3](https://github.com/jonwiggins/xmloxide/issues/3)) —
  multi-file XSD schema composition for real-world schemas like UBL 2.4
  - `SchemaResolver` trait for pluggable schema loading (filesystem, HTTP, embedded, etc.)
  - `parse_xsd_with_options()` — new entry point that follows `xsd:include` (same-namespace
    merging, chameleon includes) and `xsd:import` (cross-namespace type resolution)
  - `XsdParseOptions` struct with optional resolver and base URI
  - Cycle detection prevents infinite loops in circular import/include chains
  - Namespace-aware type resolution via `QName` prefix maps — imported types like
    `tns:AddressType` resolve correctly through `imported_namespaces`
  - Existing `parse_xsd()` unchanged (backward compatible, silently ignores import/include)
- **`xsd:element ref` support** — element references (`ref="cbc:ID"`) resolve to global
  element declarations in local or imported schemas, enabling real-world UBL 2.4 validation
- 26 new tests (24 unit + 2 integration) covering include merging, chameleon includes,
  import cross-namespace resolution, cycle detection, transitive includes, namespace
  mismatch errors, element refs, and UBL-like multi-schema validation patterns
- UBL 2.4 `BusinessCard` schema integration test: parses 15-file schema graph and
  validates the official OASIS example document with zero errors

## [0.3.1] - 2026-03-06

### Fixed

- Pin `tempfile` dev-dependency to `<3.20` and `proptest` to `<1.7` to avoid
  transitive dependencies requiring Rust 1.84+/1.85+, breaking the MSRV of 1.81

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

[0.4.0]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.4.0
[0.3.3]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.3.3
[0.3.1]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.3.1
[0.3.0]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.3.0
[0.2.0]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.2.0
[0.1.1]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.1.1
[0.1.0]: https://github.com/jonwiggins/xmloxide/releases/tag/v0.1.0
