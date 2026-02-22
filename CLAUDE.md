# CLAUDE.md — xmloxide project guide

## Project Overview

**xmloxide** is a pure Rust reimplementation of libxml2 — the de facto standard XML/HTML parsing library in the open-source world. libxml2 became officially unmaintained in December 2025 with known security issues. xmloxide aims to be a memory-safe, high-performance replacement that passes the same conformance test suites.

### Goals

- Full conformance with W3C XML 1.0 (Fifth Edition) and Namespaces in XML 1.0
- Pass the W3C XML Conformance Test Suite (2000+ test files)
- Pass libxml2's own regression suite (`runtest`, `runsuite`, `runxmlconf`, `testapi`)
- Feature parity with libxml2's core: XML/HTML parsing, DOM, SAX2, XPath 1.0, XmlReader, DTD/RelaxNG/XSD validation, C14N, XInclude, XML Catalogs
- Zero `unsafe` in public API surface; minimal `unsafe` internally (arena internals only, if needed)
- No system dependencies — pure Rust (uses `encoding_rs` for character encoding)

### Non-Goals (for now)

- C FFI compatibility layer (Phase 6, later)
- XSLT (that's libxslt, a separate project)
- XML 1.1 support (rarely used, can add later)
- Full libxml2 API-level compatibility (we design an idiomatic Rust API)

---

## Architecture

### Tree Representation (the critical design decision)

libxml2 uses a web of raw C pointers (parent, children, next, prev, doc, ns). We use **arena allocation with typed indices**:

- `Document` owns a `Vec<NodeData>` — all nodes live here
- `NodeId` is a `#[repr(transparent)]` newtype over a `NonZeroU32` index
- Navigation (parent, first_child, next_sibling, prev_sibling) stored as `Option<NodeId>` fields on each `NodeData`
- This gives us O(1) node access, cache-friendly layout, no reference counting overhead, no borrow checker fights, and safe freeing (drop the Document, everything is freed)
- Trade-off: individual node removal requires a free-list or tombstone approach

**Why not `Rc<RefCell<>>`:** Reference cycles between parent/child require weak refs, runtime borrow panics are possible, and per-node allocation is slow.

**Why not raw pointers behind unsafe:** We want to minimize unsafe surface area. Arena indices give us the same performance with compile-time safety.

### Module Map

```
src/
├── lib.rs              # Public API re-exports, crate docs
├── tree/
│   ├── mod.rs          # Document, NodeId, NodeData, tree navigation
│   ├── node.rs         # Node type enum, element/text/comment/PI/etc
│   ├── namespace.rs    # Namespace handling, NsId
│   ├── attribute.rs    # Attribute storage and access
│   └── arena.rs        # Arena allocator internals, free-list
├── parser/
│   ├── mod.rs          # ParseOptions, top-level parse functions
│   ├── xml.rs          # XML 1.0 parser (core state machine)
│   ├── push.rs         # Push/incremental parser wrapper
│   ├── dtd.rs          # DTD parsing (ELEMENT, ATTLIST, ENTITY, NOTATION)
│   └── input.rs        # Parser input stack (entity expansion, includes)
├── html/
│   ├── mod.rs          # HTML parser (error-tolerant)
│   └── entities.rs     # HTML5 named character references
├── sax/
│   └── mod.rs          # SAX2 handler trait and default implementation
├── encoding/
│   └── mod.rs          # Encoding detection, BOM sniffing, encoding_rs bridge
├── io/
│   └── mod.rs          # Input/output abstraction (files, memory, streams)
├── xpath/
│   ├── mod.rs          # XPath public API
│   ├── lexer.rs        # XPath expression tokenizer
│   ├── parser.rs       # XPath expression parser → AST
│   ├── compiler.rs     # AST → compiled expression
│   ├── eval.rs         # Expression evaluator against a node tree
│   └── types.rs        # XPath value types (nodeset, string, number, boolean)
├── serial/
│   ├── mod.rs          # Serialization options and entry points
│   ├── xml.rs          # XML serializer
│   ├── html.rs         # HTML serializer (void elements, attribute rules)
│   └── c14n.rs         # Canonical XML (C14N 1.0 / 1.1)
├── error/
│   └── mod.rs          # Error types, error accumulator, structured diagnostics
└── util/
    ├── mod.rs
    ├── dict.rs          # String interning dictionary (like libxml2's xmlDict)
    ├── uri.rs           # RFC 3986 URI parsing and resolution
    ├── buf.rs           # Growable byte buffer
    └── qname.rs         # QName (prefix:localname) handling
```

### Phased Implementation Plan

| Phase | Scope | Test Target |
|-------|-------|-------------|
| 1 | Core tree + XML parser + serialization | `runtest` XML regression tests |
| 2 | HTML parser, SAX2, XmlReader, push parser | All `runtest` categories |
| 3 | XPath 1.0 | XPath regression tests |
| 4 | DTD + RelaxNG validation | `runsuite` RelaxNG tests |
| 5 | XML Schema, Schematron, C14N, XInclude | Full `runsuite` + `runxmlconf` |
| 6 | `xmllint` CLI, C FFI layer, perf tuning | End-to-end parity |

We are currently in **Phase 1**.

---

## Coding Standards

### Formatting

All code is formatted with `rustfmt`. Configuration is in `rustfmt.toml` at the project root. Run before every commit:

```sh
cargo fmt --all
```

CI will reject unformatted code.

### Linting

All code must pass `clippy` at the **pedantic** level with zero warnings. Configuration is in `Cargo.toml` under `[lints]` and in `clippy.toml`.

```sh
cargo clippy --all-targets --all-features -- -D warnings
```

Key clippy policies:
- `pedantic` is enabled globally — we selectively `allow` specific pedantic lints only with justification
- `unwrap()` and `expect()` are **forbidden** in library code (use `?` or return `Result`)
- `unwrap()` is permitted in tests and benchmarks only
- `unsafe` blocks require a `// SAFETY:` comment explaining the invariant

### Naming Conventions

- Types: `PascalCase` — `NodeId`, `Document`, `ParseOptions`
- Functions/methods: `snake_case` — `parse_str`, `root_element`, `first_child`
- Constants: `SCREAMING_SNAKE_CASE` — `MAX_ENTITY_DEPTH`, `DEFAULT_BUF_SIZE`
- Feature flags: `kebab-case` in Cargo.toml, `snake_case` in `#[cfg(feature = "...")]`
- Module files: `snake_case.rs`
- Private helpers: prefixed with `_` only if needed to avoid dead-code warnings; prefer `pub(crate)` visibility over private-with-underscore
- NodeId / NsId / AttrId: always use the newtype, never raw `usize` or `u32`

### API Design Principles

- **Parse functions return `Result<Document, ParseError>`** — never panic
- **Navigation returns `Option<NodeId>`** — missing children/siblings are None, not errors
- **Iterators for traversal** — `children()`, `ancestors()`, `descendants()`, `attributes()` all return iterators
- **`Document` is the owner** — all tree mutation goes through `&mut Document` methods
- **Builder pattern for options** — `ParseOptions::default().recover(true).no_blanks(true)`
- **Zero-copy where possible** — string interning via `Dict`, return `&str` references into the arena
- **No global state** — unlike libxml2, no `xmlInitParser()` / `xmlCleanupParser()` global init. Each `Document` is self-contained and `Send + Sync`.

### Error Handling

- Define domain-specific error enums, not stringly-typed errors
- Use `thiserror` style derives (but hand-implement for now to avoid the dependency)
- The parser supports **error recovery mode**: collect errors into a `Vec<ParseDiagnostic>` while still producing a (possibly partial) tree — this is critical because it's how most real-world users consume libxml2
- Errors carry **source location** (line, column, byte offset) matching libxml2's error reporting
- Error severity levels: `Warning`, `Error`, `Fatal` (matching libxml2's `xmlErrorLevel`)

### Documentation

- Every public type, trait, method, and function has a doc comment
- Doc comments include a brief one-line summary, then a blank line, then details
- Include `# Examples` sections for key API entry points
- Reference the relevant spec section where applicable (e.g., "See XML 1.0 §2.3 Common Syntactic Constructs")
- Module-level docs (`//!` comments in `mod.rs`) explain the module's role and design rationale

### Testing

- Unit tests go in the same file as the code, in a `#[cfg(test)] mod tests { ... }` block
- Integration tests go in `tests/`
- Test names follow `test_<function>_<scenario>` — e.g., `test_parse_str_empty_document`, `test_node_append_child_to_leaf`
- Use `pretty_assertions` for comparing large strings/structures in tests
- Every parser feature needs a roundtrip test: parse → serialize → parse again → assert tree equality
- Conformance tests (W3C suite) live in `tests/conformance/` and are driven by the test XML catalogs

### Performance

- The string dictionary (`Dict`) is load-bearing — hot-path string comparisons must go through interned `SymbolId` equality, not `str` comparison
- The parser should be zero-copy for attribute values and text content where possible (reference into the input buffer)
- Benchmark critical paths: `benches/parser_bench.rs` compares against known documents
- Profile before optimizing — don't prematurely complicate the architecture

### Git Conventions

- Commit messages: `<module>: <imperative summary>` — e.g., `tree: add NodeId newtype with NonZeroU32`, `parser: implement element start tag parsing`
- One logical change per commit
- Feature branches: `feature/<phase>-<module>` — e.g., `feature/phase1-tree`, `feature/phase1-parser`

---

## Key Specifications & References

These are the specs we implement against. Keep them bookmarked:

- [XML 1.0 (Fifth Edition)](https://www.w3.org/TR/xml/) — the core spec
- [Namespaces in XML 1.0](https://www.w3.org/TR/xml-names/) — namespace processing
- [XPath 1.0](https://www.w3.org/TR/xpath-10/) — XPath query language
- [XML Inclusions (XInclude) 1.0](https://www.w3.org/TR/xinclude/) — document merging
- [Canonical XML 1.0](https://www.w3.org/TR/xml-c14n/) — canonical serialization
- [RelaxNG](https://relaxng.org/spec-20011203.html) — schema language
- [W3C XML Schema](https://www.w3.org/TR/xmlschema-1/) — XSD 1.0
- [HTML 4.01](https://www.w3.org/TR/html401/) — libxml2's HTML parser targets HTML 4, not HTML5
- [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986) — URI syntax
- [W3C XML Conformance Test Suite](https://www.w3.org/XML/Test/) — 2000+ test files
- [libxml2 source](https://gitlab.gnome.org/GNOME/libxml2) — the reference implementation

### libxml2 Source Files → xmloxide Modules

| libxml2 file | Lines (approx) | xmloxide module | Priority |
|---|---|---|---|
| `tree.c` | ~10K | `tree/` | Phase 1 |
| `parser.c` | ~16K | `parser/xml.rs` | Phase 1 |
| `HTMLparser.c` | ~7K | `html/` | Phase 2 |
| `SAX2.c` | ~3K | `sax/` | Phase 2 |
| `encoding.c` | ~3K | `encoding/` | Phase 1 |
| `xmlIO.c` | ~4K | `io/` | Phase 1 |
| `xpath.c` | ~14K | `xpath/` | Phase 3 |
| `xmlsave.c` + `HTMLtree.c` | ~5K | `serial/` | Phase 1 |
| `valid.c` | ~5K | (validation module, TBD) | Phase 4 |
| `relaxng.c` | ~8K | (validation module, TBD) | Phase 4 |
| `xmlschemas.c` + `xmlschemastypes.c` | ~15K | (validation module, TBD) | Phase 5 |
| `c14n.c` | ~2K | `serial/c14n.rs` | Phase 5 |
| `xinclude.c` | ~3K | (TBD) | Phase 5 |
| `dict.c` | ~1K | `util/dict.rs` | Phase 1 |
| `uri.c` | ~2K | `util/uri.rs` | Phase 1 |
| `entities.c` | ~1K | `tree/` (entity storage) | Phase 1 |
| `xmlstring.c`, `buf.c` | ~2K | `util/buf.rs` | Phase 1 |
| `hash.c`, `list.c` | ~1K | Use `std::collections` | Phase 1 |
| `catalog.c` | ~3K | (TBD) | Phase 5 |
| `xmlreader.c` | ~3K | (TBD) | Phase 2 |
| `xmlwriter.c` | ~3K | `serial/` (writer API) | Phase 2 |
| `xmlregexp.c` | ~4K | (TBD, internal to validation) | Phase 4 |
| `schematron.c` | ~2K | (TBD) | Phase 5 |
| `pattern.c` | ~2K | (TBD) | Phase 3 |
| `xpointer.c` | ~2K | (TBD) | Phase 3 |

---

## Build & Test Commands

```sh
# Format everything
cargo fmt --all

# Lint everything (must pass with zero warnings)
cargo clippy --all-targets --all-features -- -D warnings

# Run all tests
cargo test --all-features

# Run tests for a specific module
cargo test --all-features tree::
cargo test --all-features parser::

# Run benchmarks
cargo bench

# Check without building (fast feedback)
cargo check --all-features

# Build docs
cargo doc --all-features --no-deps --open

# Run with a specific feature set (e.g., minimal)
cargo test --no-default-features --features "html,xpath"
```

---

## Common Patterns

### Adding a new node type

1. Add variant to `NodeKind` enum in `tree/node.rs`
2. Add constructor method on `Document` in `tree/mod.rs`
3. Add serialization case in `serial/xml.rs` (and `serial/html.rs` if applicable)
4. Add parsing case in `parser/xml.rs`
5. Add tests for parse → serialize roundtrip

### Adding a new parser feature

1. Reference the exact spec section in a comment
2. Implement in `parser/xml.rs` (or appropriate sub-module)
3. Add unit tests covering valid input, malformed input (error recovery), and edge cases
4. Add a conformance test if the W3C suite covers it
5. Run the full test suite to check for regressions

### Error recovery pattern

```rust
// The parser supports recovering from errors. When `ParseOptions::recover` is
// set, the parser logs the error and attempts to continue. This pattern is
// used throughout:
fn parse_something(&mut self) -> Result<NodeId, ParseError> {
    match self.try_parse_something() {
        Ok(node) => Ok(node),
        Err(e) if self.options.recover => {
            self.push_diagnostic(e.into());
            self.skip_to_recovery_point();
            // Return a best-effort node or skip
            Ok(self.create_error_placeholder())
        }
        Err(e) => Err(e),
    }
}
```

---

## Dependencies Policy

- **Minimize dependencies.** This is a foundational library — every dep is a supply chain risk.
- **Required:** `encoding_rs` (character encoding — replaces iconv/ICU, well-audited Mozilla crate)
- **Dev only:** `criterion` (benchmarks), `pretty_assertions` (test output)
- **Explicitly rejected:** `thiserror` (we hand-implement error types), `serde` (not needed in core — add as optional feature later), `regex` (we implement our own pattern matching for XPath/validation), `nom`/`pest`/`winnow` (we write a hand-rolled recursive descent parser for performance and error recovery, matching libxml2's approach)

The parser is hand-rolled recursive descent (not combinator-based) because:
1. libxml2's parser is recursive descent and we need identical behavior for conformance
2. Error recovery requires fine-grained control over the parse state
3. Push/incremental parsing requires suspendable state, which combinators make awkward
4. Performance — no abstraction overhead
