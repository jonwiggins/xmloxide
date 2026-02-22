# Plan: Parser Consolidation, Security Hardening, and Conformance Prep

## Context

We have 4 parser implementations (parser/xml.rs, sax/mod.rs, reader/mod.rs, html/mod.rs)
with ~1,200 lines of duplicated logic across them. We also have zero security limits
(nesting depth, entity expansion, input size). This plan addresses:

1. **Parser consolidation** — extract shared parsing core
2. **Security hardening** — add limits and fuzzing
3. **Entity expansion limits** — defense-in-depth guards
4. **Conformance test plan** — W3C XML Test Suite integration

---

## Task 1: Extract `ParserInput` Shared Core

**Goal:** Eliminate ~1,200 lines of duplicated low-level parsing code.

### 1a. Create `src/parser/input.rs` — `ParserInput` struct

Extract these 14+ methods from all parsers into a single `ParserInput` struct:

```rust
pub(crate) struct ParserInput<'a> {
    input: &'a [u8],
    pos: usize,
    line: u32,
    column: u32,
    depth: u32,          // NEW: nesting depth tracking
    max_depth: u32,      // NEW: configurable limit
}
```

Methods to extract (identical across all parsers):
- `peek()`, `peek_at()`, `peek_char()` — lookahead
- `advance()`, `advance_char()` — consume with line/column tracking
- `next_byte()`, `next_char()` — consume and return
- `expect_byte()`, `expect_str()` — assert-and-consume
- `looking_at()` — lookahead without consuming
- `skip_whitespace()`, `skip_whitespace_required()` — whitespace
- `take_while()` — consume while predicate
- `at_end()`, `location()` — state queries

Also extract into `ParserInput`:
- `parse_name()` — XML name parsing (~30 lines)
- `parse_reference()` — entity/char reference resolution (~50 lines)
- `parse_attribute_value()` — quoted value parsing (~35 lines)
- `parse_quoted_value()` — simple quoted string
- `is_name_start_char()`, `is_name_char()` — XML name character classes
- `split_name()` — prefix:local splitting

And the error helpers:
- `fatal()` → returns `ParseError` with current location
- `push_diagnostic()` → appends to diagnostics vec
  (ParserInput holds `diagnostics: Vec<ParseDiagnostic>` directly)

### 1b. Create `src/parser/common.rs` — Shared XML Parsing Methods

Extract higher-level parsing methods that are identical across XML/SAX/Reader
(but differ from HTML) into free functions or methods that take `&mut ParserInput`:

- `parse_comment(input) -> Result<String, ParseError>` — returns comment text
- `parse_cdata(input) -> Result<String, ParseError>` — returns CDATA content
- `parse_processing_instruction(input) -> Result<(String, Option<String>), ParseError>`
  — returns (target, data)
- `parse_xml_declaration(input) -> Result<XmlDeclaration, ParseError>`
  — returns version/encoding/standalone

These return plain data, and each consumer (tree/SAX/reader) wraps the result
into its own output format.

### 1c. Refactor each parser to use `ParserInput`

- **parser/xml.rs**: `XmlParser` holds `ParserInput` + `Document` + `ParseOptions` + `ns_stack`.
  All low-level methods removed, delegated to `self.input.*`.
- **sax/mod.rs**: `SaxParser` holds `ParserInput` + `SaxHandler` + `ParseOptions` + `ns_stack`.
  Same delegation.
- **reader/mod.rs**: `XmlReader` holds `ParserInput` + state fields.
  Same delegation.
- **html/mod.rs**: `HtmlParser` holds `ParserInput` + `Document` + `HtmlParseOptions`.
  Uses `ParserInput` for low-level ops but keeps its own entity handling
  (HTML entities differ from XML).

### 1d. Add namespace resolution to `ParserInput`

The ns_stack logic is also duplicated across XML/SAX/Reader. Extract:
- `ns_stack: Vec<Vec<(Option<String>, String)>>`
- `push_ns_scope()`, `pop_ns_scope()`, `bind_namespace()`, `resolve_namespace()`

This could live in `ParserInput` directly or in a separate `NamespaceResolver`
struct composed into `ParserInput`.

### Estimated impact
- **Lines removed**: ~1,000-1,200 across 4 files
- **Lines added**: ~350-400 in input.rs + common.rs
- **Net reduction**: ~600-800 lines
- **Risk**: Medium — requires careful testing to ensure no behavioral changes

---

## Task 2: Add Security Limits

**Goal:** Prevent denial-of-service via deeply nested elements, huge attributes, etc.

### 2a. Add limits to `ParseOptions`

```rust
pub struct ParseOptions {
    pub recover: bool,
    pub no_blanks: bool,
    // NEW security limits:
    pub max_depth: u32,               // default: 256
    pub max_attributes: u32,          // default: 256
    pub max_attribute_length: usize,  // default: 10 * 1024 * 1024 (10MB)
    pub max_text_length: usize,       // default: 10 * 1024 * 1024 (10MB)
    pub max_name_length: usize,       // default: 50_000
    pub max_entity_expansions: u32,   // default: 10_000 (defense-in-depth)
}
```

Add builder methods: `.max_depth(n)`, `.max_attributes(n)`, etc.

### 2b. Enforce limits in `ParserInput`

- **Depth tracking**: `ParserInput` increments `depth` when entering an element,
  decrements when leaving. Error if `depth > max_depth`.
- **Name length**: Check in `parse_name()`.
- **Attribute count**: Check when parsing element start tags.
- **Text/attribute value length**: Check in `parse_attribute_value()` and
  character data accumulation.
- **Entity expansion count**: Track in `parse_reference()`, error if exceeded.

### 2c. Add limits to HTML parser

The HTML parser should also respect depth limits. Wire the same checks through
`HtmlParseOptions`.

### 2d. Security-focused tests

Add tests in `tests/security.rs`:
- `test_deeply_nested_elements_rejected` — 10,000 nested `<a>` elements
- `test_billion_laughs_rejected` — entity expansion attack (once entities are supported)
- `test_huge_attribute_value_rejected` — 100MB attribute value
- `test_many_attributes_rejected` — 10,000 attributes on one element
- `test_huge_name_rejected` — 1MB element name
- `test_limits_configurable` — custom limits via `ParseOptions`
- `test_default_limits_permissive_enough` — normal documents parse fine

---

## Task 3: Entity Expansion Hardening

**Goal:** Defense-in-depth even though we currently only support built-in entities.

### 3a. Add entity expansion counter to `ParserInput`

```rust
entity_expansions: u32,
max_entity_expansions: u32,
```

Every call to `parse_reference()` increments the counter. If it exceeds the
limit, return an error. This protects against future entity support and
malicious documents with thousands of `&amp;` references.

### 3b. Document the security model

Add a `// SECURITY:` comment block at the top of `parser/input.rs` explaining:
- Only built-in XML entities (amp, lt, gt, apos, quot) are expanded
- No external entity loading (immune to XXE)
- No recursive entity expansion (immune to billion laughs)
- Expansion counter is defense-in-depth for future DTD entity support
- Depth limit prevents stack overflow from deeply nested elements

### 3c. Future-proof for DTD entity support

When DTD internal subset entity declarations (`<!ENTITY name "value">`) are
eventually supported in the parser (currently only in validation/dtd.rs), the
expansion counter will be critical. Leave clear TODO markers for this.

---

## Task 4: Plan for W3C XML Conformance Test Suite

**Goal:** Prepare infrastructure for running the 2000+ W3C test files.

### 4a. Download and integrate the test suite

- Download from https://www.w3.org/XML/Test/
- Place in `tests/conformance/xmlconf/` (gitignored, downloaded by script)
- Create `scripts/download-conformance-suite.sh`

### 4b. Create conformance test harness

Create `tests/conformance/mod.rs` (or a separate binary) that:
1. Reads the test catalog XML file (xmlconf.xml)
2. For each test case, extracts: ID, type (valid/invalid/not-wf/error), URI, description
3. Runs the xmloxide parser against the test input
4. Checks the result matches expected (valid→succeeds, not-wf→fails, etc.)
5. Produces a report: pass/fail/skip counts

### 4c. Track conformance progress

Create a simple reporting mechanism:
- Total tests, passing, failing, skipped
- Breakdown by category (valid, invalid, not-well-formed, error)
- List of failing test IDs for targeted debugging

### 4d. CI integration

Add a CI job that:
1. Downloads the conformance suite
2. Runs the harness
3. Compares results against a known-good baseline
4. Fails if any previously passing test now fails (regression)

---

## Execution Order

1. **Task 1a-1b** (ParserInput + common.rs) — foundation, no behavior change
2. **Task 1c** (refactor parsers) — one parser at a time, tests after each
3. **Task 2a-2c** (security limits) — builds on ParserInput infrastructure
4. **Task 2d + Task 3** (security tests + entity hardening) — validation
5. **Task 1d** (namespace consolidation) — lower priority, can be parallel
6. **Task 4** (conformance planning) — can start anytime, independent

After each step: `cargo fmt`, `cargo clippy`, `cargo test` — all must pass.
